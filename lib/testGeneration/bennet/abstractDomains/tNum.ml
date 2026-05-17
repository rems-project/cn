module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms

(** Tristate number (tnum) abstract domain.

    A tnum (v, m) represents a set of integers where:
    - v (value): bits known to be 1
    - m (mask): bits that are unknown (0 = known, 1 = unknown)
    - Well-formedness invariant: v & m = 0

    Concretization: γ(v, m) = { c | c & ~m = v }

    This domain provides precise bit-level tracking, particularly useful for
    bitwise operations, alignment checks, and flag manipulation. *)

module TristateBasis = struct
  let name = "tristate"

  let c_name = "tnum"

  type t =
    { bt : BT.t;
      is_bottom : bool;
      value : Z.t; (* Known 1 bits *)
      mask : Z.t (* Unknown bits: 0 = known, 1 = unknown *)
    }

  let supported (bt : BT.t) = match bt with Bits _ | Loc _ -> true | _ -> false

  let bt { bt; _ } = bt

  let bottom bt = { bt; is_bottom = true; value = Z.zero; mask = Z.zero }

  let is_bottom { is_bottom; _ } = is_bottom

  let get_bits_bt bt =
    match bt with
    | BT.Loc () -> Memory.uintptr_bt
    | Bits _ -> bt
    | _ -> failwith ("invalid type: " ^ Pp.plain (BT.pp bt) ^ " @ " ^ __LOC__)


  let get_width bt =
    let bt = match bt with BT.Loc () -> Memory.uintptr_bt | _ -> bt in
    match BT.is_bits_bt bt with
    | Some (_, w) -> w
    | None -> failwith ("not a bits type: " ^ Pp.plain (BT.pp bt))


  let get_extrema bt =
    let bt =
      match bt with
      | BT.Bits _ -> bt
      | Loc () -> Memory.uintptr_bt
      | _ -> failwith ("invalid type: " ^ Pp.plain (BT.pp bt) ^ " @ " ^ __LOC__)
    in
    BT.bits_range (Option.get (BT.is_bits_bt bt))


  (* Convert an unsigned stored value back to signed representation.
     The tnum stores values masked to bit width (unsigned form).
     For signed types, values >= 2^(width-1) need to be converted back to negative. *)
  let to_signed_value bt v =
    match BT.is_bits_bt bt with
    | Some (BT.Signed, width) ->
      let sign_bit = Z.shift_left Z.one (width - 1) in
      if Z.geq v sign_bit then
        Z.sub v (Z.shift_left Z.one width)
      else
        v
    | _ -> v


  (* Create a mask with all bits set for the width *)
  let full_mask bt =
    let width = get_width bt in
    Z.sub (Z.shift_left Z.one width) Z.one


  (* Normalize value and mask to bit width *)
  let normalize bt value mask =
    let fm = full_mask bt in
    let v = Z.logand value fm in
    let m = Z.logand mask fm in
    (* Ensure well-formedness: v & m = 0 *)
    let v = Z.logand v (Z.lognot m) in
    (v, m)


  (* Top: all bits unknown (mask = all 1s, value = 0) *)
  let top bt =
    let fm = full_mask bt in
    { bt; is_bottom = false; value = Z.zero; mask = fm }


  let is_top { bt; is_bottom; value; mask; _ } =
    if is_bottom then
      false
    else (
      let fm = full_mask bt in
      Z.equal value Z.zero && Z.equal mask fm)


  (* Create a constant tnum (all bits known) *)
  let of_const bt n =
    let fm = full_mask bt in
    let value = Z.logand n fm in
    { bt; is_bottom = false; value; mask = Z.zero }


  (* Create a tnum from value and mask *)
  let of_tnum bt value mask =
    let v, m = normalize bt value mask in
    { bt; is_bottom = false; value = v; mask = m }


  (* of_interval creates a tnum that represents all values in [start, stop] *)
  let of_interval bt start stop =
    if Z.gt start stop then
      bottom bt
    else if Z.equal start stop then
      of_const bt start
    else (
      (* For a range [start, stop], determine which bits are constant *)
      let fm = full_mask bt in
      let start = Z.logand start fm in
      let stop = Z.logand stop fm in
      (* XOR gives us bits that differ between start and stop *)
      let diff = Z.logxor start stop in
      (* Find the highest differing bit and make all bits from there down unknown *)
      let rec find_highest_bit n pos =
        if Z.equal n Z.zero then
          pos
        else
          find_highest_bit (Z.shift_right n 1) (pos + 1)
      in
      let highest = find_highest_bit diff 0 in
      (* Mask from highest differing bit down *)
      let uncertain_mask = Z.sub (Z.shift_left Z.one highest) Z.one in
      (* Value is the common prefix (bits above the highest differing bit) *)
      let value = Z.logand start (Z.lognot uncertain_mask) in
      of_tnum bt value uncertain_mask)


  let equal t1 t2 =
    if is_bottom t1 && is_bottom t2 then
      true
    else if is_bottom t1 || is_bottom t2 then
      false
    else
      BT.equal t1.bt t2.bt && Z.equal t1.value t2.value && Z.equal t1.mask t2.mask


  let compare t1 t2 =
    if equal t1 t2 then
      0
    else if is_bottom t1 then
      -1
    else if is_bottom t2 then
      1
    else (
      let c = BT.compare t1.bt t2.bt in
      if c <> 0 then
        c
      else (
        let c = Z.compare t1.value t2.value in
        if c <> 0 then c else Z.compare t1.mask t2.mask))


  (* Partial order: t1 ⊑ t2 iff γ(t1) ⊆ γ(t2)
     This holds when t1 has more known bits that agree with t2's constraints *)
  let leq t1 t2 =
    if is_bottom t1 then
      true
    else if is_bottom t2 then
      false
    else if is_top t2 then
      true
    else if is_top t1 then
      false
    else (
      assert (BT.equal t1.bt t2.bt);
      (* t1 ⊑ t2 iff:
         1. All known bits in t2 are also known in t1 with same value
         2. t1's mask is a subset of t2's mask *)
      let known_in_t2 = Z.lognot t2.mask in
      let known_in_t1 = Z.lognot t1.mask in
      (* t1 must know at least all bits t2 knows *)
      let knows_enough = Z.equal (Z.logand known_in_t2 known_in_t1) known_in_t2 in
      (* For known bits in t2, values must match *)
      let values_match = Z.equal (Z.logand t1.value known_in_t2) t2.value in
      knows_enough && values_match)


  (* Join (least upper bound): union of representable values
     Result has unknown bits wherever either operand is unknown or they differ *)
  let join t1 t2 =
    if is_bottom t1 then
      t2
    else if is_bottom t2 then
      t1
    else (
      assert (BT.equal t1.bt t2.bt);
      let fm = full_mask t1.bt in
      (* Bits that are known in both *)
      let known_both = Z.logand (Z.lognot t1.mask) (Z.lognot t2.mask) in
      (* Bits that differ in value where both are known *)
      let diff = Z.logxor t1.value t2.value in
      let conflict = Z.logand known_both diff in
      (* New mask: unknown in either operand, or conflicting values *)
      let mask = Z.logand fm (Z.logor (Z.logor t1.mask t2.mask) conflict) in
      (* New value: keep only bits that are known and agree *)
      let value = Z.logand (Z.logand t1.value t2.value) (Z.lognot mask) in
      of_tnum t1.bt value mask)


  (* Meet (greatest lower bound): intersection of representable values *)
  let meet t1 t2 =
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      assert (BT.equal t1.bt t2.bt);
      (* Check for conflict: both known but different values *)
      let known_in_t1 = Z.lognot t1.mask in
      let known_in_t2 = Z.lognot t2.mask in
      let conflict =
        Z.logand (Z.logand known_in_t1 known_in_t2) (Z.logxor t1.value t2.value)
      in
      if not (Z.equal conflict Z.zero) then
        bottom t1.bt
      else (
        (* New value: combine known bits from both *)
        let value = Z.logor t1.value t2.value in
        (* New mask: unknown only where both are unknown *)
        let mask = Z.logand t1.mask t2.mask in
        of_tnum t1.bt value mask))


  let join_many tnums =
    match tnums with
    | [] -> failwith "join_many requires a non-empty list"
    | h :: t -> List.fold_left join h t


  let meet_many tnums =
    match tnums with
    | [] -> failwith "meet_many requires a non-empty list"
    | h :: t -> List.fold_left meet h t


  let is_meet_assoc = true

  let is_join_assoc = true

  (* Forward bitwise AND (optimal)
     For each output bit:
     - If either input is known 0 -> output is known 0
     - If both inputs are known 1 -> output is known 1
     - Otherwise -> output is unknown *)
  let tnum_and t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let fm = full_mask t1.bt in
      (* Output value: bits that are known 1 in both inputs *)
      let value = Z.logand t1.value t2.value in
      (* For AND, output is known when:
         - Either input is known 0 (output is 0), OR
         - Both inputs are known 1 (output is 1) *)
      (* Known 0 bits in t1: ~value & ~mask, masked to bit width *)
      let k0_t1 = Z.logand fm (Z.logand (Z.lognot t1.value) (Z.lognot t1.mask)) in
      let k0_t2 = Z.logand fm (Z.logand (Z.lognot t2.value) (Z.lognot t2.mask)) in
      (* Output bits that are known 0 *)
      let out_k0 = Z.logor k0_t1 k0_t2 in
      (* Output bits that are known 1 *)
      let out_k1 = value in
      (* Known bits = k0 | k1 *)
      let known = Z.logor out_k0 out_k1 in
      (* Mask = unknown bits *)
      let mask = Z.logand fm (Z.lognot known) in
      of_tnum t1.bt value mask)


  (* Forward bitwise OR (optimal)
     For each output bit:
     - If either input is known 1 -> output is known 1
     - If both inputs are known 0 -> output is known 0
     - Otherwise -> output is unknown *)
  let tnum_or t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let fm = full_mask t1.bt in
      (* Output value: bits that are known 1 in either input *)
      let value = Z.logor t1.value t2.value in
      (* For OR, output is known when:
         - Either input is known 1 (output is 1), OR
         - Both inputs are known 0 (output is 0) *)
      (* Known 0 bits in each operand *)
      let k0_t1 = Z.logand fm (Z.logand (Z.lognot t1.value) (Z.lognot t1.mask)) in
      let k0_t2 = Z.logand fm (Z.logand (Z.lognot t2.value) (Z.lognot t2.mask)) in
      (* Output bits that are known 0: both inputs must be known 0 *)
      let out_k0 = Z.logand k0_t1 k0_t2 in
      (* Output bits that are known 1 *)
      let out_k1 = value in
      (* Known bits = k0 | k1 *)
      let known = Z.logor out_k0 out_k1 in
      (* Mask = unknown bits *)
      let mask = Z.logand fm (Z.lognot known) in
      of_tnum t1.bt value mask)


  (* Forward bitwise XOR
     For each output bit:
     - If both inputs are known -> output is known (xor of values)
     - Otherwise -> output is unknown *)
  let tnum_xor t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      (* Output is known only where both inputs are known *)
      let value = Z.logxor t1.value t2.value in
      let mask = Z.logor t1.mask t2.mask in
      of_tnum t1.bt value mask)


  (* Forward bitwise NOT *)
  let tnum_not t =
    if is_bottom t then
      bottom t.bt
    else (
      let fm = full_mask t.bt in
      (* For NOT: flip all known bits, mask stays the same *)
      (* Known 0 bits become known 1, known 1 bits become known 0 *)
      (* Known 0 in input = ~value & ~mask *)
      let k0 = Z.logand fm (Z.logand (Z.lognot t.value) (Z.lognot t.mask)) in
      (* After NOT, these become known 1 *)
      let value = k0 in
      of_tnum t.bt value t.mask)


  (* Forward addition (Linux kernel algorithm)
     This is the optimal tnum addition algorithm from the Linux kernel verifier *)
  let tnum_add t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let fm = full_mask t1.bt in
      (* Sum of known values *)
      let sv = Z.logand (Z.add t1.value t2.value) fm in
      (* Sum of masks *)
      let sm = Z.logand (Z.add t1.mask t2.mask) fm in
      (* sigma = sv + sm (propagates carry through unknown bits) *)
      let sigma = Z.logand (Z.add sv sm) fm in
      (* chi = sigma XOR sv (identifies changed bits) *)
      let chi = Z.logxor sigma sv in
      (* Output mask: any bit that could change is unknown *)
      let mask = Z.logor (Z.logor chi t1.mask) t2.mask in
      let mask = Z.logand mask fm in
      (* Output value: known bits that don't change *)
      let value = Z.logand sv (Z.lognot mask) in
      of_tnum t1.bt value mask)


  (* Forward subtraction *)
  let tnum_sub t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let fm = full_mask t1.bt in
      (* For subtraction, we compute a - b = a + (~b + 1) *)
      (* Negate t2: flip bits and add 1 *)
      let neg_value = Z.logand (Z.add (Z.lognot t2.value) Z.one) fm in
      let neg_t2 = { t2 with value = Z.logand neg_value (Z.lognot t2.mask) } in
      tnum_add t1 neg_t2)


  (* Forward left shift *)
  let tnum_shl t shift_amt =
    if is_bottom t then
      bottom t.bt
    else if is_bottom shift_amt then
      bottom t.bt
    else if not (Z.equal shift_amt.mask Z.zero) then (* Non-constant shift: return top *)
      top t.bt
    else (
      let fm = full_mask t.bt in
      let width = get_width t.bt in
      let shift = Z.to_int shift_amt.value in
      if shift < 0 || shift >= width then
        top t.bt
      else (
        let value = Z.logand (Z.shift_left t.value shift) fm in
        let mask = Z.logand (Z.shift_left t.mask shift) fm in
        of_tnum t.bt value mask))


  (* Forward right shift (logical) *)
  let tnum_lshr t shift_amt =
    if is_bottom t then
      bottom t.bt
    else if is_bottom shift_amt then
      bottom t.bt
    else if not (Z.equal shift_amt.mask Z.zero) then (* Non-constant shift: return top *)
      top t.bt
    else (
      let width = get_width t.bt in
      let shift = Z.to_int shift_amt.value in
      if shift < 0 || shift >= width then
        top t.bt
      else (
        let value = Z.shift_right t.value shift in
        let mask = Z.shift_right t.mask shift in
        of_tnum t.bt value mask))


  (* Forward multiplication - conservative approximation *)
  let tnum_mul t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t1.mask Z.zero && Z.equal t2.mask Z.zero then (
      (* Both constants *)
      let fm = full_mask t1.bt in
      of_const t1.bt (Z.logand (Z.mul t1.value t2.value) fm))
    else if Z.equal t1.mask Z.zero && Z.equal t1.value Z.zero then (* t1 is 0 *)
      of_const t1.bt Z.zero
    else if Z.equal t2.mask Z.zero && Z.equal t2.value Z.zero then (* t2 is 0 *)
      of_const t1.bt Z.zero
    else (* Conservative: return top *)
      top t1.bt


  (* Forward division - conservative approximation *)
  let tnum_div t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t2.mask Z.zero && Z.equal t2.value Z.zero then (* Division by zero *)
      bottom t1.bt
    else if Z.equal t1.mask Z.zero && Z.equal t2.mask Z.zero then (* Both constants *)
      of_const t1.bt (Z.div t1.value t2.value)
    else (* Conservative: return top *)
      top t1.bt


  (* Forward modulo - conservative approximation *)
  let tnum_mod t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t2.mask Z.zero && Z.equal t2.value Z.zero then (* Modulo by zero *)
      bottom t1.bt
    else if Z.equal t1.mask Z.zero && Z.equal t2.mask Z.zero then (* Both constants *)
      of_const t1.bt (Z.rem t1.value t2.value)
    else (* Conservative: return top *)
      top t1.bt


  let forward_abs_binop (op : IT.binop) (t1 : t) (t2 : t) : t option =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      Some (bottom t1.bt)
    else (
      match op with
      | Add -> Some (tnum_add t1 t2)
      | Sub -> Some (tnum_sub t1 t2)
      | Mul -> Some (tnum_mul t1 t2)
      | Div | DivNoSMT -> Some (tnum_div t1 t2)
      | Mod -> Some (tnum_mod t1 t2)
      | BW_And -> Some (tnum_and t1 t2)
      | BW_Or -> Some (tnum_or t1 t2)
      | BW_Xor -> Some (tnum_xor t1 t2)
      | ShiftLeft -> Some (tnum_shl t1 t2)
      | ShiftRight -> Some (tnum_lshr t1 t2)
      | _ -> None)


  let forward_abs_unop (op : IT.unop) (t : t) : t option =
    match op with
    | BW_Compl -> Some (tnum_not t)
    | Negate ->
      if is_bottom t then
        Some (bottom t.bt)
      else if Z.equal t.mask Z.zero then (
        let fm = full_mask t.bt in
        Some (of_const t.bt (Z.logand (Z.neg t.value) fm)))
      else
        Some (top t.bt)
    | _ -> None


  let pp { bt = _; is_bottom; value; mask } =
    let open Pp in
    if is_bottom then
      !^"⊥"
    else if Z.equal mask Z.zero then
      z value
    else
      !^"tnum" ^^ parens (z value ^^ comma ^^ space ^^ z mask)


  let cast (t : t) (target_bt : BT.t) : t =
    if is_bottom t then
      bottom target_bt
    else if not (supported target_bt) then
      top target_bt
    else (
      let src_width = get_width t.bt in
      let dst_width = get_width target_bt in
      if src_width = dst_width then
        { bt = target_bt; is_bottom = false; value = t.value; mask = t.mask }
      else if src_width > dst_width then (
        (* Truncation: keep lower dst_width bits *)
        let dst_fm = full_mask target_bt in
        let value = Z.logand t.value dst_fm in
        let mask = Z.logand t.mask dst_fm in
        of_tnum target_bt value mask)
      else (
        (* Extension: upper bits become unknown for unsigned,
           or sign-extended for signed *)
        let is_src_signed =
          match BT.is_bits_bt t.bt with Some (Signed, _) -> true | _ -> false
        in
        if is_src_signed then (
          (* Sign extension: if sign bit is unknown, upper bits are unknown *)
          let sign_bit = Z.shift_left Z.one (src_width - 1) in
          let sign_known = Z.equal (Z.logand t.mask sign_bit) Z.zero in
          if sign_known then (
            let sign_is_one = not (Z.equal (Z.logand t.value sign_bit) Z.zero) in
            if sign_is_one then (
              (* Extend with 1s *)
              let dst_fm = full_mask target_bt in
              let extension = Z.logand (Z.lognot (full_mask t.bt)) dst_fm in
              let value = Z.logor t.value extension in
              of_tnum target_bt value t.mask)
            else (* Extend with 0s *)
              of_tnum target_bt t.value t.mask)
          else (
            (* Sign bit unknown: upper bits are unknown *)
            let dst_fm = full_mask target_bt in
            let extension_mask = Z.logand (Z.lognot (full_mask t.bt)) dst_fm in
            let mask = Z.logor t.mask extension_mask in
            of_tnum target_bt t.value mask))
        else (* Zero extension: upper bits are 0 *)
          of_tnum target_bt t.value t.mask))


  let forward_abs_it (it : IT.t) (t_args : t list) : t option =
    let (IT (it_, bt, _loc)) = it in
    match it_ with
    | Const (Bits (_, n)) -> Some (of_const bt n)
    | Binop (op, _, _) ->
      let t1, t2 =
        match t_args with
        | [ t1; t2 ] -> (t1, t2)
        | _ -> failwith "Incorrect number of arguments for binop"
      in
      if not (BT.equal t1.bt t2.bt) then (
        print_endline
          Pp.(
            plain
              (IT.pp it
               ^^^ pp t1
               ^^ parens (BT.pp t1.bt)
               ^^^ pp t2
               ^^ parens (BT.pp t2.bt)));
        failwith __LOC__);
      forward_abs_binop op t1 t2
    | Unop (op, _) ->
      let t =
        match t_args with
        | [ t ] -> t
        | _ -> failwith "Incorrect number of arguments for unop"
      in
      forward_abs_unop op t
    | Cast (target_bt, _) ->
      let t =
        match t_args with
        | [ t ] -> t
        | _ -> failwith "Incorrect number of arguments for cast"
      in
      Some (cast t target_bt)
    | _ -> None


  let rec backward_abs_it (it : IT.t) (ts : t list) =
    let (IT (it_, _, loc)) = it in
    match it_ with
    | Binop (EQ, it', _) ->
      let bt = IT.get_bt it' in
      if Option.is_none (BT.is_bits_bt bt) && not (BT.equal bt (BT.Loc ())) then
        ts
      else (
        let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
        let t = meet t1 t2 in
        [ t; t ])
    (* Handle inequality via negation of equality *)
    | Unop (Not, IT (Binop (EQ, it', _), _, _)) ->
      let bt = IT.get_bt it' in
      if Option.is_none (BT.is_bits_bt bt) && not (BT.equal bt (BT.Loc ())) then
        ts
      else (
        let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
        (* For inequality (not equal), refine by removing the excluded value *)
        let is_constant t = Z.equal t.mask Z.zero in
        if is_constant t2 then (
          (* If t2 is a constant c, try to refine t1 to exclude c *)
          let c = t2.value in
          if is_constant t1 && Z.equal t1.value c then
            (* Both are same constant - contradiction: return bottom for both *)
            [ bottom t1.bt; bottom t2.bt ]
          else if is_constant t1 then (* t1 is different constant - no change needed *)
            [ t1; t2 ]
          else (
            (* Check if mask has exactly one bit set (t1 represents 2 values) *)
            let is_power_of_two m =
              (not (Z.equal m Z.zero)) && Z.equal (Z.logand m (Z.sub m Z.one)) Z.zero
            in
            if is_power_of_two t1.mask then (
              (* t1 represents exactly two values: t1.value and t1.value | t1.mask *)
              let v1 = t1.value in
              let v2 = Z.logor t1.value t1.mask in
              if Z.equal v1 c then
                [ of_const bt v2; t2 ] (* Eliminate v1, keep v2 *)
              else if Z.equal v2 c then
                [ of_const bt v1; t2 ] (* Eliminate v2, keep v1 *)
              else
                ts (* c is not one of the possibilities *))
            else
              ts (* More than 2 possible values - conservative *)))
        else if is_constant t1 then (
          (* Symmetric case: t1 is a constant, refine t2 *)
          let c = t1.value in
          (* Note: is_constant t2 && t2.value = c case already handled above *)
          let is_power_of_two m =
            (not (Z.equal m Z.zero)) && Z.equal (Z.logand m (Z.sub m Z.one)) Z.zero
          in
          if is_power_of_two t2.mask then (
            let v1 = t2.value in
            let v2 = Z.logor t2.value t2.mask in
            if Z.equal v1 c then
              [ t1; of_const bt v2 ]
            else if Z.equal v2 c then
              [ t1; of_const bt v1 ]
            else
              ts)
          else
            ts)
        else (* Both are non-constant - conservative: no refinement *)
          ts)
    | Binop (LE, it', _) | Binop (LEPointer, it', _) ->
      let bt = IT.get_bt it' in
      let min, max = get_extrema bt in
      let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
      (* Convert stored unsigned values to signed for interval computation *)
      let t1_val = to_signed_value bt t1.value in
      let t2_val = to_signed_value bt t2.value in
      let t1' = of_interval bt min t2_val in
      let t2' = of_interval bt t1_val max in
      let t1'' = if Z.equal t2.mask Z.zero then meet t1 t1' else t1 in
      let t2'' = if Z.equal t1.mask Z.zero then meet t2 t2' else t2 in
      [ t1''; t2'' ]
    | Binop (LT, it', _) | Binop (LTPointer, it', _) ->
      let bt = IT.get_bt it' in
      let min, max = get_extrema bt in
      let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
      (* Convert stored unsigned values to signed for interval computation *)
      let t1_val = to_signed_value bt t1.value in
      let t2_val = to_signed_value bt t2.value in
      let t1' = of_interval bt min (Z.sub t2_val Z.one) in
      let t2' = of_interval bt (Z.add t1_val Z.one) max in
      let t1'' = if Z.equal t2.mask Z.zero then meet t1 t1' else t1 in
      let t2'' = if Z.equal t1.mask Z.zero then meet t2 t2' else t2 in
      [ t1''; t2'' ]
    | Unop (Not, IT (Binop (LE, it1, it2), _, _))
    | Unop (Not, IT (Binop (LEPointer, it1, it2), _, _)) ->
      backward_abs_it (IT.le_ (it2, it1) loc) ts
    | Unop (Not, IT (Binop (LT, it1, it2), _, _))
    | Unop (Not, IT (Binop (LTPointer, it1, it2), _, _)) ->
      backward_abs_it (IT.lt_ (it2, it1) loc) ts
    | Binop (BW_And, _, _) ->
      (* Backward refinement for bitwise AND: if we know (x & mask) = result,
         we can refine x. For bits where mask is 1, x must match result.
         For bits where mask is 0, we learn nothing about x. *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; bottom t2.bt ]
      else if Z.equal t2.mask Z.zero then (
        (* t2 (mask) is a constant - we can refine t1 (x) *)
        let mask_val = t2.value in
        let fm = full_mask t1.bt in
        (* For bits where mask is 1, x must have the same value as result *)
        (* For bits where mask is 0, x is unconstrained *)
        (* New value for x: result.value for masked bits *)
        let new_value = Z.logand result.value mask_val in
        (* New mask for x: unknown where mask is 0, or where result is unknown *)
        let new_mask = Z.logand fm (Z.logor (Z.lognot mask_val) result.mask) in
        let refined_t1 = of_tnum t1.bt new_value new_mask in
        [ meet t1 refined_t1; t2 ])
      else if Z.equal t1.mask Z.zero then (
        (* t1 (x) is a constant, t2 (mask) is variable - less useful refinement *)
        (* Similar logic but for mask *)
        let x_val = t1.value in
        let fm = full_mask t2.bt in
        (* For bits where x is 1, mask must match result *)
        (* For bits where x is 0, result must be 0 regardless of mask *)
        let new_value = Z.logand result.value x_val in
        let new_mask = Z.logand fm (Z.logor (Z.lognot x_val) result.mask) in
        let refined_t2 = of_tnum t2.bt new_value new_mask in
        [ t1; meet t2 refined_t2 ])
      else (* Both operands are variable - limited refinement possible *)
        [ t1; t2 ]
    | Binop (BW_Or, _, _) ->
      (* Backward refinement for bitwise OR: R = P | Q
         - R[k]=0 => both P[k] and Q[k] must be 0
         - R[k]=1 and Q[k]=0 (known) => P[k] must be 1 *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; bottom t2.bt ]
      else (
        let fm = full_mask t1.bt in
        (* Bits where result is known 0 *)
        let result_k0 =
          Z.logand fm (Z.logand (Z.lognot result.value) (Z.lognot result.mask))
        in
        (* Bits where result is known 1 *)
        let result_k1 = Z.logand fm (Z.logand result.value (Z.lognot result.mask)) in
        (* Refine t1 based on t2 *)
        let refine_operand t other =
          (* Where result is known 0, operand must be 0 *)
          (* Where result is known 1 and other is known 0, operand must be 1 *)
          let other_k0 =
            Z.logand fm (Z.logand (Z.lognot other.value) (Z.lognot other.mask))
          in
          (* Bits forced to 0: where result is known 0 *)
          let forced_0 = result_k0 in
          (* Bits forced to 1: where result is known 1 AND other is known 0 *)
          let forced_1 = Z.logand result_k1 other_k0 in
          (* New value: current value bits OR forced_1 bits, but clear forced_0 bits *)
          let new_value = Z.logand (Z.logor t.value forced_1) (Z.lognot forced_0) in
          (* New mask: clear bits that are now known (forced_0 or forced_1) *)
          let new_mask = Z.logand t.mask (Z.lognot (Z.logor forced_0 forced_1)) in
          of_tnum t.bt new_value new_mask
        in
        let refined_t1 = meet t1 (refine_operand t1 t2) in
        let refined_t2 = meet t2 (refine_operand t2 t1) in
        [ refined_t1; refined_t2 ])
    | Binop (BW_Xor, _, _) ->
      (* Backward refinement for bitwise XOR: R = P ^ Q
         If R[k] and Q[k] are both known, P[k] = R[k] ^ Q[k] *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; bottom t2.bt ]
      else (
        let fm = full_mask t1.bt in
        (* Refine t1 based on result and t2 *)
        let refine_operand t other =
          (* Bits known in both result and other *)
          let result_known = Z.logand fm (Z.lognot result.mask) in
          let other_known = Z.logand fm (Z.lognot other.mask) in
          let both_known = Z.logand result_known other_known in
          (* For bits known in both, derive the operand value via XOR *)
          let derived_value = Z.logand (Z.logxor result.value other.value) both_known in
          (* New mask: unknown bits minus the newly derivable bits *)
          let new_mask = Z.logand t.mask (Z.lognot both_known) in
          (* New value: keep existing known bits, add newly derived bits *)
          let existing_known = Z.logand t.value (Z.lognot t.mask) in
          let new_value = Z.logor existing_known derived_value in
          of_tnum t.bt new_value new_mask
        in
        let refined_t1 = meet t1 (refine_operand t1 t2) in
        let refined_t2 = meet t2 (refine_operand t2 t1) in
        [ refined_t1; refined_t2 ])
    | Unop (BW_Compl, _) ->
      (* Backward refinement for bitwise NOT: R = ~P
         If R[k] is known, P[k] = ~R[k] *)
      let result, t1 = match ts with [ r; t1 ] -> (r, t1) | _ -> failwith __LOC__ in
      if is_bottom result then
        [ bottom t1.bt ]
      else (
        let fm = full_mask t1.bt in
        (* Flip the known bits of result to get constraints on t1 *)
        (* Known 0 in result means known 1 in operand, and vice versa *)
        let result_k0 =
          Z.logand fm (Z.logand (Z.lognot result.value) (Z.lognot result.mask))
        in
        let result_k1 = Z.logand fm (Z.logand result.value (Z.lognot result.mask)) in
        (* After NOT: result_k0 -> operand_k1, result_k1 -> operand_k0 *)
        let derived_value = result_k0 in
        (* Bits known from result *)
        let derived_known = Z.logor result_k0 result_k1 in
        let new_mask = Z.logand t1.mask (Z.lognot derived_known) in
        let refined_t1 = of_tnum t1.bt derived_value new_mask in
        [ meet t1 refined_t1 ])
    | Binop (ShiftLeft, _, _) ->
      (* Backward refinement for left shift: R = P << k (constant k)
         P' = P ⊓ (R.v >> k, R.m >> k) *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; t2 ]
      else if not (Z.equal t2.mask Z.zero) then (* Non-constant shift: no refinement *)
        [ t1; t2 ]
      else (
        let shift = Z.to_int t2.value in
        let width = get_width t1.bt in
        if shift < 0 || shift >= width then
          [ t1; t2 ]
        else (
          (* Shift result back right to get constraint on operand *)
          let derived_value = Z.shift_right result.value shift in
          let derived_mask = Z.shift_right result.mask shift in
          let refined_t1 = of_tnum t1.bt derived_value derived_mask in
          [ meet t1 refined_t1; t2 ]))
    | Binop (ShiftRight, _, _) ->
      (* Backward refinement for right shift: R = P >> k (constant k)
         P' = P ⊓ (R.v << k, (R.m << k) | low_k_bits)
         The low k bits of P are lost in the shift, so they remain unknown *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; t2 ]
      else if not (Z.equal t2.mask Z.zero) then (* Non-constant shift: no refinement *)
        [ t1; t2 ]
      else (
        let shift = Z.to_int t2.value in
        let width = get_width t1.bt in
        if shift < 0 || shift >= width then
          [ t1; t2 ]
        else (
          let fm = full_mask t1.bt in
          (* Shift result back left *)
          let derived_value = Z.logand fm (Z.shift_left result.value shift) in
          (* Low k bits become unknown (they were shifted out) *)
          let low_k_bits = Z.sub (Z.shift_left Z.one shift) Z.one in
          let derived_mask =
            Z.logand fm (Z.logor (Z.shift_left result.mask shift) low_k_bits)
          in
          let refined_t1 = of_tnum t1.bt derived_value derived_mask in
          [ meet t1 refined_t1; t2 ]))
    | _ ->
      if BT.equal BT.Bool (IT.get_bt it) then
        ts
      else
        List.tl ts


  let widen _ = failwith __LOC__

  let narrow _ = failwith __LOC__

  let pp_params () = "ty tnum_value, ty tnum_mask"

  let pp_sym_args () = "tnum_value, tnum_mask"

  let pp_args { bt; is_bottom; value; mask } =
    assert (not is_bottom);
    let sign, width =
      match bt with
      | Loc () -> (BT.Unsigned, Memory.uintptr_bt |> BT.is_bits_bt |> Option.get |> snd)
      | Bits (sign, sz) -> (sign, sz)
      | _ -> failwith ("unsupported type: " ^ Pp.plain (BaseTypes.pp bt))
    in
    let z_min, _ = BT.bits_range (sign, width) in
    let suffix =
      let size_of = Memory.size_of_integer_type in
      match sign with
      | Unsigned ->
        if width <= size_of (Unsigned Int_) then
          Some A.U
        else if width <= size_of (Unsigned Long) then
          Some A.UL
        else
          Some A.ULL
      | Signed ->
        if width <= size_of (Signed Int_) then
          None
        else if width <= size_of (Signed Long) then
          Some A.L
        else
          Some A.LL
    in
    let mk_ail_const v =
      Fulminate.Utils.mk_expr
        (let k a = A.(AilEconst (ConstantInteger (IConstant (a, Decimal, suffix)))) in
         if Z.equal v z_min && BT.equal_sign sign BT.Signed then
           A.(
             AilEbinary
               ( Fulminate.Utils.mk_expr (k (Z.neg (Z.sub (Z.neg v) Z.one))),
                 Arithmetic Sub,
                 Fulminate.Utils.mk_expr (k Z.one) ))
         else
           k v)
    in
    let open Pp in
    plain
      (CF.Pp_ail.pp_expression (mk_ail_const value)
       ^^ comma
       ^^ space
       ^^ CF.Pp_ail.pp_expression (mk_ail_const mask))


  let definitions () = Pp.empty

  let to_it (sym : Sym.t) (t : t) : IT.t =
    let loc = Locations.other __LOC__ in
    if is_bottom t then
      IT.bool_ false loc
    else if is_top t then
      IT.bool_ true loc
    else if Z.equal t.mask Z.zero then (
      (* Constant: sym == value *)
      let sym_it = IT.sym_ (sym, t.bt, loc) in
      let value_it = IT.num_lit_ t.value t.bt loc in
      IT.eq_ (sym_it, value_it) loc)
    else (
      (* General case: (sym & ~mask) == value *)
      let sym_it = IT.sym_ (sym, t.bt, loc) in
      let not_mask = Z.logand (Z.lognot t.mask) (full_mask t.bt) in
      let not_mask_it = IT.num_lit_ not_mask t.bt loc in
      let masked_sym = IT.add_ (sym_it, not_mask_it) loc in
      let value_it = IT.num_lit_ t.value t.bt loc in
      IT.eq_ (masked_sym, value_it) loc)


  let to_lc (t : t) (sym : Sym.t) : LogicalConstraints.t =
    LogicalConstraints.T (to_it sym t)
end

module Inner : Domain.T = NonRelational.Make (TristateBasis)
