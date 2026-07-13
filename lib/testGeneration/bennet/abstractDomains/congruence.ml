module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module MT = MakeTerm

(** Congruence abstract domain.

    A congruence (modulus, residue) represents the set aZ + b,
    i.e., all integers congruent to b modulo a.

    Special cases:
    - modulus = 0: singleton {residue}
    - modulus = 1: top (all values of the type)

    For bitvectors of width w, ξ-normalization ensures:
    - modulus = gcd(modulus, 2^w)  (always a power-of-2 dividing 2^w)
    - residue = residue mod modulus (reduced, in [0, 2^w)) *)

module CongruenceBasis = struct
  let name = "congruence"

  let c_name = "congr"

  type t =
    { bt : BT.t;
      is_bottom : bool;
      modulus : Z.t; (* stride: 0 = singleton, 1 = top after ξ-norm *)
      residue : Z.t (* offset, unsigned in [0, 2^w) *)
    }

  let supported (bt : BT.t) = match bt with Bits _ | Loc _ -> true | _ -> false

  let bt { bt; _ } = bt

  let bottom bt = { bt; is_bottom = true; modulus = Z.zero; residue = Z.zero }

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


  (* 2^w for the given base type *)
  let two_to_w bt =
    let width = get_width bt in
    Z.shift_left Z.one width


  (* Full mask: 2^w - 1 *)
  let full_mask bt = Z.sub (two_to_w bt) Z.one

  (* Convert an unsigned stored value back to signed representation *)
  let to_signed_value bt v =
    match BT.is_bits_bt bt with
    | Some (BT.Signed, width) ->
      let sign_bit = Z.shift_left Z.one (width - 1) in
      if Z.geq v sign_bit then Z.sub v (Z.shift_left Z.one width) else v
    | _ -> v


  (* ξ-normalization: ensure modulus divides 2^w, residue is reduced *)
  let xi_normalize bt modulus residue =
    let tw = two_to_w bt in
    let fm = Z.sub tw Z.one in
    if Z.equal modulus Z.zero then (
      (* Singleton: just mask residue to width *)
      let r = Z.logand residue fm in
      (Z.zero, r))
    else (
      let m = Z.gcd modulus tw in
      if Z.equal m tw then (
        (* modulus is a multiple of 2^w → singleton in bitvector semantics *)
        let r = Z.logand residue fm in
        (Z.zero, r))
      else (
        (* m is always a power-of-2 dividing 2^w *)
        let r = Z.erem residue m in
        (* Also mask to bit width *)
        let r = Z.logand r fm in
        (m, r)))


  (* Create a ξ-normalized congruence *)
  let mk bt modulus residue =
    let m, r = xi_normalize bt modulus residue in
    { bt; is_bottom = false; modulus = m; residue = r }


  (* Top: 1·Z + 0 (after ξ, gcd(1, 2^w) = 1) *)
  let top bt = { bt; is_bottom = false; modulus = Z.one; residue = Z.zero }

  let is_top { is_bottom; modulus; _ } = (not is_bottom) && Z.equal modulus Z.one

  (* Create a constant congruence (singleton) *)
  let of_const bt n =
    let fm = full_mask bt in
    let r = Z.logand n fm in
    { bt; is_bottom = false; modulus = Z.zero; residue = r }


  let of_interval bt start stop =
    if Z.gt start stop then
      bottom bt
    else if Z.equal start stop then
      of_const bt start
    else
      top bt


  let to_interval t =
    if is_bottom t || is_top t then
      None
    else if Z.equal t.modulus Z.zero then (* Singleton *)
      Some (t.residue, t.residue)
    else (
      (* General case: compute range of congruence class within type *)
      let fm = full_mask t.bt in
      (* The minimum value is residue itself *)
      let lo = t.residue in
      (* The maximum value: largest residue + k*modulus <= fm *)
      let range = Z.sub fm t.residue in
      let k = Z.div range t.modulus in
      let hi = Z.add t.residue (Z.mul k t.modulus) in
      Some (lo, hi))


  let equal t1 t2 =
    if is_bottom t1 && is_bottom t2 then
      true
    else if is_bottom t1 || is_bottom t2 then
      false
    else
      BT.equal t1.bt t2.bt
      && Z.equal t1.modulus t2.modulus
      && Z.equal t1.residue t2.residue


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
        let c = Z.compare t1.modulus t2.modulus in
        if c <> 0 then c else Z.compare t1.residue t2.residue))


  (* Partial order: t1 ⊑ t2 iff γ(t1) ⊆ γ(t2)
     This holds when: t2.modulus | t1.modulus  and  t1.residue ≡ t2.residue (mod t2.modulus) *)
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
      if Z.equal t2.modulus Z.zero then
        (* t2 is singleton: t1 must also be same singleton *)
        Z.equal t1.modulus Z.zero && Z.equal t1.residue t2.residue
      else (* t2.modulus | t1.modulus and residues match mod t2.modulus *)
        Z.equal (Z.erem t1.modulus t2.modulus) Z.zero
        && Z.equal (Z.erem t1.residue t2.modulus) (Z.erem t2.residue t2.modulus))


  (* Join (least upper bound):
     gcd(a, c, |b-d|)·Z + (b mod gcd(a, c, |b-d|)), ξ-normalized *)
  let join t1 t2 =
    if is_bottom t1 then
      t2
    else if is_bottom t2 then
      t1
    else (
      assert (BT.equal t1.bt t2.bt);
      let a = t1.modulus in
      let b = t1.residue in
      let c = t2.modulus in
      let d = t2.residue in
      let diff = Z.abs (Z.sub b d) in
      let g = Z.gcd (Z.gcd a c) diff in
      if Z.equal g Z.zero then (* Both are singletons with same residue *)
        t1
      else
        mk t1.bt g (Z.erem b g))


  (* Meet (greatest lower bound):
     Uses CRT when compatible, bottom when incompatible *)
  let meet t1 t2 =
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if is_top t1 then
      t2
    else if is_top t2 then
      t1
    else (
      assert (BT.equal t1.bt t2.bt);
      let a = t1.modulus in
      let b = t1.residue in
      let c = t2.modulus in
      let d = t2.residue in
      if Z.equal a Z.zero && Z.equal c Z.zero then
        if
          (* Both singletons *)
          Z.equal b d
        then
          t1
        else
          bottom t1.bt
      else if Z.equal a Z.zero then
        if
          (* t1 singleton, check if b is in t2's congruence class *)
          Z.equal (Z.erem b c) (Z.erem d c)
        then
          t1
        else
          bottom t1.bt
      else if Z.equal c Z.zero then
        if
          (* t2 singleton, check if d is in t1's congruence class *)
          Z.equal (Z.erem d a) (Z.erem b a)
        then
          t2
        else
          bottom t1.bt
      else (
        (* General CRT case *)
        let g = Z.gcd a c in
        if not (Z.equal (Z.erem (Z.sub b d) g) Z.zero) then (* Residues incompatible *)
          bottom t1.bt
        else (
          (* CRT: find r such that r ≡ b (mod a) and r ≡ d (mod c) *)
          let l = Z.lcm a c in
          (* Use extended GCD: g = a*u + c*v *)
          let _g, u, _v = Z.gcdext a c in
          (* r = b + a * u * ((d - b) / g) *)
          let diff_div_g = Z.div (Z.sub d b) g in
          let r = Z.add b (Z.mul (Z.mul a u) diff_div_g) in
          mk t1.bt l r)))


  let join_many ts =
    match ts with
    | [] -> failwith "join_many requires a non-empty list"
    | h :: t -> List.fold_left join h t


  let meet_many ts =
    match ts with
    | [] -> failwith "meet_many requires a non-empty list"
    | h :: t -> List.fold_left meet h t


  let is_meet_assoc = true

  let is_join_assoc = true

  (* ---- Forward abstract transformers ---- *)

  (* Addition: gcd(a, c, 2^w)·Z + ((b+d) mod 2^w) *)
  let congr_add t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let tw = two_to_w t1.bt in
      let fm = full_mask t1.bt in
      let g = Z.gcd (Z.gcd t1.modulus t2.modulus) tw in
      let r = Z.logand (Z.add t1.residue t2.residue) fm in
      mk t1.bt g r)


  (* Subtraction: gcd(a, c, 2^w)·Z + ((b-d) mod 2^w) *)
  let congr_sub t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let tw = two_to_w t1.bt in
      let fm = full_mask t1.bt in
      let g = Z.gcd (Z.gcd t1.modulus t2.modulus) tw in
      let r = Z.logand (Z.sub t1.residue t2.residue) fm in
      mk t1.bt g r)


  (* Negation: a·Z + ((2^w - b) mod 2^w) *)
  let congr_neg t =
    if is_bottom t then
      bottom t.bt
    else (
      let fm = full_mask t.bt in
      let r = Z.logand (Z.sub (two_to_w t.bt) t.residue) fm in
      mk t.bt t.modulus r)


  (* Multiplication: gcd(ac, ad, bc, 2^w)·Z + ((bd) mod 2^w) *)
  let congr_mul t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else (
      let tw = two_to_w t1.bt in
      let fm = full_mask t1.bt in
      let a = t1.modulus in
      let b = t1.residue in
      let c = t2.modulus in
      let d = t2.residue in
      let ac = Z.mul a c in
      let ad = Z.mul a d in
      let bc = Z.mul b c in
      let g = Z.gcd (Z.gcd (Z.gcd ac ad) bc) tw in
      let r = Z.logand (Z.mul b d) fm in
      mk t1.bt g r)


  (* Division: singleton divisor n≠0 where n|a and n|b *)
  let congr_div t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t2.modulus Z.zero && Z.equal t2.residue Z.zero then
      (* Division by zero *)
      bottom t1.bt
    else if Z.equal t2.modulus Z.zero then (
      (* Singleton divisor n *)
      let n = t2.residue in
      let abs_n = Z.abs n in
      if Z.equal t1.modulus Z.zero then
        (* Both constants: divide with the signed interpretation for signed types
           (Z.div truncates toward zero, matching C), then of_const re-normalizes
           to [0, 2^w). Dividing the raw unsigned residues would make e.g. int8
           {6} / {-3} yield {0} instead of {-2}. *)
        of_const
          t1.bt
          (Z.div (to_signed_value t1.bt t1.residue) (to_signed_value t1.bt n))
      else if
        Z.equal (Z.erem t1.modulus abs_n) Z.zero
        && Z.equal (Z.erem t1.residue abs_n) Z.zero
      then
        mk t1.bt (Z.div t1.modulus abs_n) (Z.div t1.residue n)
      else
        top t1.bt)
    else
      top t1.bt


  (* Modulo: singleton divisor n≠0 *)
  let congr_mod t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t2.modulus Z.zero && Z.equal t2.residue Z.zero then
      (* Modulo by zero *)
      bottom t1.bt
    else if Z.equal t2.modulus Z.zero then (
      (* Singleton divisor n *)
      let n = t2.residue in
      let m = Z.abs n in
      if Z.equal t1.modulus Z.zero then (* Both constants *)
        of_const t1.bt (Z.erem t1.residue m)
      else (
        let g = Z.gcd t1.modulus m in
        mk t1.bt g (Z.erem t1.residue g)))
    else
      top t1.bt


  (* Bitwise operations: extract trailing zeros k = trailing_zeros(modulus) *)
  let trailing_zeros m =
    if Z.equal m Z.zero then
      max_int (* singleton: all bits "known" *)
    else
      Z.trailing_zeros m


  let congr_and t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t1.modulus Z.zero && Z.equal t2.modulus Z.zero then
      (* Both singletons: exact constant. Guards against trailing_zeros = max_int
         (for modulus 0) feeding Z.shift_left Z.one max_int -> out-of-memory. *)
      of_const t1.bt (Z.logand t1.residue t2.residue)
    else (
      let k1 = trailing_zeros t1.modulus in
      let k2 = trailing_zeros t2.modulus in
      let k = min k1 k2 in
      if k = 0 then
        top t1.bt
      else (
        let mask = Z.sub (Z.shift_left Z.one k) Z.one in
        let r = Z.logand (Z.logand t1.residue t2.residue) mask in
        mk t1.bt (Z.shift_left Z.one k) r))


  let congr_or t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t1.modulus Z.zero && Z.equal t2.modulus Z.zero then
      of_const t1.bt (Z.logor t1.residue t2.residue)
    else (
      let k1 = trailing_zeros t1.modulus in
      let k2 = trailing_zeros t2.modulus in
      let k = min k1 k2 in
      if k = 0 then
        top t1.bt
      else (
        let mask = Z.sub (Z.shift_left Z.one k) Z.one in
        let r = Z.logand (Z.logor t1.residue t2.residue) mask in
        mk t1.bt (Z.shift_left Z.one k) r))


  let congr_xor t1 t2 =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      bottom t1.bt
    else if Z.equal t1.modulus Z.zero && Z.equal t2.modulus Z.zero then
      of_const t1.bt (Z.logxor t1.residue t2.residue)
    else (
      let k1 = trailing_zeros t1.modulus in
      let k2 = trailing_zeros t2.modulus in
      let k = min k1 k2 in
      if k = 0 then
        top t1.bt
      else (
        let mask = Z.sub (Z.shift_left Z.one k) Z.one in
        let r = Z.logand (Z.logxor t1.residue t2.residue) mask in
        mk t1.bt (Z.shift_left Z.one k) r))


  (* Left shift by constant k: gcd(a·2^k, 2^w)·Z + ((b·2^k) mod 2^w) *)
  let congr_shl t shift_amt =
    if is_bottom t || is_bottom shift_amt then
      bottom t.bt
    else if not (Z.equal shift_amt.modulus Z.zero) then (* Non-constant shift *)
      top t.bt
    else (
      let width = get_width t.bt in
      (* Clamp before Z.to_int: residue is normalized to [0, 2^w), so a 64-bit
         shift amount can exceed native int and raise Z.Overflow. k >= width -> top
         (project-wide shift>=width = top; congr_lshr already returns top). *)
      if Z.geq shift_amt.residue (Z.of_int width) then
        top t.bt
      else (
        let k = Z.to_int shift_amt.residue in
        let tw = two_to_w t.bt in
        let fm = full_mask t.bt in
        let two_k = Z.shift_left Z.one k in
        let new_mod = Z.gcd (Z.mul t.modulus two_k) tw in
        let new_res = Z.logand (Z.mul t.residue two_k) fm in
        mk t.bt new_mod new_res))


  (* Logical right shift by constant k *)
  let congr_lshr t shift_amt =
    if is_bottom t || is_bottom shift_amt then
      bottom t.bt
    else if not (Z.equal shift_amt.modulus Z.zero) then (* Non-constant shift *)
      top t.bt
    else (
      let width = get_width t.bt in
      (* Clamp before Z.to_int (see congr_shl): a 64-bit shift amount can exceed
         native int and raise Z.Overflow. k >= width -> top. *)
      if Z.geq shift_amt.residue (Z.of_int width) then
        top t.bt
      else (
        let k = Z.to_int shift_amt.residue in
        let k_tz = trailing_zeros t.modulus in
        if k <= k_tz then (
          let new_mod = Z.shift_right t.modulus k in
          let new_res = Z.shift_right t.residue k in
          mk t.bt new_mod new_res)
        else
          top t.bt))


  let forward_abs_binop (op : Terms.binop) (t1 : t) (t2 : t) : t option =
    assert (BT.equal t1.bt t2.bt);
    if is_bottom t1 || is_bottom t2 then
      Some (bottom t1.bt)
    else (
      match op with
      | Add -> Some (congr_add t1 t2)
      | Sub -> Some (congr_sub t1 t2)
      | Mul -> Some (congr_mul t1 t2)
      | Div | DivNoSMT -> Some (congr_div t1 t2)
      | Mod -> Some (congr_mod t1 t2)
      | BW_And -> Some (congr_and t1 t2)
      | BW_Or -> Some (congr_or t1 t2)
      | BW_Xor -> Some (congr_xor t1 t2)
      | ShiftLeft -> Some (congr_shl t1 t2)
      | ShiftRight -> Some (congr_lshr t1 t2)
      | _ -> None)


  let forward_abs_unop (op : Terms.unop) (t : t) : t option =
    match op with
    | Negate -> Some (congr_neg t)
    | BW_Compl ->
      if is_bottom t then
        Some (bottom t.bt)
      else if Z.equal t.modulus Z.zero then (
        let fm = full_mask t.bt in
        Some (of_const t.bt (Z.logand (Z.logxor t.residue fm) fm)))
      else (* ~x = -x - 1, so modulus is preserved *)
        Some (mk t.bt t.modulus (Z.logand (Z.lognot t.residue) (full_mask t.bt)))
    | _ -> None


  let pp { bt = _; is_bottom; modulus; residue } =
    let open Pp in
    if is_bottom then
      !^"⊥"
    else if Z.equal modulus Z.zero then
      z residue
    else if Z.equal modulus Z.one then
      !^"⊤"
    else
      z modulus ^^ !^"Z+" ^^ z residue


  let cast (t : t) (target_bt : BT.t) : t =
    if is_bottom t then
      bottom target_bt
    else if not (supported target_bt) then
      top target_bt
    else if is_top t then
      top target_bt
    else (* Re-normalize to target width *)
      mk target_bt t.modulus t.residue


  let forward_abs_it (it : Terms.Normal.t) (t_args : t list) : t option =
    let (IT (it_, bt, _loc)) = it in
    match it_ with
    | Const (Bits (_, n)) -> Some (of_const bt n)
    | Const Null -> Some (of_const bt Z.zero)
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
              (Terms.Normal.pp it
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
    | MemberShift (_, tag, member) ->
      let t =
        match t_args with [ t ] -> t | _ -> failwith "Incorrect number of arguments"
      in
      if t.is_bottom then
        Some (bottom bt)
      else (
        let tag_defs = CF.Tags.tagDefs () in
        let offset =
          Z.of_int (Memory.int_of_ival (CF.Impl_mem.offsetof_ival tag_defs tag member))
        in
        let offset_congr = of_const bt offset in
        forward_abs_binop Terms.Add t offset_congr)
    | ArrayShift { ct; _ } ->
      let t_base, t_index =
        match t_args with
        | [ t1; t2 ] -> (t1, t2)
        | _ -> failwith "Incorrect number of arguments"
      in
      if t_base.is_bottom || t_index.is_bottom then
        Some (bottom bt)
      else (
        let elem_size = Z.of_int (Memory.size_of_ctype ct) in
        let elem_congr = of_const t_index.bt elem_size in
        match forward_abs_binop Terms.Mul t_index elem_congr with
        | Some offset ->
          let offset = { offset with bt = t_base.bt } in
          forward_abs_binop Terms.Add t_base offset
        | None -> None)
    | _ -> None


  let rec backward_abs_it (it : Terms.Normal.t) (ts : t list) =
    let (IT (it_, _, loc)) = it in
    match it_ with
    | Binop (EQ, it', _) ->
      let bt = Terms.Normal.get_bt it' in
      if Option.is_none (BT.is_bits_bt bt) && not (BT.equal bt (BT.Loc ())) then
        ts
      else (
        let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
        let t = meet t1 t2 in
        [ t; t ])
    | Unop (Not, IT (Binop (EQ, it', _), _, _)) ->
      let bt = Terms.Normal.get_bt it' in
      if Option.is_none (BT.is_bits_bt bt) && not (BT.equal bt (BT.Loc ())) then
        ts
      else (
        let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
        (* For inequality: if both are same singleton, contradiction *)
        if
          Z.equal t1.modulus Z.zero
          && Z.equal t2.modulus Z.zero
          && Z.equal t1.residue t2.residue
        then
          [ bottom t1.bt; bottom t2.bt ]
        else
          [ t1; t2 ])
    | Binop (LE, it', _) | Binop (LEPointer, it', _) ->
      let bt = Terms.Normal.get_bt it' in
      let min, max = get_extrema bt in
      let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
      let t1_val = to_signed_value bt t1.residue in
      let t2_val = to_signed_value bt t2.residue in
      let t1' = of_interval bt min t2_val in
      let t2' = of_interval bt t1_val max in
      let t1'' = if Z.equal t2.modulus Z.zero then meet t1 t1' else t1 in
      let t2'' = if Z.equal t1.modulus Z.zero then meet t2 t2' else t2 in
      [ t1''; t2'' ]
    | Binop (LT, it', _) | Binop (LTPointer, it', _) ->
      let bt = Terms.Normal.get_bt it' in
      let min, max = get_extrema bt in
      let t1, t2 = match ts with [ t1; t2 ] -> (t1, t2) | _ -> failwith __LOC__ in
      let t1_val = to_signed_value bt t1.residue in
      let t2_val = to_signed_value bt t2.residue in
      let t1' = of_interval bt min (Z.sub t2_val Z.one) in
      let t2' = of_interval bt (Z.add t1_val Z.one) max in
      let t1'' = if Z.equal t2.modulus Z.zero then meet t1 t1' else t1 in
      let t2'' = if Z.equal t1.modulus Z.zero then meet t2 t2' else t2 in
      [ t1''; t2'' ]
    | Unop (Not, IT (Binop (LE, it1, it2), _, _))
    | Unop (Not, IT (Binop (LEPointer, it1, it2), _, _)) ->
      backward_abs_it (MT.le_ (it2, it1) loc) ts
    | Unop (Not, IT (Binop (LT, it1, it2), _, _))
    | Unop (Not, IT (Binop (LTPointer, it1, it2), _, _)) ->
      backward_abs_it (MT.lt_ (it2, it1) loc) ts
    | Binop (Add, _, _) ->
      (* z = x + y: x ← x ⊓ (z - y), y ← y ⊓ (z - x), z ← z ⊓ (x + y) *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; bottom t2.bt ]
      else (
        let t1' = meet t1 (congr_sub result t2) in
        let t2' = meet t2 (congr_sub result t1) in
        [ t1'; t2' ])
    | Binop (Sub, _, _) ->
      (* z = x - y: x ← x ⊓ (z + y), y ← y ⊓ (x - z), z ← z ⊓ (x - y) *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; bottom t2.bt ]
      else (
        let t1' = meet t1 (congr_add result t2) in
        let t2' = meet t2 (congr_sub t1 result) in
        [ t1'; t2' ])
    | Unop (Negate, _) ->
      (* z = -x: x ← x ⊓ (-z), z ← z ⊓ (-x) *)
      let result, t1 = match ts with [ r; t1 ] -> (r, t1) | _ -> failwith __LOC__ in
      if is_bottom result then
        [ bottom t1.bt ]
      else
        [ meet t1 (congr_neg result) ]
    | Binop (Mul, _, _) ->
      (* Backward mul: if Y is singleton n≠0, refine via exact div *)
      let result, t1, t2 =
        match ts with [ r; t1; t2 ] -> (r, t1, t2) | _ -> failwith __LOC__
      in
      if is_bottom result then
        [ bottom t1.bt; bottom t2.bt ]
      else if Z.equal t2.modulus Z.zero && not (Z.equal t2.residue Z.zero) then (
        let n = t2.residue in
        (* z must be divisible by n *)
        let z_refined = meet result (mk t1.bt n Z.zero) in
        let x_refined = meet t1 (congr_div z_refined t2) in
        [ x_refined; t2 ])
      else if Z.equal t1.modulus Z.zero && not (Z.equal t1.residue Z.zero) then (
        let n = t1.residue in
        let z_refined = meet result (mk t2.bt n Z.zero) in
        let y_refined = meet t2 (congr_div z_refined t1) in
        [ t1; y_refined ])
      else
        [ t1; t2 ]
    | MemberShift (_, tag, member) ->
      let t_res, t_base = match ts with [ r; b ] -> (r, b) | _ -> failwith __LOC__ in
      let tag_defs = CF.Tags.tagDefs () in
      let offset =
        Z.of_int (Memory.int_of_ival (CF.Impl_mem.offsetof_ival tag_defs tag member))
      in
      let offset_congr = of_const t_res.bt offset in
      (match forward_abs_binop Terms.Sub t_res offset_congr with
       | Some refined_base -> [ meet t_base refined_base ]
       | None -> [ t_base ])
    | ArrayShift { ct; _ } ->
      let t_res, t_base, t_index =
        match ts with [ r; b; i ] -> (r, b, i) | _ -> failwith __LOC__
      in
      let elem_size = Z.of_int (Memory.size_of_ctype ct) in
      let elem_congr = of_const t_index.bt elem_size in
      let refined_base =
        match forward_abs_binop Terms.Mul t_index elem_congr with
        | Some offset ->
          let offset = { offset with bt = t_res.bt } in
          (match forward_abs_binop Terms.Sub t_res offset with
           | Some rb -> meet t_base rb
           | None -> t_base)
        | None -> t_base
      in
      let refined_index =
        let t_res_as_index = { t_res with bt = t_index.bt } in
        let t_base_as_index = { t_base with bt = t_index.bt } in
        match forward_abs_binop Terms.Sub t_res_as_index t_base_as_index with
        | Some diff ->
          (match forward_abs_binop Terms.Div diff elem_congr with
           | Some ri -> meet t_index ri
           | None -> t_index)
        | None -> t_index
      in
      [ refined_base; refined_index ]
    | _ ->
      if BT.equal BT.Bool (Terms.Normal.get_bt it) then
        ts
      else
        List.tl ts


  let widen _ = failwith __LOC__

  let narrow _ = failwith __LOC__

  let pp_params () = "ty congr_modulus, ty congr_residue"

  let pp_sym_args () = "congr_modulus, congr_residue"

  let pp_args { bt; is_bottom; modulus; residue } =
    assert (not is_bottom);
    let residue = to_signed_value bt residue in
    let modulus = to_signed_value bt modulus in
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
      (CF.Pp_ail.pp_expression (mk_ail_const modulus)
       ^^ comma
       ^^ space
       ^^ CF.Pp_ail.pp_expression (mk_ail_const residue))


  let definitions () = Pp.empty

  let to_it (sym : Sym.t) (t : t) : Terms.Normal.t =
    let loc = Locations.other __LOC__ in
    if is_bottom t then
      MT.bool_ false loc
    else if is_top t then
      MT.bool_ true loc
    else (
      let bits_bt, sym_it =
        match t.bt with
        | BT.Loc () ->
          (Memory.uintptr_bt, MT.cast_ Memory.uintptr_bt (MT.sym_ (sym, t.bt, loc)) loc)
        | _ -> (t.bt, MT.sym_ (sym, t.bt, loc))
      in
      if Z.equal t.modulus Z.zero then (
        (* Constant: sym == residue *)
        let residue_it = MT.num_lit_ (to_signed_value t.bt t.residue) bits_bt loc in
        MT.eq_ (sym_it, residue_it) loc)
      else (
        (* General case: (sym mod modulus) == (residue mod modulus) *)
        let modulus_it = MT.num_lit_ (to_signed_value t.bt t.modulus) bits_bt loc in
        let sym_mod = MT.binop Mod (sym_it, modulus_it) loc bits_bt in
        let residue_it = MT.num_lit_ (to_signed_value t.bt t.residue) bits_bt loc in
        MT.eq_ (sym_mod, residue_it) loc))


  let to_lc (t : t) (sym : Sym.t) : LogicalConstraints.t =
    LogicalConstraints.T (to_it sym t)
end

module Inner : Domain.T = NonRelational.Make (CongruenceBasis)
