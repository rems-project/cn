module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms

module WrappedIntervalBasis = struct
  let name = "wrapped_interval"

  let c_name = "wint"

  type t =
    { bt : BT.t;
      is_bottom : bool;
      start : Z.t;
      stop : Z.t
    }

  let supported (bt : BT.t) = match bt with Bits _ | Loc _ -> true | _ -> false

  let bt { bt; _ } = bt

  let bottom bt = { bt; is_bottom = true; start = Z.zero; stop = Z.zero }

  let is_bottom { is_bottom; _ } = is_bottom

  let get_bits_bt bt =
    match bt with
    | BT.Loc () -> Memory.uintptr_bt
    | Bits _ -> bt
    | _ -> failwith ("invalid type: " ^ Pp.plain (BT.pp bt) ^ " @ " ^ __LOC__)


  let get_extrema bt =
    let bt =
      match bt with
      | BT.Bits _ -> bt
      | Loc () -> Memory.uintptr_bt
      | _ -> failwith ("invalid type: " ^ Pp.plain (BT.pp bt) ^ " @ " ^ __LOC__)
    in
    BT.bits_range (Option.get (BT.is_bits_bt bt))


  let normalize bt =
    let bt =
      match bt with
      | BT.Bits _ -> bt
      | Loc () -> Memory.uintptr_bt
      | _ -> failwith ("invalid type: " ^ Pp.plain (BT.pp bt) ^ " @ " ^ __LOC__)
    in
    BT.normalise_to_range_bt bt


  let get_width bt =
    let bt = match bt with BT.Loc () -> Memory.uintptr_bt | _ -> bt in
    match BT.is_bits_bt bt with
    | Some (_, w) -> w
    | None -> failwith ("not a bits type: " ^ Pp.plain (BT.pp bt))


  (* Cardinality function: WCard(a,b) = (b-a+1) mod 2^w *)
  let cardinality ({ is_bottom; bt; start; stop } : t) : Z.t =
    if is_bottom then
      Z.zero
    else (
      let width = get_width bt in
      let modulus = Z.shift_left Z.one width in
      let result = Z.rem (Z.add (Z.sub stop start) Z.one) modulus in
      let result = if Z.lt result Z.zero then Z.add result modulus else result in
      if Z.equal result Z.zero then modulus else result)


  (* Lexicographical comparison for wrapped intervals *)
  let lex_leq x y =
    let a = Z.geq x Z.zero in
    let b = Z.geq y Z.zero in
    if (not a) && b then
      false
    else if a && not b then
      true
    else
      Z.leq x y


  (* Wrapped membership test: e ∈ [x,y] iff e - x ≤ y - x (modular arithmetic) *)
  let wrapped_member e start stop bt =
    let normalize = normalize bt in
    let e_norm = normalize e in
    let start_norm = normalize start in
    let stop_norm = normalize stop in
    let diff_e = normalize (Z.sub e_norm start_norm) in
    let diff_stop = normalize (Z.sub stop_norm start_norm) in
    Z.leq diff_e diff_stop


  let top bt =
    let start, stop = get_extrema bt in
    { bt; is_bottom = false; start; stop }


  let is_top { bt; is_bottom; start; stop; _ } =
    if is_bottom then
      false
    else (
      let min, max = get_extrema bt in
      (Z.equal start min && Z.equal stop max)
      || Z.equal (normalize bt (Z.sub start stop)) Z.one)


  let equal b1 b2 =
    if is_top b1 && is_top b2 then
      true
    else if is_bottom b1 && is_bottom b2 then
      true
    else
      BT.equal b1.bt b2.bt && Z.equal b1.start b2.start && Z.equal b1.stop b2.stop


  let compare b1 b2 =
    if equal b1 b2 then
      0
    else if is_bottom b1 then
      -1
    else if is_bottom b2 then
      1
    else if Z.equal b1.start b2.start then
      if lex_leq b1.stop b2.stop then
        -1
      else
        1
    else if lex_leq b1.start b2.start then
      -1
    else
      1


  let of_interval bt start stop =
    let normalize =
      let bt = match bt with BT.Loc () -> Memory.uintptr_bt | _ -> bt in
      normalize bt
    in
    { bt; is_bottom = false; start = normalize start; stop = normalize stop }


  (* Wrapped interval ordering: S ≤ T iff S ⊆ T as sets *)
  let leq s1 s2 =
    assert (BT.equal s1.bt s2.bt);
    if s1.is_bottom then
      true
    else if s2.is_bottom then
      false
    else if is_top s2 then
      true
    else if is_top s1 then
      false
    else (
      (* For proper containment of wrapped intervals, we need to check:
         S ⊆ T iff all values in S are also in T *)
      let a, b = (s1.start, s1.stop) in
      let c, d = (s2.start, s2.stop) in
      let s1_crosses = Z.gt a b in
      (* S crosses south pole (wraps) *)
      let s2_crosses = Z.gt c d in
      (* T crosses south pole (wraps) *)
      match (s1_crosses, s2_crosses) with
      | false, false ->
        (* Both normal intervals: [a,b] ⊆ [c,d] iff c ≤ a ≤ b ≤ d *)
        Z.leq c a && Z.leq b d
      | false, true ->
        (* S normal, T wrapped: [a,b] ⊆ [c,max]∪[0,d] iff a ≥ c or b ≤ d *)
        Z.geq a c || Z.leq b d
      | true, false ->
        (* S wrapped, T normal: impossible unless T is full range *)
        is_top s2
      | true, true ->
        (* Both wrapped: [a,max]∪[0,b] ⊆ [c,max]∪[0,d] iff a ≥ c and b ≤ d *)
        Z.geq a c && Z.leq b d)


  (* Wrapped join operation with lexicographical tie-breaking *)
  let join s1 s2 =
    assert (BT.equal s1.bt s2.bt);
    if s1.is_bottom then
      s2
    else if s2.is_bottom then
      s1
    else if is_top s1 || is_top s2 then
      top s1.bt
    else (
      let a = s1.start
      and b = s1.stop in
      let c = s2.start
      and d = s2.stop in
      (* Containment cases *)
      if leq s2 s1 then
        s1
      else if leq s1 s2 then
        s2 (* Extra case for top: one covers the other *)
      else if
        wrapped_member a c d s1.bt
        && wrapped_member b c d s1.bt
        && wrapped_member c a b s1.bt
        && wrapped_member d a b s1.bt
      then
        top s1.bt (* Overlapping cases *)
      else if wrapped_member c a b s1.bt then
        of_interval s1.bt a d
      else if wrapped_member a c d s1.bt then
        of_interval s1.bt c b
      (* Non-overlapping cases with tie-breaking *)
      else (
        let card_bc = cardinality (of_interval s1.bt b c) in
        let card_da = cardinality (of_interval s1.bt d a) in
        if Z.lt card_bc card_da then
          of_interval s1.bt a d
        else if Z.gt card_bc card_da then
          of_interval s1.bt c b
        else if
          (* Tie case: use lexicographical comparison *)
          lex_leq a c
        then
          of_interval s1.bt a d
        else
          of_interval s1.bt c b))


  (* Helper: create a wrapped interval, handling bottom for empty cases *)
  let make_interval bt start stop =
    let s = { bt; is_bottom = false; start; stop } in
    if Z.equal (cardinality (of_interval bt start stop)) Z.zero && not (is_top s) then
      bottom bt
    else
      s


  (* Helper: compute the complement of a wrapped interval *)
  let wrapped_complement s =
    if is_bottom s then
      top s.bt
    else if is_top s then
      bottom s.bt
    else (
      let start = normalize s.bt (Z.add s.stop Z.one) in
      let stop = normalize s.bt (Z.sub s.start Z.one) in
      make_interval s.bt start stop)


  (* Wrapped meet operation *)
  let meet s1 s2 =
    assert (BT.equal s1.bt s2.bt);
    if s1.is_bottom || s2.is_bottom then
      bottom s1.bt
    else (
      let a = s1.start in
      let b = s1.stop in
      let c = s2.start in
      let d = s2.stop in
      (* Containment cases (also cover bottom and top cases) *)
      if leq s1 s2 then
        s1
      else if leq s2 s1 then
        s2
      (* If one covers the other, the meet is not convex. We return the one with smallest cardinality *)
      else if
        wrapped_member a c d s1.bt
        && wrapped_member b c d s1.bt
        && wrapped_member c a b s1.bt
        && wrapped_member d a b s1.bt
      then (
        let card1 = cardinality (of_interval s1.bt a b) in
        let card2 = cardinality (of_interval s2.bt c d) in
        if Z.lt card1 card2 || (Z.equal card1 card2 && lex_leq a c) then
          s1
        else
          s2 (* Overlapping cases *))
      else if wrapped_member c a b s1.bt then
        { bt = s1.bt; is_bottom = false; start = c; stop = b }
      else if wrapped_member a c d s1.bt then
        { bt = s1.bt; is_bottom = false; start = a; stop = d }
      else (* Disjoint case *)
        top s1.bt)


  (* Helper: return the interval with the larger cardinality *)
  let bigger r1 r2 =
    if r1.is_bottom then
      r2
    else if r2.is_bottom then
      r1
    else (
      let card1 = cardinality r1 in
      let card2 = cardinality r2 in
      if Z.gt card1 card2 then
        r1
      else if Z.lt card1 card2 then
        r2
      else if
        (* Equal cardinality - use lexicographic tie-breaking like C++ *)
        lex_leq r1.start r2.start
      then
        r1
      else
        r2)


  (* Helper: compute clockwise gap between two intervals *)
  let clockwise_gap r1 r2 =
    if is_bottom r1 || is_bottom r2 then
      bottom r1.bt
    else if
      wrapped_member r1.stop r2.start r2.stop r2.bt
      || wrapped_member r2.start r1.start r1.stop r1.bt
    then
      bottom r1.bt
    else (
      let gap_start = normalize r1.bt (Z.add r1.stop Z.one) in
      let gap_stop = normalize r1.bt (Z.sub r2.start Z.one) in
      (* Check if intervals are adjacent (no gap) *)
      if Z.equal gap_start r2.start then
        bottom r1.bt
      else
        make_interval r1.bt gap_start gap_stop)


  let under_join (intervals : t list) : t =
    match intervals with
    | [] -> failwith "maximal_under_approximation_of_union requires a non-empty list"
    | _ :: _ ->
      (* Remove bottom intervals *)
      let non_bottom_intervals = List.filter (fun i -> not (is_bottom i)) intervals in
      if List.is_empty non_bottom_intervals then
        bottom (List.hd intervals).bt
      else if List.exists is_top non_bottom_intervals then
        top (List.hd non_bottom_intervals).bt
      else (
        let bt = (List.hd non_bottom_intervals).bt in
        (* Sort intervals by lexicographic order of left bound *)
        let sorted_intervals =
          List.sort
            (fun r1 r2 ->
               if lex_leq r1.start r2.start then
                 if Z.equal r1.start r2.start then 0 else -1
               else
                 1)
            non_bottom_intervals
        in
        (* Helper function to check if two intervals overlap *)
        let overlap s1 s2 =
          wrapped_member s1.start s2.start s2.stop bt
          || wrapped_member s1.stop s2.start s2.stop bt
          || wrapped_member s2.start s1.start s1.stop bt
          || wrapped_member s2.stop s1.start s1.stop bt
        in
        (* Helper function to extend an interval with another *)
        let extend s1 s2 = join s1 s2 in
        match sorted_intervals with
        | [] -> bottom bt
        | s1 :: _ ->
          (* f₀ ← s₁ *)
          let f0 = ref s1 in
          let i = ref 1 in
          let n = List.length sorted_intervals in
          (* while i ≤ n ∧ overlap(f₀, sᵢ) do *)
          while !i < n && overlap !f0 (List.nth sorted_intervals !i) do
            (* f₀ ← extend(f₀, sᵢ) *)
            f0 := extend !f0 (List.nth sorted_intervals !i);
            (* i ← i + 1 *)
            incr i
          done;
          (* p ← f ← f₀ *)
          let p = ref !f0 in
          let f = ref !f0 in
          (* while i ≤ n do *)
          while !i < n do
            let si = List.nth sorted_intervals !i in
            if overlap !f si then (* f ← extend(f, sᵢ) *)
              f := extend !f si
            else (
              (* p ← bigger(p, f) *)
              p := bigger !p !f;
              (* f ← sᵢ *)
              f := si);
            (* i ← i + 1 *)
            incr i
          done;
          (* if overlap(f, f₀) then *)
          if overlap !f !f0 then (* f ← extend(f, f₀) *)
            f := extend !f !f0;
          (* return bigger(f, p) *)
          bigger !f !p)


  let over_meet intervals =
    match intervals with
    | [] -> failwith "meet_many requires a non-empty list"
    | _ :: _ ->
      let complements = List.map wrapped_complement intervals in
      let union_of_complements_under_approx = under_join complements in
      wrapped_complement union_of_complements_under_approx


  let meet_many = over_meet

  let join_many (intervals : t list) : t =
    match intervals with
    | [] -> failwith "join_many requires a non-empty list"
    | [ s ] -> s
    | _ :: _ ->
      (* Remove bottom intervals *)
      let non_bottom_intervals = List.filter (fun i -> not (is_bottom i)) intervals in
      if List.is_empty non_bottom_intervals then
        bottom (List.hd intervals).bt
      else if List.exists is_top non_bottom_intervals then
        top (List.hd non_bottom_intervals).bt
      else (
        let bt = (List.hd non_bottom_intervals).bt in
        (* Sort intervals by lexicographic order of lower bound *)
        let sorted_intervals =
          List.sort
            (fun r1 r2 ->
               if lex_leq r1.start r2.start then
                 if Z.equal r1.start r2.start then 0 else -1
               else
                 1)
            non_bottom_intervals
        in
        (* Phase 1: Handle intervals that cross south pole *)
        let f = ref (bottom bt) in
        List.iter
          (fun r ->
             if is_top r || Z.lt r.stop r.start then (* crosses south pole *)
               f := join !f r)
          sorted_intervals;
        (* Phase 2: Compute gaps and extend f with all intervals *)
        let g = ref (bottom bt) in
        List.iter
          (fun r ->
             let gap = clockwise_gap !f r in
             g := bigger !g gap;
             f := join !f r)
          sorted_intervals;
        (* Final result: complement of bigger(g, complement(f)) *)
        let complement_f = wrapped_complement !f in
        let bigger_gap = bigger !g complement_f in
        wrapped_complement bigger_gap)


  let is_meet_assoc = false

  let is_join_assoc = false

  (* Check if operation would cause wrapped overflow *)
  let is_wrapped_overflow_add_sub bt card1 card2 =
    let width = get_width bt in
    let max_card = Z.shift_left Z.one width in
    Z.gt (Z.add card1 card2) max_card


  (* Pole splitting functions *)

  (* North pole split: cut at north pole (0111...1 to 1000...0) for signed operations *)
  let north_split bt start stop =
    let width = get_width bt in
    let np_lb = Z.sub (Z.shift_left Z.one (width - 1)) Z.one in
    (* 0111...1 *)
    let np_ub = Z.neg (Z.shift_left Z.one (width - 1)) in
    (* 1000...0 *)
    let s = { bt; is_bottom = false; start; stop } in
    let np = { bt; is_bottom = false; start = np_lb; stop = np_ub } in
    if not (leq np s) then (* No need to split *)
      [ s ]
    else (
      (* Split into two intervals *)
      let s1 = { bt; is_bottom = false; start; stop = np_lb } in
      let s2 = { bt; is_bottom = false; start = np_ub; stop } in
      [ s1; s2 ])


  (* South pole split: cut at south pole (111...1 to 000...0) for unsigned operations *)
  let south_split bt start stop =
    let width = get_width bt in
    let sp_lb = Z.sub (Z.shift_left Z.one width) Z.one in
    (* 111...1 *)
    let sp_ub = Z.zero in
    (* 000...0 *)
    let s = { bt; is_bottom = false; start; stop } in
    let sp = { bt; is_bottom = false; start = sp_lb; stop = sp_ub } in
    if not (leq sp s) then (* No need to split *)
      [ s ]
    else (
      (* Split into two intervals *)
      let s1 = { bt; is_bottom = false; start; stop = sp_lb } in
      let s2 = { bt; is_bottom = false; start = sp_ub; stop } in
      [ s1; s2 ])


  (* Combined pole split: both north and south *)
  let pole_split bt start stop =
    let north_splits = north_split bt start stop in
    List.concat_map (fun s -> south_split bt s.start s.stop) north_splits


  (* Check if interval crosses south pole (unsigned wraparound from max to 0) *)
  let cross_south_pole start stop =
    (* South pole crossing means the interval wraps around from max unsigned to 0 *)
    Z.gt start stop


  (* Check if interval crosses north pole (signed wraparound from max positive to min negative) *)
  let cross_north_pole bt start stop =
    let width = get_width bt in
    let max_positive = Z.sub (Z.shift_left Z.one (width - 1)) Z.one in
    (* 01...1 *)
    let min_negative = Z.neg (Z.shift_left Z.one (width - 1)) in
    (* 10...0 *)
    (* North pole crossing means interval contains both max positive and min negative *)
    wrapped_member max_positive start stop bt && wrapped_member min_negative start stop bt


  (* Truncate interval to specified number of bits (for left shift overflow detection) *)
  let truncate_to_bits bt interval num_bits =
    if num_bits <= 0 then
      bottom bt
    else if num_bits >= get_width bt then
      interval
    else (
      let mask = Z.sub (Z.shift_left Z.one num_bits) Z.one in
      let truncated_start = Z.logand interval.start mask in
      let truncated_stop = Z.logand interval.stop mask in
      { bt; is_bottom = false; start = truncated_start; stop = truncated_stop })


  (* Remove zero interval from list (for division) *)
  let purge_zero intervals =
    let is_zero_interval s = Z.equal s.start Z.zero && Z.equal s.stop Z.zero in
    List.filter (fun s -> not (is_zero_interval s)) intervals


  (* Check if MSB is zero (positive for signed interpretation) *)
  let is_msb_zero bt value =
    let width = get_width bt in
    let msb_mask = Z.shift_left Z.one (width - 1) in
    Z.equal (Z.logand value msb_mask) Z.zero


  let wrapped_remainder bt dividend divisor is_signed =
    (* Trivial cases *)
    if Z.equal dividend.start Z.zero && Z.equal dividend.stop Z.zero then
      { bt; is_bottom = false; start = Z.zero; stop = Z.zero }
    else if Z.equal divisor.start Z.zero && Z.equal divisor.stop Z.zero then
      bottom bt
    else if dividend.is_bottom || divisor.is_bottom then
      bottom bt
    else if is_signed then (
      (* Signed case: implement MSB-based logic without pole splitting *)
      (* Check MSB of dividend and divisor bounds *)
      let is_dividend_pos =
        is_msb_zero bt dividend.start && is_msb_zero bt dividend.stop
      in
      let is_dividend_neg =
        (not (is_msb_zero bt dividend.start)) && not (is_msb_zero bt dividend.stop)
      in
      let is_divisor_pos = is_msb_zero bt divisor.start && is_msb_zero bt divisor.stop in
      let is_divisor_neg =
        (not (is_msb_zero bt divisor.start)) && not (is_msb_zero bt divisor.stop)
      in
      if is_dividend_pos && is_divisor_pos then (
        (* Both positive: [0, divisor_max-1] *)
        let ub = normalize bt (Z.sub divisor.stop Z.one) in
        { bt; is_bottom = false; start = Z.zero; stop = ub })
      else if is_dividend_pos && is_divisor_neg then (
        (* Dividend positive, divisor negative: [0, -divisor_min-1] *)
        (* divisor.start is the "most negative" value, so -divisor.start-1 gives the upper bound *)
        let width = get_width bt in
        let divisor_start_signed =
          if Z.geq divisor.start (Z.shift_left Z.one (width - 1)) then
            Z.sub divisor.start (Z.shift_left Z.one width)
          else
            divisor.start
        in
        let ub = normalize bt (Z.sub (Z.neg divisor_start_signed) Z.one) in
        { bt; is_bottom = false; start = Z.zero; stop = ub })
      else if is_dividend_neg && is_divisor_pos then (
        (* Dividend negative, divisor positive: [-divisor_max+1, 0] *)
        let lb = normalize bt (Z.add (Z.neg divisor.stop) Z.one) in
        { bt; is_bottom = false; start = lb; stop = Z.zero })
      else if is_dividend_neg && is_divisor_neg then (
        (* Both negative: [divisor_min+1, 0] *)
        let lb = normalize bt (Z.add divisor.start Z.one) in
        { bt; is_bottom = false; start = lb; stop = Z.zero })
      else (
        (* Mixed signs or complex cases - fall back to conservative bounds *)
        let width = get_width bt in
        let max_abs_divisor =
          Z.max
            (Z.abs
               (Z.sub
                  divisor.start
                  (if Z.geq divisor.start (Z.shift_left Z.one (width - 1)) then
                     Z.shift_left Z.one width
                   else
                     Z.zero)))
            (Z.abs
               (Z.sub
                  divisor.stop
                  (if Z.geq divisor.stop (Z.shift_left Z.one (width - 1)) then
                     Z.shift_left Z.one width
                   else
                     Z.zero)))
        in
        let lb = normalize bt (Z.sub Z.one max_abs_divisor) in
        let ub = normalize bt (Z.sub max_abs_divisor Z.one) in
        { bt; is_bottom = false; start = lb; stop = ub }))
    else (
      (* Unsigned case: south pole cut and compute for each element *)
      let s1 = south_split bt dividend.start dividend.stop in
      let s2 = purge_zero (south_split bt divisor.start divisor.stop) in
      if List.is_empty s2 then
        bottom bt
      else (
        let result = ref (bottom bt) in
        List.iter
          (fun _d1 ->
             List.iter
               (fun d2 ->
                  (* For unsigned: result is [0, d-1] where d is upper bound of divisor *)
                  let d = d2.stop in
                  let lb = Z.zero in
                  let ub = Z.sub d Z.one in
                  let tmp = { bt; is_bottom = false; start = lb; stop = ub } in
                  result := join !result tmp)
               s2)
          s1;
        !result))


  (* Wrapped multiplication using pole splitting *)
  let wrapped_multiplication bt s1 s2 =
    if s1.is_bottom || s2.is_bottom then
      bottom bt
    else if is_top s1 || is_top s2 then
      top bt
    else (
      (* Zero interval special case *)
      let is_zero s = Z.equal s.start Z.zero && Z.equal s.stop Z.zero in
      if is_zero s1 || is_zero s2 then
        { bt; is_bottom = false; start = Z.zero; stop = Z.zero }
      else (
        (* General case: pole split both operands and compute all combinations *)
        let s1_splits = pole_split bt s1.start s1.stop in
        let s2_splits = pole_split bt s2.start s2.stop in
        let results = ref [] in
        List.iter
          (fun s1' ->
             List.iter
               (fun s2' ->
                  (* Compute multiplication for this pair *)
                  let mults =
                    [ Z.mul s1'.start s2'.start;
                      Z.mul s1'.start s2'.stop;
                      Z.mul s1'.stop s2'.start;
                      Z.mul s1'.stop s2'.stop
                    ]
                  in
                  let min_mult = List.fold_left Z.min (List.hd mults) (List.tl mults) in
                  let max_mult = List.fold_left Z.max (List.hd mults) (List.tl mults) in
                  let result =
                    { bt; is_bottom = false; start = min_mult; stop = max_mult }
                  in
                  results := result :: !results)
               s2_splits)
          s1_splits;
        (* Join all results *)
        join_many !results))


  (* Wrapped division using pole splitting *)
  let wrapped_division bt dividend divisor is_signed =
    if dividend.is_bottom || divisor.is_bottom then
      bottom bt
    else if Z.equal divisor.start Z.zero && Z.equal divisor.stop Z.zero then
      bottom bt (* Division by zero *)
    else if Z.equal dividend.start Z.zero && Z.equal dividend.stop Z.zero then
      { bt; is_bottom = false; start = Z.zero; stop = Z.zero }
    else (
      (* Split operands and purge zero from divisor *)
      let dividend_splits =
        if is_signed then
          pole_split bt dividend.start dividend.stop
        else
          south_split bt dividend.start dividend.stop
      in
      let divisor_splits =
        if is_signed then
          pole_split bt divisor.start divisor.stop
        else
          south_split bt divisor.start divisor.stop
      in
      let divisor_splits = purge_zero divisor_splits in
      let results = ref [] in
      List.iter
        (fun d1 ->
           List.iter
             (fun d2 ->
                (* Compute division extremes *)
                let divs =
                  [ Z.div d1.start d2.start;
                    Z.div d1.start d2.stop;
                    Z.div d1.stop d2.start;
                    Z.div d1.stop d2.stop
                  ]
                in
                let min_div = List.fold_left Z.min (List.hd divs) (List.tl divs) in
                let max_div = List.fold_left Z.max (List.hd divs) (List.tl divs) in
                let result = { bt; is_bottom = false; start = min_div; stop = max_div } in
                results := result :: !results)
             divisor_splits)
        dividend_splits;
      (* Join all results *)
      join_many !results)


  let pp ({ bt = _; is_bottom; start; stop } as b) =
    let open Pp in
    if is_bottom then
      !^"⊥"
    else if is_top b then
      !^"⊤"
    else if Z.equal start stop then
      z start
    else
      brackets (z start ^^ !^".." ^^ z stop)


  (* Bitwise operation helpers *)

  (* Compute minimum OR: scan from MSB, try to reduce operands *)
  let min_or bt a b c d =
    let width = get_width bt in
    let rec loop m a c =
      if Z.equal m Z.zero then
        Z.logor a c
      else (
        let not_a_and_c_and_m = Z.logand (Z.logand (Z.lognot a) c) m in
        let a_and_not_c_and_m = Z.logand (Z.logand a (Z.lognot c)) m in
        if Z.equal not_a_and_c_and_m m then (
          let temp = Z.logand (Z.logor a m) (Z.neg m) in
          if Z.leq temp b then
            loop (Z.shift_right m 1) temp c
          else
            loop (Z.shift_right m 1) a c)
        else if Z.equal a_and_not_c_and_m m then (
          let temp = Z.logand (Z.logor c m) (Z.neg m) in
          if Z.leq temp d then
            loop (Z.shift_right m 1) a temp
          else
            loop (Z.shift_right m 1) a c)
        else
          loop (Z.shift_right m 1) a c)
    in
    let m = Z.shift_left Z.one (width - 1) in
    loop m a c


  (* Compute maximum OR: scan from MSB, try to maximize result *)
  let max_or bt a b c d =
    let width = get_width bt in
    let rec loop m b d =
      if Z.equal m Z.zero then
        Z.logor b d
      else if Z.equal (Z.logand (Z.logand b d) m) m then (
        let temp = Z.logor (Z.sub b m) (Z.sub m Z.one) in
        if Z.geq temp a then
          Z.logor temp d
        else (
          let temp = Z.logor (Z.sub d m) (Z.sub m Z.one) in
          if Z.geq temp c then
            Z.logor b temp
          else
            loop (Z.shift_right m 1) b d))
      else
        loop (Z.shift_right m 1) b d
    in
    let m = if width > 0 then Z.shift_left Z.one (width - 1) else Z.one in
    loop m b d


  (* Compute minimum AND: scan from MSB, try to reduce result *)
  let min_and bt a b c d =
    let width = get_width bt in
    let rec loop m a c =
      if Z.equal m Z.zero then
        Z.logand a c
      else if Z.equal (Z.logand (Z.logand (Z.lognot a) (Z.lognot c)) m) m then (
        let temp = Z.logand (Z.logor a m) (Z.lognot m) in
        if Z.leq temp b then
          Z.logand temp c
        else (
          let temp = Z.logand (Z.logor c m) (Z.lognot m) in
          if Z.leq temp d then
            Z.logand a temp
          else
            loop (Z.shift_right m 1) a c))
      else
        loop (Z.shift_right m 1) a c
    in
    let m = if width > 0 then Z.shift_left Z.one (width - 1) else Z.one in
    loop m a c


  (* Compute maximum AND: scan from MSB, try to maximize result *)
  let max_and bt a b c d =
    let width = get_width bt in
    let rec loop m b d =
      if Z.equal m Z.zero then
        Z.logand b d
      else if Z.equal (Z.logand (Z.logand b (Z.lognot d)) m) m then (
        let temp = Z.logor (Z.logand b (Z.lognot m)) (Z.sub m Z.one) in
        if Z.geq temp a then
          Z.logand temp d
        else
          loop (Z.shift_right m 1) b d)
      else if Z.equal (Z.logand (Z.logand (Z.lognot b) d) m) m then (
        let temp = Z.logor (Z.logand d (Z.lognot m)) (Z.sub m Z.one) in
        if Z.geq temp c then
          Z.logand b temp
        else
          loop (Z.shift_right m 1) b d)
      else
        loop (Z.shift_right m 1) b d
    in
    let m = if width > 0 then Z.shift_left Z.one (width - 1) else Z.one in
    loop m b d


  (* XOR bounds using De Morgan's laws *)
  let min_xor bt a b c d =
    Z.logor
      (min_and bt a b (Z.lognot d) (Z.lognot c))
      (min_and bt (Z.lognot b) (Z.lognot a) c d)


  let max_xor bt a b c d =
    max_or
      bt
      Z.zero
      (max_and bt a b (Z.lognot d) (Z.lognot c))
      Z.zero
      (max_and bt (Z.lognot b) (Z.lognot a) c d)


  (* Bitwise operation on intervals using south pole splitting *)
  let wrapped_bitwise_op bt op s1 s2 =
    if s1.is_bottom || s2.is_bottom then
      bottom bt
    else if is_top s1 || is_top s2 then
      top bt
    else (
      (* South pole split both operands *)
      let s1_splits = south_split bt s1.start s1.stop in
      let s2_splits = south_split bt s2.start s2.stop in
      let result = ref (bottom bt) in
      List.iter
        (fun s1' ->
           List.iter
             (fun s2' ->
                (* Compute bitwise operation bounds for this pair *)
                let lb, ub =
                  match op with
                  | IT.BW_Or ->
                    ( min_or bt s1'.start s1'.stop s2'.start s2'.stop,
                      max_or bt s1'.start s1'.stop s2'.start s2'.stop )
                  | IT.BW_And ->
                    ( min_and bt s1'.start s1'.stop s2'.start s2'.stop,
                      max_and bt s1'.start s1'.stop s2'.start s2'.stop )
                  | IT.BW_Xor ->
                    ( min_xor bt s1'.start s1'.stop s2'.start s2'.stop,
                      max_xor bt s1'.start s1'.stop s2'.start s2'.stop )
                  | _ -> failwith "unsupported bitwise operation"
                in
                let tmp = { bt; is_bottom = false; start = lb; stop = ub } in
                result := join !result tmp)
             s2_splits)
        s1_splits;
      !result)


  (* Shift operations with pole-crossing checks *)
  let wrapped_shift_op bt op operand shift =
    if operand.is_bottom || shift.is_bottom then
      bottom bt
    else if is_top operand || is_top shift then
      top bt
    else if Z.equal shift.start Z.zero && Z.equal shift.stop Z.zero then
      operand (* Shift by zero is no-op *)
    else if not (Z.equal shift.start shift.stop) then
      top bt (* Non-constant shift - be conservative *)
    else (
      let k = shift.start in
      let width = get_width bt in
      if Z.geq k (Z.of_int width) || Z.lt k Z.zero then
        top bt (* Shift by width or more, or negative shift - undefined/conservative *)
      else (
        let k_int = Z.to_int k in
        match op with
        | IT.ShiftLeft ->
          (* Left shift: check if lower (width-k) bits fit in interval *)
          let num_bits_survive_shift = width - k_int in
          if num_bits_survive_shift <= 0 then
            (* Shifting by width or more results in zero for unsigned *)
            { bt; is_bottom = false; start = Z.zero; stop = Z.zero }
          else (
            (* Use enhanced truncation to check if shift is safe *)
            let truncated = truncate_to_bits bt operand num_bits_survive_shift in
            if not (is_top truncated) then (
              (* Truncation succeeded - safe to perform precise shift *)
              let shifted_start = normalize bt (Z.shift_left operand.start k_int) in
              let shifted_stop = normalize bt (Z.shift_left operand.stop k_int) in
              { bt; is_bottom = false; start = shifted_start; stop = shifted_stop })
            else (
              (* Truncation failed - return conservative bounds *)
              let max_val = Z.sub (Z.shift_left Z.one num_bits_survive_shift) Z.one in
              let upper_bound = normalize bt (Z.shift_left max_val k_int) in
              { bt; is_bottom = false; start = Z.zero; stop = upper_bound }))
        | IT.ShiftRight ->
          (* Right shift algorithm - determine if logical or arithmetic based on type *)
          let is_signed =
            match BT.is_bits_bt bt with Some (Signed, _) -> true | _ -> false
          in
          let a = operand.start in
          let b = operand.stop in
          if is_signed then
            if is_top operand || cross_north_pole bt a b then (
              (* North pole crossing case - precise conservative bounds from Crab *)
              (* Lower bound: 1...10..0 where #1's = k and #0's = b-k *)
              (* Create k leading 1's followed by (width-k) 0's *)
              let remaining_bits = width - k_int in
              let leading_ones_mask =
                Z.shift_left (Z.sub (Z.shift_left Z.one k_int) Z.one) remaining_bits
              in
              let lb = normalize bt leading_ones_mask in
              (* Upper bound: 0..01..1 where #0's = k and #1's = b-k *)
              let ub =
                if remaining_bits > 0 then
                  Z.sub (Z.shift_left Z.one remaining_bits) Z.one (* 0..01..1 *)
                else
                  Z.zero
              in
              { bt; is_bottom = false; start = lb; stop = ub })
            else (
              (* No pole crossing - precise arithmetic shift *)
              let shifted_start =
                if Z.geq a Z.zero then
                  Z.shift_right a k_int
                else
                  Z.neg (Z.shift_right (Z.neg a) k_int)
              in
              let shifted_stop =
                if Z.geq b Z.zero then
                  Z.shift_right b k_int
                else
                  Z.neg (Z.shift_right (Z.neg b) k_int)
              in
              { bt; is_bottom = false; start = shifted_start; stop = shifted_stop })
          else if is_top operand || cross_south_pole a b then (
            (* South pole crossing case *)
            let lb = Z.zero in
            let remaining_bits = width - k_int in
            let ub =
              if remaining_bits > 0 then
                Z.sub (Z.shift_left Z.one remaining_bits) Z.one
              else
                Z.zero
            in
            { bt; is_bottom = false; start = lb; stop = ub })
          else (
            (* No pole crossing - precise shift *)
            let shifted_start = Z.shift_right a k_int in
            let shifted_stop = Z.shift_right b k_int in
            { bt; is_bottom = false; start = shifted_start; stop = shifted_stop })
        | _ -> failwith "unsupported shift operation"))


  let forward_abs_binop (op : IT.binop) (b1 : t) (b2 : t) : t option =
    assert (BT.equal b1.bt b2.bt);
    if b1.is_bottom || b2.is_bottom then
      Some (bottom b1.bt)
    else (
      match op with
      | Add ->
        let start = Z.add b1.start b2.start in
        let stop = Z.add b1.stop b2.stop in
        let card1 = cardinality b1 in
        let card2 = cardinality b2 in
        if is_wrapped_overflow_add_sub b1.bt card1 card2 then
          Some (top b1.bt)
        else
          Some { bt = b1.bt; is_bottom = false; start; stop }
      | Sub ->
        let start = Z.sub b1.start b2.stop in
        let stop = Z.sub b1.stop b2.start in
        let card1 = cardinality b1 in
        let card2 = cardinality b2 in
        if is_wrapped_overflow_add_sub b1.bt card1 card2 then
          Some (top b1.bt)
        else
          Some { bt = b1.bt; is_bottom = false; start; stop }
      | Mul -> Some (wrapped_multiplication b1.bt b1 b2)
      | Div | DivNoSMT ->
        let is_signed =
          match BT.is_bits_bt b1.bt with
          | Some (Signed, _) -> true
          | Some (Unsigned, _) -> false
          | None -> false
        in
        Some (wrapped_division b1.bt b1 b2 is_signed)
      | BW_And | BW_Or | BW_Xor -> Some (wrapped_bitwise_op b1.bt op b1 b2)
      | ShiftLeft | ShiftRight -> Some (wrapped_shift_op b1.bt op b1 b2)
      | Mod ->
        (* Remainder operation *)
        let is_signed =
          match BT.is_bits_bt b1.bt with
          | Some (Signed, _) -> true
          | Some (Unsigned, _) -> false
          | None -> false
        in
        Some (wrapped_remainder b1.bt b1 b2 is_signed)
      | _ -> None)


  (* Handle unary operations *)
  let forward_abs_unop (op : IT.unop) (b : t) : t option =
    match op with
    | BW_Compl ->
      if b.is_bottom then
        Some (bottom b.bt)
      else (
        (* Bitwise NOT: ~x = -x - 1 for two's complement *)
        let start = Z.neg (Z.add b.stop Z.one) in
        let stop = Z.neg (Z.add b.start Z.one) in
        Some { bt = b.bt; is_bottom = false; start; stop })
    | Negate ->
      if b.is_bottom then
        Some (bottom b.bt)
      else (
        (* Unary minus: -x *)
        let start = Z.neg b.stop in
        let stop = Z.neg b.start in
        (* Check for overflow in two's complement *)
        let width = get_width b.bt in
        let min_val = Z.neg (Z.shift_left Z.one (width - 1)) in
        if Z.equal b.start min_val then
          Some (top b.bt) (* -MIN_INT overflows *)
        else
          Some { bt = b.bt; is_bottom = false; start; stop })
    | _ -> None


  let forward_abs_it (it : IT.t) (b_args : t list) : t option =
    let (IT (it_, bt, _loc)) = it in
    match it_ with
    | Const (Bits (_, n)) -> Some (of_interval bt n n)
    | Binop (op, _, _) ->
      let b1, b2 =
        match b_args with
        | [ b1; b2 ] -> (b1, b2)
        | _ -> failwith "Incorrect number of arguments"
      in
      if not (BT.equal b1.bt b2.bt) then (
        print_endline
          Pp.(
            plain
              (IT.pp it
               ^^^ pp b1
               ^^ parens (BT.pp b1.bt)
               ^^^ pp b2
               ^^ parens (BT.pp b2.bt)));
        failwith __LOC__);
      forward_abs_binop op b1 b2
    | Unop (op, _) ->
      let b =
        match b_args with
        | [ b ] -> b
        | _ -> failwith "Incorrect number of arguments for unop"
      in
      forward_abs_unop op b
    | Cast (dst_bt, _) ->
      (* Casting between different bit widths *)
      let b =
        match b_args with
        | [ b ] -> b
        | _ -> failwith "Incorrect number of arguments for cast"
      in
      if b.is_bottom then
        Some (bottom dst_bt)
      else (
        let src_width = get_width b.bt in
        let dst_width = get_width dst_bt in
        if src_width = dst_width then (* Same width - just change type *)
          Some { bt = dst_bt; is_bottom = false; start = b.start; stop = b.stop }
        else if src_width > dst_width then (
          (* Truncation *)
          let modulus = Z.shift_left Z.one dst_width in
          let mask = Z.sub modulus Z.one in
          let start = Z.logand b.start mask in
          let stop = Z.logand b.stop mask in
          (* Check if truncation causes wrapping *)
          let card = cardinality b in
          if Z.geq card modulus then
            Some (top dst_bt) (* Truncation causes full range *)
          else
            Some { bt = dst_bt; is_bottom = false; start; stop })
        else (
          (* Extension - depends on signed/unsigned *)
          let is_src_signed =
            match BT.is_bits_bt b.bt with Some (Signed, _) -> true | _ -> false
          in
          if is_src_signed then (
            (* Sign extension *)
            let sign_bit = Z.shift_left Z.one (src_width - 1) in
            let extend_start =
              if Z.equal (Z.logand b.start sign_bit) sign_bit then
                Z.logor b.start (Z.shift_left (Z.sub Z.zero Z.one) src_width)
              else
                b.start
            in
            let extend_stop =
              if Z.equal (Z.logand b.stop sign_bit) sign_bit then
                Z.logor b.stop (Z.shift_left (Z.sub Z.zero Z.one) src_width)
              else
                b.stop
            in
            Some
              { bt = dst_bt; is_bottom = false; start = extend_start; stop = extend_stop })
          else (* Zero extension *)
            Some { bt = dst_bt; is_bottom = false; start = b.start; stop = b.stop }))
    | _ -> None


  (* Helper: check if an interval is a constant (singleton) *)
  let is_constant_range s = Z.equal s.start s.stop

  let rec backward_abs_it (it : IT.t) (bs : t list) =
    let (IT (it_, _, loc)) = it in
    match it_ with
    (* Handle inequality via negation of equality *)
    | Unop (Not, IT (Binop (EQ, it', _), _, _)) ->
      let bt = IT.get_bt it' in
      if Option.is_none (BT.is_bits_bt bt) && not (BT.equal bt (BT.Loc ())) then
        bs
      else (
        let b1, b2 = match bs with [ b1; b2 ] -> (b1, b2) | _ -> failwith __LOC__ in
        (* For inequality (not equal), refine by removing the intersection *)
        if is_constant_range b2 then (
          (* If b2 is a constant, remove it from b1 if it's at boundary *)
          let const_val = b2.start in
          let refined_b1 =
            if is_constant_range b1 && Z.equal b1.start const_val then
              bottom bt (* Point intervals equal - contradiction *)
            else if Z.equal b1.start const_val then
              { bt; is_bottom = false; start = Z.add b1.start Z.one; stop = b1.stop }
            else if Z.equal b1.stop const_val then
              { bt; is_bottom = false; start = b1.start; stop = Z.sub b1.stop Z.one }
            else
              b1 (* Constant not at boundary *)
          in
          [ refined_b1; b2 ])
        else if is_constant_range b1 then (
          (* Symmetric case *)
          let const_val = b1.start in
          let refined_b2 =
            if Z.equal b2.start const_val then
              { bt; is_bottom = false; start = Z.add b2.start Z.one; stop = b2.stop }
            else if Z.equal b2.stop const_val then
              { bt; is_bottom = false; start = b2.start; stop = Z.sub b2.stop Z.one }
            else
              b2
          in
          [ b1; refined_b2 ])
        else (* Both are intervals - conservative: no refinement *)
          bs)
    | Binop (EQ, it', _) ->
      let bt = IT.get_bt it' in
      if Option.is_none (BT.is_bits_bt bt) && not (BT.equal bt (BT.Loc ())) then
        bs
      else (
        let b1, b2 = match bs with [ b1; b2 ] -> (b1, b2) | _ -> failwith __LOC__ in
        let b = meet b1 b2 in
        [ b; b ])
    | Binop (LE, it', _) | Binop (LEPointer, it', _) ->
      let bt = IT.get_bt it' in
      let min, max = get_extrema bt in
      let b1, b2 = match bs with [ b1; b2 ] -> (b1, b2) | _ -> failwith __LOC__ in
      let b1' = of_interval bt min b2.stop in
      let b2' = of_interval bt b1.start max in
      [ meet b1 b1'; meet b2 b2' ]
    | Binop (LT, it', _) | Binop (LTPointer, it', _) ->
      let bt = IT.get_bt it' in
      let min, max = get_extrema bt in
      let b1, b2 = match bs with [ b1; b2 ] -> (b1, b2) | _ -> failwith __LOC__ in
      let b1' = of_interval bt min (Z.sub b2.stop Z.one) in
      let b2' = of_interval bt (Z.add b1.start Z.one) max in
      [ meet b1 b1'; meet b2 b2' ]
    | Unop (Not, IT (Binop (LE, it1, it2), _, _))
    | Unop (Not, IT (Binop (LEPointer, it1, it2), _, _)) ->
      backward_abs_it (IT.le_ (it2, it1) loc) bs
    | Unop (Not, IT (Binop (LT, it1, it2), _, _))
    | Unop (Not, IT (Binop (LTPointer, it1, it2), _, _)) ->
      backward_abs_it (IT.lt_ (it2, it1) loc) bs
    | _ ->
      if BT.equal BT.Bool (IT.get_bt it) then
        bs
      else
        List.tl bs


  let widen _ = failwith __LOC__

  let narrow _ = failwith __LOC__

  let pp_params () = "ty wint_start, ty wint_stop"

  let pp_sym_args () = "wint_start, wint_stop"

  let pp_args { bt; is_bottom; start; stop } =
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
    let mk_ail_const value =
      Fulminate.Utils.mk_expr
        (let k a = A.(AilEconst (ConstantInteger (IConstant (a, Decimal, suffix)))) in
         if Z.equal value z_min && BT.equal_sign sign BT.Signed then
           A.(
             AilEbinary
               ( Fulminate.Utils.mk_expr (k (Z.neg (Z.sub (Z.neg value) Z.one))),
                 Arithmetic Sub,
                 Fulminate.Utils.mk_expr (k Z.one) ))
         else
           k value)
    in
    let open Pp in
    plain
      (CF.Pp_ail.pp_expression (mk_ail_const start)
       ^^ comma
       ^^ space
       ^^ CF.Pp_ail.pp_expression (mk_ail_const stop))


  let definitions () = Pp.empty

  let to_it (sym : Sym.t) (t : t) : IT.t =
    let loc = Locations.other __LOC__ in
    if is_bottom t then
      IT.bool_ false loc
    else if is_top t then
      IT.bool_ true loc
    else (
      let sym_it = IT.sym_ (sym, t.bt, loc) in
      let start_it = IT.num_lit_ t.start t.bt loc in
      let stop_it = IT.num_lit_ t.stop t.bt loc in
      if Z.leq t.start t.stop then (* Normal interval: start <= X && X <= stop *)
        IT.and_ [ IT.le_ (start_it, sym_it) loc; IT.le_ (sym_it, stop_it) loc ] loc
      else (* Wrapped interval [start..max] ∪ [min..stop]: start <= X || X <= stop *)
        IT.or_ [ IT.le_ (start_it, sym_it) loc; IT.le_ (sym_it, stop_it) loc ] loc)
end

module Inner : Domain.T = NonRelational.Make (WrappedIntervalBasis)
