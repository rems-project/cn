module CF = Cerb_frontend
module A = CF.AilSyntax
module BT = BaseTypes
module IT = IndexTerms

module IntervalBasis = struct
  let name = "interval"

  let c_name = "wint"

  type t =
    { bt : BT.t;
      is_bottom : bool;
      start : Z.t;
      stop : Z.t
    }
  [@@deriving eq, ord]

  let supported (bt : BT.t) = match bt with Bits _ | Loc _ -> true | _ -> false

  let bt { bt; _ } = bt

  let bottom bt = { bt; is_bottom = true; start = Z.zero; stop = Z.zero }

  let is_bottom { is_bottom; _ } = is_bottom

  let get_extrema bt =
    let bt =
      match bt with
      | BT.Bits _ -> bt
      | Loc () -> Memory.uintptr_bt
      | _ -> failwith ("invalid type: " ^ Pp.plain (BT.pp bt) ^ " @ " ^ __LOC__)
    in
    BT.bits_range (Option.get (BT.is_bits_bt bt))


  let top bt =
    let start, stop = get_extrema bt in
    { bt; is_bottom = false; start; stop }


  let is_top { bt; is_bottom; start; stop; _ } =
    if is_bottom then
      false
    else (
      let min, max = get_extrema bt in
      start == min && stop == max)


  let leq
        { bt = bt1; is_bottom = b1; start = start1; stop = stop1 }
        { bt = bt2; is_bottom = b2; start = start2; stop = stop2 }
    =
    assert (BT.equal bt1 bt2);
    if b1 then
      true
    else if b2 then
      false
    else
      Z.leq start2 start1 && Z.leq stop1 stop2


  let join
        { bt = bt1; is_bottom = b1; start = start1; stop = stop1 }
        { bt = bt2; is_bottom = b2; start = start2; stop = stop2 }
    =
    assert (BT.equal bt1 bt2);
    { bt = bt1;
      is_bottom = b1 && b2;
      start = Z.min start1 start2;
      stop = Z.max stop1 stop2
    }


  let meet
        { bt = bt1; is_bottom = b1; start = start1; stop = stop1 }
        { bt = bt2; is_bottom = b2; start = start2; stop = stop2 }
    =
    assert (BT.equal bt1 bt2);
    let start = Z.max start1 start2 in
    let stop = Z.min stop1 stop2 in
    { bt = bt1; is_bottom = b1 || b2 || Z.lt stop start; start; stop }


  let join_many intervals =
    match intervals with
    | [] -> failwith "join_many requires a non-empty list"
    | h :: t -> List.fold_left join h t


  let meet_many intervals =
    match intervals with
    | [] -> failwith "meet_many requires a non-empty list"
    | h :: t -> List.fold_left meet h t


  let handle_overflow bt start stop =
    let min, max = get_extrema bt in
    let stop = if Z.lt start min then max else stop in
    let start = if Z.lt max stop then min else start in
    (start, stop)


  let of_interval bt start stop =
    let is_bottom = Z.lt stop start in
    let start, stop = handle_overflow bt start stop in
    { bt; is_bottom; start; stop }


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


  let forward_abs_binop (op : IT.binop) (b1 : t) (b2 : t) : t option =
    assert (BT.equal b1.bt b2.bt);
    if b1.is_bottom || b2.is_bottom then
      Some (bottom b1.bt)
    else (
      match op with
      | Add ->
        let start = Z.add b1.start b2.start in
        let stop = Z.add b1.stop b2.stop in
        let start, stop = handle_overflow b1.bt start stop in
        Some { bt = b1.bt; is_bottom = false; start; stop }
      | Sub ->
        let start = Z.sub b1.start b2.stop in
        let stop = Z.sub b1.stop b2.start in
        let start, stop = handle_overflow b1.bt start stop in
        Some { bt = b1.bt; is_bottom = false; start; stop }
      | Mul ->
        let mults =
          [ Z.mul b1.start b2.start;
            Z.mul b1.start b2.stop;
            Z.mul b1.stop b2.start;
            Z.mul b1.stop b2.stop
          ]
        in
        let start = List.fold_left Z.min (List.hd mults) (List.tl mults) in
        let stop = List.fold_left Z.max (List.hd mults) (List.tl mults) in
        let start, stop = handle_overflow b1.bt start stop in
        Some { bt = b1.bt; is_bottom = false; start; stop }
      | Div ->
        (* Division - handle division by zero *)
        if Z.equal b2.start Z.zero && Z.equal b2.stop Z.zero then
          (* Division by zero interval [0,0] -> bottom *)
          Some (bottom b1.bt)
        else if Z.equal b1.start Z.zero && Z.equal b1.stop Z.zero then
          (* Zero divided by anything (non-zero) -> zero *)
          Some { bt = b1.bt; is_bottom = false; start = Z.zero; stop = Z.zero }
        else if Z.leq b2.start Z.zero && Z.geq b2.stop Z.zero then
          (* Divisor contains zero but is not exactly [0,0] -> conservative: top *)
          Some (top b1.bt)
        else (
          (* Safe division - compute all combinations *)
          let divs =
            [ Z.div b1.start b2.start;
              Z.div b1.start b2.stop;
              Z.div b1.stop b2.start;
              Z.div b1.stop b2.stop
            ]
          in
          let start = List.fold_left Z.min (List.hd divs) (List.tl divs) in
          let stop = List.fold_left Z.max (List.hd divs) (List.tl divs) in
          let start, stop = handle_overflow b1.bt start stop in
          Some { bt = b1.bt; is_bottom = false; start; stop })
      | Mod ->
        (* Modulo/remainder operation *)
        if Z.equal b2.start Z.zero && Z.equal b2.stop Z.zero then
          (* Modulo by zero -> bottom *)
          Some (bottom b1.bt)
        else if Z.equal b1.start Z.zero && Z.equal b1.stop Z.zero then
          (* Zero modulo anything (non-zero) -> zero *)
          Some { bt = b1.bt; is_bottom = false; start = Z.zero; stop = Z.zero }
        else if Z.leq b2.start Z.zero && Z.geq b2.stop Z.zero then
          (* Divisor contains zero but is not exactly [0,0] -> conservative: top *)
          Some (top b1.bt)
        else (
          (* For modulo, result is bounded by |divisor| - 1 *)
          let max_divisor = Z.max (Z.abs b2.start) (Z.abs b2.stop) in
          let bound = Z.sub max_divisor Z.one in
          let start = Z.neg bound in
          let stop = bound in
          let start, stop = handle_overflow b1.bt start stop in
          Some { bt = b1.bt; is_bottom = false; start; stop })
      | BW_And ->
        (* Bitwise AND - conservative approximation *)
        (* For AND, result is bounded by min of the operands *)
        let start = Z.min (Z.min b1.start b2.start) Z.zero in
        let stop = Z.min b1.stop b2.stop in
        let start, stop = handle_overflow b1.bt start stop in
        Some { bt = b1.bt; is_bottom = false; start; stop }
      | BW_Or ->
        (* Bitwise OR - conservative approximation *)
        (* For OR, result is bounded by max of the operands *)
        let start = Z.max b1.start b2.start in
        let stop = Z.max b1.stop b2.stop in
        let start, stop = handle_overflow b1.bt start stop in
        Some { bt = b1.bt; is_bottom = false; start; stop }
      | BW_Xor ->
        (* Bitwise XOR - very conservative approximation *)
        (* XOR can produce any value between 0 and max of operands *)
        let max_val =
          Z.max
            (Z.max (Z.abs b1.start) (Z.abs b1.stop))
            (Z.max (Z.abs b2.start) (Z.abs b2.stop))
        in
        let start = Z.neg max_val in
        let stop = max_val in
        let start, stop = handle_overflow b1.bt start stop in
        Some { bt = b1.bt; is_bottom = false; start; stop }
      | ShiftLeft ->
        (* Left shift: x << y *)
        if Z.lt b2.start Z.zero then (* Negative shift -> top *)
          Some (top b1.bt)
        else if Z.equal b1.start Z.zero && Z.equal b1.stop Z.zero then
          (* 0 << anything = 0 *)
          Some { bt = b1.bt; is_bottom = false; start = Z.zero; stop = Z.zero }
        else (
          (* Conservative: multiply by 2^shift_amount *)
          let min_shift = Z.max b2.start Z.zero in
          let max_shift = Z.max b2.stop Z.zero in
          let min_mult = Z.pow (Z.of_int 2) (Z.to_int min_shift) in
          let max_mult = Z.pow (Z.of_int 2) (Z.to_int max_shift) in
          let shifts =
            [ Z.mul b1.start min_mult;
              Z.mul b1.start max_mult;
              Z.mul b1.stop min_mult;
              Z.mul b1.stop max_mult
            ]
          in
          let start = List.fold_left Z.min (List.hd shifts) (List.tl shifts) in
          let stop = List.fold_left Z.max (List.hd shifts) (List.tl shifts) in
          let start, stop = handle_overflow b1.bt start stop in
          Some { bt = b1.bt; is_bottom = false; start; stop })
      | ShiftRight ->
        (* Right shift: x >> y *)
        if Z.lt b2.start Z.zero then (* Negative shift -> top *)
          Some (top b1.bt)
        else if Z.equal b1.start Z.zero && Z.equal b1.stop Z.zero then
          (* 0 >> anything = 0 *)
          Some { bt = b1.bt; is_bottom = false; start = Z.zero; stop = Z.zero }
        else (
          (* Conservative: divide by 2^shift_amount *)
          let min_shift = Z.max b2.start Z.zero in
          let max_shift = Z.max b2.stop Z.zero in
          let min_div = Z.pow (Z.of_int 2) (Z.to_int min_shift) in
          let max_div = Z.pow (Z.of_int 2) (Z.to_int max_shift) in
          let shifts =
            [ Z.div b1.start min_div;
              Z.div b1.start max_div;
              Z.div b1.stop min_div;
              Z.div b1.stop max_div
            ]
          in
          let start = List.fold_left Z.min (List.hd shifts) (List.tl shifts) in
          let stop = List.fold_left Z.max (List.hd shifts) (List.tl shifts) in
          let start, stop = handle_overflow b1.bt start stop in
          Some { bt = b1.bt; is_bottom = false; start; stop })
      | _ -> None)


  let cast (interval : t) (target_bt : BT.t) : t =
    if is_bottom interval then
      bottom target_bt
    else if not (supported target_bt) then
      top target_bt
    else (
      let target_min, target_max = get_extrema target_bt in
      let clamped_start = Z.max target_min (Z.min target_max interval.start) in
      let clamped_stop = Z.max target_min (Z.min target_max interval.stop) in
      (* Check for overflow: if the original interval extends beyond target range *)
      let has_overflow =
        Z.lt interval.start target_min || Z.gt interval.stop target_max
      in
      if has_overflow then
        top target_bt
      else
        { bt = target_bt; is_bottom = false; start = clamped_start; stop = clamped_stop })


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
    | Cast (_, _) ->
      let source_interval =
        match b_args with
        | [ interval ] -> interval
        | _ -> failwith "Cast requires exactly one argument"
      in
      Some (cast source_interval bt)
    | _ -> None


  let rec backward_abs_it (it : IT.t) (bs : t list) =
    let (IT (it_, _, loc)) = it in
    match it_ with
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
    (* TODO: NE *)
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
      let bt', sym_it =
        match t.bt with
        | BT.Loc () ->
          (* Cast pointer to uintptr_t and compare as integers *)
          (Memory.uintptr_bt, IT.cast_ Memory.uintptr_bt (IT.sym_ (sym, t.bt, loc)) loc)
        | _ -> (t.bt, IT.sym_ (sym, t.bt, loc))
      in
      let start_it = IT.num_lit_ t.start bt' loc in
      let stop_it = IT.num_lit_ t.stop bt' loc in
      IT.and_ [ IT.le_ (start_it, sym_it) loc; IT.le_ (sym_it, stop_it) loc ] loc)
end

module Inner : Domain.T = NonRelational.Make (IntervalBasis)
