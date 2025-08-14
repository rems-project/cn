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


  let handle_overflow bt start stop =
    let min, max = get_extrema bt in
    let stop = if Z.lt start min then max else stop in
    let start = if Z.lt max stop then min else start in
    (start, stop)


  let of_interval bt start stop =
    let is_bottom = Z.lt stop start in
    let start, stop = handle_overflow bt start stop in
    { bt; is_bottom; start; stop }


  let pp ({ bt; is_bottom; start; stop } as b) =
    let open Pp in
    if is_bottom then
      !^"⊥"
    else if equal b (top bt) then
      !^"⊤"
    else if Z.equal start stop then
      !^(Z.to_string start)
    else
      brackets (!^(Z.to_string start) ^^ !^".." ^^ !^(Z.to_string stop))


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
      | _ -> None)


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
    let const v =
      let macro =
        match bt with
        | Loc () -> "UINTMAX_C"
        | Bits (Signed, sz) -> Printf.sprintf "INT%d_C" sz
        | Bits (Unsigned, sz) -> Printf.sprintf "UINT%d_C" sz
        | _ -> failwith ("unsupported type: " ^ Pp.plain (BaseTypes.pp bt))
      in
      macro ^ Printf.sprintf "(%s)" v
    in
    Printf.sprintf "%s, %s" (const (Z.to_string start)) (const (Z.to_string stop))


  let definitions () = Pp.empty
end

module Inner : Domain.T = NonRelational.Make (IntervalBasis)
