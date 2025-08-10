module BT = BaseTypes
module IT = IndexTerms

module Interval : NonRelational.BASIS = struct
  let name = "int"

  type t =
    { bt : BT.t;
      is_bottom : bool;
      start : Z.t;
      stop : Z.t
    }
  [@@deriving eq, ord]

  let bt { bt; _ } = bt

  let bottom bt = { bt; is_bottom = true; start = Z.zero; stop = Z.zero }

  let get_extrema bt =
    let bt =
      match bt with
      | BT.Bits _ -> bt
      | Loc () -> Memory.uintptr_bt
      | _ -> failwith __LOC__
    in
    BT.bits_range (Option.get (BT.is_bits_bt bt))


  let top bt =
    let start, stop = get_extrema bt in
    { bt; is_bottom = false; start; stop }


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


  let forward_abs_binop (op : IT.binop) (b1 : t) (b2 : t) : t =
    assert (BT.equal b1.bt b2.bt);
    if b1.is_bottom || b2.is_bottom then
      bottom b1.bt
    else (
      match op with
      | Add ->
        let start = Z.add b1.start b2.start in
        let stop = Z.add b1.stop b2.stop in
        let start, stop = handle_overflow b1.bt start stop in
        { bt = b1.bt; is_bottom = false; start; stop }
      | Sub ->
        let start = Z.sub b1.start b2.stop in
        let stop = Z.sub b1.stop b2.start in
        let start, stop = handle_overflow b1.bt start stop in
        { bt = b1.bt; is_bottom = false; start; stop }
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
        { bt = b1.bt; is_bottom = false; start; stop }
      | _ -> top b1.bt)


  let forward_abs_it (it : IT.t) (b_args : t list) =
    let (IT (it_, bt, _loc)) = it in
    match it_ with
    | Const (Bits (_, n)) -> of_interval bt n n
    | Binop (op, _, _) ->
      let b1, b2 =
        match b_args with
        | [ b1; b2 ] -> (b1, b2)
        | _ -> failwith "Incorrect number of arguments"
      in
      forward_abs_binop op b1 b2
    | _ -> top bt


  let backward_abs_it _ ds = List.tl ds

  let widen = failwith __LOC__

  let narrow = failwith __LOC__

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


  let pp_c = failwith __LOC__
end

module Inner : Domain.T = NonRelational.Make (Interval)
