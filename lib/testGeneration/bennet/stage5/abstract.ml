module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

type domain =
  { cast : BT.t option;
    lower_bound_inc : IT.t option;
    lower_bound_ex : IT.t option;
    upper_bound_inc : IT.t option;
    upper_bound_ex : IT.t option;
    multiple : IT.t option
  }
[@@deriving eq, ord]

let default_domain =
  { cast = None;
    lower_bound_inc = None;
    lower_bound_ex = None;
    upper_bound_inc = None;
    upper_bound_ex = None;
    multiple = None
  }


let abstract_lc (vars : Sym.t list) (lc : LC.t) : (bool * (Sym.t * BT.t) * domain) option =
  if not (TestGenConfig.is_experimental_runtime ()) then
    None
  else (
    match lc with
    (* Two symbols *)
    | LC.T
        (IT (Binop (EQ, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)), _, _))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some
          ( true,
            (x2, bt),
            { default_domain with lower_bound_inc = Some it1; upper_bound_inc = Some it1 }
          )
      else
        Some
          ( true,
            (x1, bt),
            { default_domain with lower_bound_inc = Some it2; upper_bound_inc = Some it2 }
          )
    | LC.T
        (IT (Binop (LE, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)), _, _))
    | LC.T
        (IT
           ( Binop (LEPointer, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some (true, (x2, bt), { default_domain with lower_bound_inc = Some it1 })
      else
        Some (true, (x1, bt), { default_domain with upper_bound_inc = Some it2 })
    | LC.T
        (IT (Binop (LT, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)), _, _))
    | LC.T
        (IT
           ( Binop (LTPointer, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some (true, (x2, bt), { default_domain with lower_bound_ex = Some it1 })
      else
        Some (true, (x1, bt), { default_domain with upper_bound_ex = Some it2 })
    (* Two symbols with cast *)
    | LC.T
        (IT
           ( Binop
               ( EQ,
                 (IT (Cast (cast_bt1, IT (Sym x1, bt, _)), _, _) as it1),
                 (IT (Sym x2, _, _) as it2) ),
             _,
             _ ))
    | LC.T
        (IT
           ( Binop
               ( EQ,
                 (IT (Sym x2, _, _) as it2),
                 (IT (Cast (cast_bt1, IT (Sym x1, bt, _)), _, _) as it1) ),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some
          ( true,
            (x2, cast_bt1),
            { default_domain with lower_bound_inc = Some it1; upper_bound_inc = Some it1 }
          )
      else
        Some
          ( true,
            (x1, bt),
            { default_domain with
              cast = Some cast_bt1;
              lower_bound_inc = Some it2;
              upper_bound_inc = Some it2
            } )
    | LC.T
        (IT
           ( Binop
               ( LE,
                 (IT (Cast (cast_bt1, IT (Sym x1, bt, _)), _, _) as it1),
                 (IT (Sym x2, _, _) as it2) ),
             _,
             _ ))
    | LC.T
        (IT
           ( Binop
               ( LEPointer,
                 (IT (Cast (cast_bt1, IT (Sym x1, bt, _)), _, _) as it1),
                 (IT (Sym x2, _, _) as it2) ),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some (true, (x2, cast_bt1), { default_domain with lower_bound_inc = Some it1 })
      else
        Some
          ( true,
            (x1, bt),
            { default_domain with cast = Some cast_bt1; upper_bound_inc = Some it2 } )
    | LC.T
        (IT
           ( Binop
               ( LT,
                 (IT (Cast (cast_bt1, IT (Sym x1, bt, _)), _, _) as it1),
                 (IT (Sym x2, _, _) as it2) ),
             _,
             _ ))
    | LC.T
        (IT
           ( Binop
               ( LTPointer,
                 (IT (Cast (cast_bt1, IT (Sym x1, bt, _)), _, _) as it1),
                 (IT (Sym x2, _, _) as it2) ),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some (true, (x2, cast_bt1), { default_domain with lower_bound_ex = Some it1 })
      else
        Some
          ( true,
            (x1, bt),
            { default_domain with cast = Some cast_bt1; upper_bound_ex = Some it2 } )
    | LC.T
        (IT
           ( Binop
               ( LT,
                 (IT (Sym x1, _, _) as it1),
                 (IT (Cast (cast_bt2, IT (Sym x2, bt, _)), _, _) as it2) ),
             _,
             _ ))
    | LC.T
        (IT
           ( Binop
               ( LTPointer,
                 (IT (Sym x1, _, _) as it1),
                 (IT (Cast (cast_bt2, IT (Sym x2, bt, _)), _, _) as it2) ),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some
          ( true,
            (x2, bt),
            { default_domain with cast = Some cast_bt2; lower_bound_ex = Some it1 } )
      else
        Some (true, (x1, cast_bt2), { default_domain with upper_bound_ex = Some it2 })
    | LC.T
        (IT
           ( Binop
               ( LE,
                 (IT (Sym x1, _, _) as it1),
                 (IT (Cast (cast_bt2, IT (Sym x2, bt, _)), _, _) as it2) ),
             _,
             _ ))
    | LC.T
        (IT
           ( Binop
               ( LEPointer,
                 (IT (Sym x1, _, _) as it1),
                 (IT (Cast (cast_bt2, IT (Sym x2, bt, _)), _, _) as it2) ),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if
        Option.equal
          Sym.equal
          (List.find_opt (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars)
          (Some x2)
      then
        Some
          ( true,
            (x2, bt),
            { default_domain with cast = Some cast_bt2; lower_bound_inc = Some it1 } )
      else
        Some (true, (x1, cast_bt2), { default_domain with upper_bound_inc = Some it2 })
    (* General *)
    | LC.T (IT (Binop (EQ, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( true,
          (x, bt),
          { default_domain with lower_bound_inc = Some it; upper_bound_inc = Some it } )
    | LC.T (IT (Binop (LE, IT (Sym x, bt, _), it), _, _))
    | LC.T (IT (Binop (LEPointer, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some (true, (x, bt), { default_domain with upper_bound_inc = Some it })
    | LC.T (IT (Binop (LT, IT (Sym x, bt, _), it), _, _))
    | LC.T (IT (Binop (LTPointer, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some (true, (x, bt), { default_domain with upper_bound_ex = Some it })
    | LC.T (IT (Binop (LE, it, IT (Sym x, bt, _)), _, _))
    | LC.T (IT (Binop (LEPointer, it, IT (Sym x, bt, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some (true, (x, bt), { default_domain with lower_bound_inc = Some it })
    | LC.T (IT (Binop (LT, it, IT (Sym x, bt, _)), _, _))
    | LC.T (IT (Binop (LTPointer, it, IT (Sym x, bt, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some (true, (x, bt), { default_domain with lower_bound_ex = Some it })
    (* Cast *)
    | LC.T (IT (Binop (LE, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
    | LC.T
        (IT (Binop (LEPointer, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( true,
          (x, bt),
          { default_domain with cast = Some cast_bt; upper_bound_inc = Some it } )
    | LC.T (IT (Binop (LT, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
    | LC.T
        (IT (Binop (LTPointer, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( true,
          (x, bt),
          { default_domain with cast = Some cast_bt; upper_bound_ex = Some it } )
    | LC.T (IT (Binop (LE, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
    | LC.T
        (IT (Binop (LEPointer, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( true,
          (x, bt),
          { default_domain with cast = Some cast_bt; lower_bound_inc = Some it } )
    | LC.T (IT (Binop (LT, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
    | LC.T
        (IT (Binop (LTPointer, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( true,
          (x, bt),
          { default_domain with cast = Some cast_bt; lower_bound_ex = Some it } )
    (* Overflow *)
    | LC.T
        (IT
           ( Binop
               ( LT,
                 IT (Sym x1, _, _),
                 IT (Binop (Add, IT (Sym x2, _, _), IT (Sym y, y_bt, _)), _, _) ),
             _,
             loc ))
      when Sym.equal x1 x2 ->
      Some
        ( false,
          (y, y_bt),
          { default_domain with lower_bound_inc = Some (IT.num_lit_ Z.one y_bt loc) } )
    | LC.T
        (IT
           ( Binop
               ( LT,
                 IT (Sym x1, _, _),
                 IT (Binop (Add, IT (Sym y, y_bt, _), IT (Sym x2, _, _)), _, _) ),
             _,
             loc ))
      when Sym.equal x1 x2 ->
      Some
        ( false,
          (y, y_bt),
          { default_domain with lower_bound_inc = Some (IT.num_lit_ Z.one y_bt loc) } )
    | LC.T
        (IT
           ( Binop
               ( LT,
                 IT (Sym x1, _, _),
                 IT
                   ( Binop
                       ( Add,
                         IT (Sym x2, _, _),
                         IT (Cast (cast_bt, IT (Sym y, y_bt, _)), _, _) ),
                     _,
                     _ ) ),
             _,
             loc ))
      when Sym.equal x1 x2
           && Option.equal
                Bool.equal
                (let open Option in
                 let@ sign1, bits1 =
                   BT.is_bits_bt y_bt
                   |> map some
                   |> value ~default:(BT.is_bits_bt Memory.uintptr_bt)
                 in
                 let@ sign2, bits2 =
                   BT.is_bits_bt cast_bt
                   |> map some
                   |> value ~default:(BT.is_bits_bt Memory.uintptr_bt)
                 in
                 let f = BT.fits_range (sign2, bits2) in
                 let min, max = BT.bits_range (sign1, bits1) in
                 return (f min && f max))
                (Some true) ->
      Some
        ( false,
          (y, y_bt),
          { default_domain with lower_bound_inc = Some (IT.num_lit_ Z.one y_bt loc) } )
    | LC.T
        (IT
           ( Binop
               ( LT,
                 IT (Sym x1, _, _),
                 IT
                   ( Binop
                       ( Add,
                         IT (Cast (cast_bt, IT (Sym y, y_bt, _)), _, _),
                         IT (Sym x2, _, _) ),
                     _,
                     _ ) ),
             _,
             loc ))
      when Sym.equal x1 x2
           && Option.equal
                Bool.equal
                (let open Option in
                 let@ sign1, bits1 =
                   BT.is_bits_bt y_bt
                   |> map some
                   |> value ~default:(BT.is_bits_bt Memory.uintptr_bt)
                 in
                 let@ sign2, bits2 =
                   BT.is_bits_bt cast_bt
                   |> map some
                   |> value ~default:(BT.is_bits_bt Memory.uintptr_bt)
                 in
                 let f = BT.fits_range (sign2, bits2) in
                 let min, max = BT.bits_range (sign1, bits1) in
                 return (f min && f max))
                (Some true) ->
      Some
        ( false,
          (y, y_bt),
          { default_domain with lower_bound_inc = Some (IT.num_lit_ Z.one y_bt loc) } )
    | _ -> None)
