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


let abstract_lc (vars : Sym.t list) (lc : LC.t) : ((Sym.t * BT.t) * domain) option =
  if not (TestGenConfig.is_experimental_runtime ()) then
    None
  else (
    match lc with
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
          ( (x2, bt),
            { default_domain with lower_bound_inc = Some it1; upper_bound_inc = Some it1 }
          )
      else
        Some
          ( (x1, bt),
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
        Some ((x2, bt), { default_domain with lower_bound_inc = Some it1 })
      else
        Some ((x1, bt), { default_domain with upper_bound_inc = Some it2 })
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
        Some ((x2, bt), { default_domain with lower_bound_ex = Some it1 })
      else
        Some ((x1, bt), { default_domain with upper_bound_ex = Some it2 })
    | LC.T (IT (Binop (EQ, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( (x, bt),
          { default_domain with lower_bound_inc = Some it; upper_bound_inc = Some it } )
    | LC.T (IT (Binop (LE, IT (Sym x, bt, _), it), _, _))
    | LC.T (IT (Binop (LEPointer, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some ((x, bt), { default_domain with upper_bound_inc = Some it })
    | LC.T (IT (Binop (LT, IT (Sym x, bt, _), it), _, _))
    | LC.T (IT (Binop (LTPointer, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some ((x, bt), { default_domain with upper_bound_ex = Some it })
    | LC.T (IT (Binop (LE, it, IT (Sym x, bt, _)), _, _))
    | LC.T (IT (Binop (LEPointer, it, IT (Sym x, bt, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some ((x, bt), { default_domain with lower_bound_inc = Some it })
    | LC.T (IT (Binop (LT, it, IT (Sym x, bt, _)), _, _))
    | LC.T (IT (Binop (LTPointer, it, IT (Sym x, bt, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some ((x, bt), { default_domain with lower_bound_ex = Some it })
    (* Cast *)
    | LC.T (IT (Binop (LE, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
    | LC.T
        (IT (Binop (LEPointer, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ((x, bt), { default_domain with cast = Some cast_bt; upper_bound_inc = Some it })
    | LC.T (IT (Binop (LT, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
    | LC.T
        (IT (Binop (LTPointer, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some ((x, bt), { default_domain with cast = Some cast_bt; upper_bound_ex = Some it })
    | LC.T (IT (Binop (LE, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
    | LC.T
        (IT (Binop (LEPointer, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ((x, bt), { default_domain with cast = Some cast_bt; lower_bound_inc = Some it })
    | LC.T (IT (Binop (LT, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
    | LC.T
        (IT (Binop (LTPointer, it, IT (Cast (cast_bt, IT (Sym x, bt, _)), _, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some ((x, bt), { default_domain with cast = Some cast_bt; lower_bound_ex = Some it })
    | _ -> None)
