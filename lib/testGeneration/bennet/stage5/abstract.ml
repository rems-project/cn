module BT = BaseTypes
module IT = IndexTerms
module LC = LogicalConstraints

type bound_type =
  | Inclusive
  | Exclusive
[@@deriving eq, ord]

type domain =
  { lower_bound : (bound_type * IT.t) option; (* Inclusive *)
    upper_bound : (bound_type * IT.t) option; (* Inclusive *)
    multiple : IT.t option
  }
[@@deriving eq, ord]

let abstract_lc (vars : Sym.t list) (lc : LC.t) : ((Sym.t * BT.t) * domain) option =
  if not (TestGenConfig.is_experimental_runtime ()) then
    None
  else (
    match lc with
    | LC.T
        (IT (Binop (LE, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)), _, _))
    | LC.T
        (IT
           ( Binop (LEPointer, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if Sym.equal (List.find (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars) x1 then
        Some
          ( (x1, bt),
            { lower_bound = None; upper_bound = Some (Inclusive, it2); multiple = None }
          )
      else
        Some
          ( (x2, bt),
            { lower_bound = Some (Exclusive, it1); upper_bound = None; multiple = None }
          )
    | LC.T
        (IT (Binop (LT, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)), _, _))
    | LC.T
        (IT
           ( Binop (LTPointer, (IT (Sym x1, bt, _) as it1), (IT (Sym x2, _, _) as it2)),
             _,
             _ ))
      when not (Sym.equal x1 x2) ->
      if Sym.equal (List.find (fun z -> Sym.equal x1 z || Sym.equal x2 z) vars) x1 then
        Some
          ( (x1, bt),
            { lower_bound = None; upper_bound = Some (Exclusive, it2); multiple = None }
          )
      else
        Some
          ( (x2, bt),
            { lower_bound = Some (Exclusive, it1); upper_bound = None; multiple = None }
          )
    | LC.T (IT (Binop (LE, IT (Sym x, bt, _), it), _, _))
    | LC.T (IT (Binop (LEPointer, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( (x, bt),
          { lower_bound = None; upper_bound = Some (Inclusive, it); multiple = None } )
    | LC.T (IT (Binop (LT, IT (Sym x, bt, _), it), _, _))
    | LC.T (IT (Binop (LTPointer, IT (Sym x, bt, _), it), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( (x, bt),
          { lower_bound = None; upper_bound = Some (Exclusive, it); multiple = None } )
    | LC.T (IT (Binop (LE, it, IT (Sym x, bt, _)), _, _))
    | LC.T (IT (Binop (LEPointer, it, IT (Sym x, bt, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( (x, bt),
          { lower_bound = Some (Inclusive, it); upper_bound = None; multiple = None } )
    | LC.T (IT (Binop (LT, it, IT (Sym x, bt, _)), _, _))
    | LC.T (IT (Binop (LTPointer, it, IT (Sym x, bt, _)), _, _))
      when not (Sym.Set.mem x (IT.free_vars it)) ->
      Some
        ( (x, bt),
          { lower_bound = Some (Exclusive, it); upper_bound = None; multiple = None } )
    | _ -> None)
