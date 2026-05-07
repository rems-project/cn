module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let simp = Simplify.IndexTerms.simp (Simplify.default Global.empty)

  let is_simp_true it = IT.is_true (simp it)

  let check_index_ok (m : Sym.t) (i : Sym.t) (it : IT.t) : bool =
    let rec aux (it : IT.t) : bool =
      let (IT (it_, _bt, _loc)) = it in
      match it_ with
      | MapGet (IT (Sym x, _, _), it_key) when Sym.equal m x ->
        (match IT.is_sym it_key with Some (j, _) -> Sym.equal i j | _ -> false)
      | Const _ | SizeOf _ | OffsetOf _ | Nil _ | CN_None _ -> true
      | Sym x -> not (Sym.equal x m)
      | Unop (_, it')
      | EachI ((_, _, _), it')
      | NthTuple (_, it')
      | StructMember (it', _)
      | RecordMember (it', _)
      | Cast (_, it')
      | MemberShift (it', _, _)
      | HasAllocId it'
      | Head it'
      | Tail it'
      | Representable (_, it')
      | Good (_, it')
      | WrapI (_, it')
      | MapConst (_, it')
      | MapDef (_, it')
      | CN_Some it'
      | IsSome it'
      | GetOpt it' ->
        aux it'
      | Binop (_, it1, it2)
      | StructUpdate ((it1, _), it2)
      | RecordUpdate ((it1, _), it2)
      | ArrayShift { base = it1; ct = _; index = it2 }
      | Cons (it1, it2)
      | CopyAllocId { addr = it1; loc = it2 }
      | Aligned { t = it1; align = it2 }
      | MapGet (it1, it2)
      | Let ((_, it1), it2) ->
        aux it1 && aux it2
      | ITE (it1, it2, it3) | MapSet (it1, it2, it3) -> aux it1 && aux it2 && aux it3
      | Tuple its | Apply (_, its) -> List.for_all aux its
      | Struct (_, xits) | Record xits | Constructor (_, xits) ->
        List.for_all aux (List.map snd xits)
      | Match (it, pits) -> aux it && List.for_all aux (List.map snd pits)
    in
    aux it


  let collect_constraints
        (vars : Sym.Set.t)
        (x : Sym.t)
        (new_i : Sym.t)
        ((it_min, it_max) : IT.t * IT.t)
        (gt : Term.t)
    : Term.t * IT.t
    =
    let it_true = IT.bool_ true (Locations.other __LOC__) in
    let it_and a b = IT.and2_ (a, b) (Locations.other __LOC__) in
    let rec aux (delete : bool) (gt : Term.t) : Term.t * IT.t =
      let (Annot (gt_, (), _, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `Call _ | `Return _ | `Map _ -> (gt, it_true)
      | `Pick gts ->
        let _, constraints =
          gts |> List.map (aux false) |> List.map_snd simp |> List.split
        in
        if List.exists IT.is_true constraints then
          (gt, it_true)
        else
          ( gt,
            List.fold_left
              (fun a b -> IT.or2_ (a, b) (Locations.other __LOC__))
              (List.hd constraints)
              (List.tl constraints) )
      | `Asgn ((it_addr, sct), it_val, gt') ->
        let gt', res = aux delete gt' in
        (Term.asgn_ ((it_addr, sct), it_val, gt') () loc, res)
      | `LetStar ((y, _), _) when Sym.equal x y -> (gt, it_true)
      | `LetStar ((y, gt_inner), gt_rest) ->
        let gt_inner, res = aux delete gt_inner in
        let gt_rest, res' = aux delete gt_rest in
        (Term.let_star_ ((y, gt_inner), gt_rest) () loc, it_and res res')
      | `Assert
          ( Forall
              ((i, i_bt), (IT (Binop (Implies, it_perm, it_body), _, loc_implies) as it)),
            gt' )
        when Sym.Set.mem x (IT.free_vars it) && check_index_ok x i it ->
        let it_min', it_max' = IndexTerms.Bounds.get_bounds (i, i_bt) it_perm in
        let gt', res = aux delete gt' in
        if
          IT.equal it_min it_min'
          && IT.equal it_max it_max'
          && Sym.Set.subset
               (Sym.Set.remove i (IT.free_vars_list [ it_perm; it_body ]))
               vars
        then (
          let res' =
            (it_and
               (IT.subst
                  (IT.make_rename ~from:i ~to_:new_i)
                  (IT.impl_ (it_perm, it_body) loc_implies)))
              res
          in
          if delete then
            (gt', res')
          else
            (Term.assert_ (Forall ((i, i_bt), it), gt') () loc, res'))
        else
          (Term.assert_ (Forall ((i, i_bt), it), gt') () loc, res)
      | `Assert (lc, gt') ->
        let gt', res = aux delete gt' in
        (Term.assert_ (lc, gt') () loc, res)
      | `ITE (it_if, gt_then, gt_else) ->
        let delete' = Sym.Set.subset (IT.free_vars it_if) vars in
        let gt_then', then_constraints = aux delete' gt_then in
        let gt_else', else_constraints = aux delete' gt_else in
        let gt' = Term.ite_ (it_if, gt_then', gt_else') () loc in
        (match (is_simp_true then_constraints, is_simp_true else_constraints) with
         | false, true when delete' -> (gt', IT.impl_ (it_if, then_constraints) loc)
         | true, false when delete' ->
           (gt', IT.impl_ (IT.not_ it_if loc, then_constraints) loc)
         | false, false when delete' ->
           (gt', IT.ite_ (it_if, then_constraints, else_constraints) loc)
         | _, _ -> (gt', IT.or2_ (then_constraints, else_constraints) loc))
    in
    aux true gt


  let replace_index (m : Sym.t) (i : Sym.t) (result : Sym.t) (it : IT.t) : IT.t =
    let aux (it : IT.t) : IT.t =
      let (IT (it_, bt, loc)) = it in
      match it_ with
      | MapGet (IT (Sym y, _, _), IT (Sym j, _, _)) when Sym.equal m y ->
        if not (Sym.equal i j) then
          failwith (Pp.plain (IT.pp it) ^ " @ " ^ __LOC__);
        IT.sym_ (result, bt, loc)
      | _ -> it
    in
    IT.map_term_pre aux it


  let transform_gt (vars : Sym.Set.t) (gt : Term.t) : Term.t =
    let rec aux (vars : Sym.Set.t) (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `Call _ | `Return _ -> gt
      | `Pick gts -> Term.pick_ (List.map (aux vars) gts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt') ->
        Term.asgn_ ((it_addr, sct), it_val, aux vars gt') () loc
      | `LetStar
          ((x, Annot (`Map ((i, i_bt, it_perm), gt_inner), (), _, loc_map)), gt_rest) ->
        let its_bounds = IndexTerms.Bounds.get_bounds (i, i_bt) it_perm in
        let gt_inner = aux (Sym.Set.add i vars) gt_inner in
        let gt_rest, constraints =
          collect_constraints (Sym.Set.add x vars) x i its_bounds gt_rest
        in
        let gt_rest = aux (Sym.Set.add x vars) gt_rest in
        if is_simp_true constraints then
          Term.let_star_
            ((x, Term.map_ ((i, i_bt, it_perm), gt_inner) () loc_map), gt_rest)
            ()
            loc
        else (
          let result = Sym.fresh_anon () in
          let gt_inner =
            let loc_inner = Term.loc gt_inner in
            Term.let_star_
              ( (result, gt_inner),
                Term.assert_
                  ( LogicalConstraints.T (replace_index x i result constraints),
                    Term.return_
                      (IT.sym_ (result, Term.basetype gt_inner, loc_inner))
                      ()
                      loc_inner )
                  ()
                  loc_inner )
              ()
              loc_inner
          in
          Term.let_star_
            ( (x, Term.map_ ((i, i_bt, it_perm), gt_inner) () loc_map),
              aux (Sym.Set.add x vars) gt_rest )
            ()
            loc)
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_ ((x, aux vars gt_inner), aux (Sym.Set.add x vars) gt_rest) () loc
      | `Assert (lc, gt') -> Term.assert_ (lc, aux vars gt') () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux vars gt_then, aux vars gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i, i_bt, it_perm), aux (Sym.Set.add i vars) gt_inner) () loc
    in
    aux vars gt


  let transform_gd (gd : Def.t) : Def.t =
    let body = transform_gt (gd.iargs |> List.map fst |> Sym.Set.of_list) gd.body in
    { gd with body }


  let transform (ctx : Ctx.t) : Ctx.t = List.map_snd transform_gd ctx
end
