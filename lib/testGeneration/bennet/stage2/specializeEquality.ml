module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let find_constraint (vars : Sym.Set.t) (x : Sym.t) (gt : Term.t)
    : (Term.t * IT.t) option
    =
    let rec aux (gt : Term.t) : (Term.t * IT.t) option =
      let open Option in
      let (Annot (gt_, (), _, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `Pick _ | `Call _ | `Return _ | `ITE _ | `Map _ -> None
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let@ gt_rest, it = aux gt_rest in
        return (Term.asgn_ ((it_addr, sct), it_val, gt_rest) () loc, it)
      | `LetStar ((x, gt_inner), gt_rest) ->
        if TestGenConfig.has_pass "reorder" then
          (* We assume reordering has been applied,
           so `assert`s appear before the next `let*` *)
          None
        else
          let@ gt_rest, it = aux gt_rest in
          return (Term.let_star_ ((x, gt_inner), gt_rest) () loc, it)
      | `Assert (T (IT (Binop (EQ, IT (Sym x', bt, _), it), _, _)), gt_rest)
      | `Assert
          (T (IT (Binop (EQ, IT (Cast (_, IT (Sym x', bt, _)), _, _), it), _, _)), gt_rest)
        when Sym.equal x x' && Sym.Set.subset (IT.free_vars it) vars ->
        return (gt_rest, IT.cast_ bt it (IT.get_loc it))
      | `Assert (T (IT (Binop (EQ, it, IT (Sym x', bt, _)), _, _)), gt_rest)
      | `Assert
          (T (IT (Binop (EQ, it, IT (Cast (_, IT (Sym x', bt, _)), _, _)), _, _)), gt_rest)
        when Sym.equal x x' && Sym.Set.subset (IT.free_vars it) vars ->
        return (gt_rest, IT.cast_ bt it (IT.get_loc it))
      | `Assert (lc, gt_rest) ->
        let@ gt_rest, it = aux gt_rest in
        return (Term.assert_ (lc, gt_rest) () loc, it)
    in
    aux gt


  let transform_gt (vars : Sym.Set.t) (gt : Term.t) : Term.t =
    let rec aux (vars : Sym.Set.t) (gt : Term.t) : Term.t =
      let (Annot (gt_, (), bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `Symbolic | `Call _ | `Return _ -> gt
      | `Pick gts -> Term.pick_ (List.map (aux vars) gts) () bt loc
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        Term.asgn_ ((it_addr, sct), it_val, aux vars gt_rest) () loc
      | `LetStar ((x, (Annot (`Arbitrary, (), _, loc) as gt_inner)), gt_rest) ->
        let gt_rest, gt_res =
          match find_constraint vars x gt_rest with
          | Some (gt_rest, it) -> (gt_rest, Term.return_ it () loc)
          | None -> (gt_rest, gt_inner)
        in
        Term.let_star_ ((x, gt_res), aux (Sym.Set.add x vars) gt_rest) () loc
      | `LetStar ((x, gt_inner), gt_rest) ->
        Term.let_star_ ((x, aux vars gt_inner), aux (Sym.Set.add x vars) gt_rest) () loc
      | `Assert (lc, gt_rest) -> Term.assert_ (lc, aux vars gt_rest) () loc
      | `ITE (it_if, gt_then, gt_else) ->
        Term.ite_ (it_if, aux vars gt_then, aux vars gt_else) () loc
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        Term.map_ ((i, i_bt, it_perm), aux (Sym.Set.add i vars) gt_inner) () loc
    in
    aux vars gt


  let transform_gd (gd : Def.t) : Def.t =
    let vars = gd.iargs |> List.map fst |> Sym.Set.of_list in
    { gd with body = transform_gt vars gd.body }


  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "specialize_equality");
    List.map_snd transform_gd ctx
end
