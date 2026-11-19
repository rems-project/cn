module BT = BaseTypes
module MT = MakeTerm

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)
  module CollectConstraints = CollectConstraints.Make (AD)

  let transform_gt (vars : Sym.Set.t) (defined : Sym.Set.t) (gt : Term.t) : Term.t =
    let rec aux (vars : Sym.Set.t) (defined : Sym.Set.t) (gt : Term.t) : Term.t * AD.t =
      let (Annot (gt_, tag, bt, loc)) = gt in
      match gt_ with
      (* Important parts *)
      | `LetStar ((x, Annot (`Arbitrary, tag_inner, bt_inner, loc_inner)), gt_rest) ->
        let vars' = Sym.Set.add x vars in
        let defined' = Sym.Set.add x defined in
        let gt_rest, d = aux vars' defined' gt_rest in
        let d = AD.retain vars' d in
        let constraints, asgns =
          if TestGenConfig.has_dynamic_arbitrary_domain () then (
            let cs, asgns, _ =
              CollectConstraints.collect_constraints ~defined ~target:x gt_rest
            in
            (cs, asgns))
          else
            ([], [])
        in
        let gt_inner =
          Term.arbitrary_domain_
            (AD.relative_to x bt_inner d)
            constraints
            asgns
            tag_inner
            bt_inner
            loc_inner
        in
        let d_remove_x = AD.remove x d in
        (Term.let_star_ ((x, gt_inner), gt_rest) tag loc, d_remove_x)
      (* The rest *)
      | `Symbolic -> failwith ("unreachable @ " ^ __LOC__)
      | `Arbitrary | `ArbitrarySpecialized _ | `ArbitraryDomain _ | `Call _ | `Return _ ->
        (gt, AD.top)
      | `Pick gts ->
        let gts, ds = List.split (List.map (aux vars defined) gts) in
        (Term.pick_ gts tag bt loc, AD.join_many ds)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest, d = aux vars defined gt_rest in
        (Term.asgn_ ((it_addr, sct), it_val, gt_rest) tag loc, d)
      | `LetStar ((x, gt_inner), gt_rest) ->
        let gt_inner, _ = aux vars defined gt_inner in
        let defined' = Sym.Set.add x defined in
        let gt_rest, d = aux (Sym.Set.add x vars) defined' gt_rest in
        (Term.let_star_ ((x, gt_inner), gt_rest) tag loc, d)
      | `Assert (lc, gt_rest) ->
        let gt_rest, d = aux vars defined gt_rest in
        (Term.assert_ (lc, gt_rest) tag loc, d)
      | `AssertDomain (d, its, asgns, gt_rest) ->
        let gt_rest', d_inner = aux vars defined gt_rest in
        (Term.assert_domain_ (d, its, asgns, gt_rest') tag loc, AD.meet d d_inner)
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then, d_then = aux vars defined gt_then in
        let gt_else, d_else = aux vars defined gt_else in
        (Term.ite_ (it_if, gt_then, gt_else) tag loc, AD.join d_then d_else)
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        let defined' = Sym.Set.add i defined in
        let gt_inner, d = aux (Sym.Set.add i vars) defined' gt_inner in
        (Term.map_ ((i, i_bt, it_perm), gt_inner) tag loc, AD.remove i d)
    in
    fst (aux vars defined gt)


  let transform_gd (gd : Def.t) : Def.t =
    let vars = gd.iargs |> List.map fst |> Sym.Set.of_list in
    let defined = vars in
    let body = transform_gt vars defined gd.body in
    { gd with body }


  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "specialize_domain");
    List.map_snd transform_gd ctx
end
