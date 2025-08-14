module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  let transform_gt (vars : Sym.Set.t) (gt : Term.t) : Term.t =
    let rec aux (vars : Sym.Set.t) (gt : Term.t) : Term.t * AD.t =
      let (Annot (gt_, tag, bt, loc)) = gt in
      match gt_ with
      | `Arbitrary | `ArbitraryDomain _ | `Call _ | `Return _ -> (gt, AD.top)
      | `Pick gts ->
        let gts, ds = List.split (List.map (aux vars) gts) in
        (Term.pick_ gts tag bt loc, AD.join_many ds)
      | `Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest, d = aux vars gt_rest in
        (Term.asgn_ ((it_addr, sct), it_val, gt_rest) tag loc, d)
      | `LetStar ((x, Annot (`Arbitrary, tag_inner, bt_inner, loc_inner)), gt_rest) ->
        let vars' = Sym.Set.add x vars in
        let gt_rest, d = aux vars' gt_rest in
        let d = AD.retain vars' d in
        let gt_inner =
          Term.arbitrary_domain_
            (AD.relative_to x bt_inner d)
            tag_inner
            bt_inner
            loc_inner
        in
        let gt' = Term.let_star_ ((x, gt_inner), gt_rest) tag loc in
        let d_remove_x = AD.remove x d in
        if
          Sym.Set.mem x (AD.free_vars d)
          && not (Sym.Set.is_empty (AD.free_vars d_remove_x))
        then
          (Term.assert_domain_ (d_remove_x, gt') tag loc, d_remove_x)
        else
          (gt', d)
      | `LetStar ((x, gt_inner), gt_rest) ->
        let gt_inner, _ = aux vars gt_inner in
        let gt_rest, d = aux (Sym.Set.add x vars) gt_rest in
        (Term.let_star_ ((x, gt_inner), gt_rest) tag loc, d)
      | `Assert (lc, gt_rest) ->
        let gt_rest, d = aux vars gt_rest in
        (Term.assert_ (lc, gt_rest) tag loc, d)
      | `AssertDomain (d, gt_rest) -> (Term.assert_domain_ (d, gt_rest) tag loc, d)
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then, d_then = aux vars gt_then in
        let gt_else, d_else = aux vars gt_else in
        (Term.ite_ (it_if, gt_then, gt_else) tag loc, AD.join d_then d_else)
      | `Map ((i, i_bt, it_perm), gt_inner) ->
        let gt_inner, d = aux (Sym.Set.add i vars) gt_inner in
        (Term.map_ ((i, i_bt, it_perm), gt_inner) tag loc, AD.remove i d)
    in
    fst (aux vars gt)


  let transform_gd (gd : Def.t) : Def.t =
    let vars = gd.iargs |> List.map fst |> Sym.Set.of_list in
    { gd with body = transform_gt vars gd.body }


  let transform (ctx : Ctx.t) : Ctx.t =
    Cerb_debug.print_debug 2 [] (fun () -> "specialize_domain");
    List.map_snd transform_gd ctx
end
