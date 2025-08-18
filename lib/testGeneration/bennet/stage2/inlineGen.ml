module IT = IndexTerms

module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)
  module Term = Term.Make (AD)

  module InlineNonRecursive = struct
    let transform_gt (ctx : Ctx.t) (gt : Term.t) : Term.t =
      let rec aux (gt : Term.t) : Term.t =
        let (Annot (gt_, (), bt, loc)) = gt in
        match gt_ with
        | `Arbitrary | `Symbolic | `Return _ -> gt
        | `Pick wgts -> Term.pick_ (List.map aux wgts) () bt loc
        | `Call (fsym, _) when (Ctx.find fsym ctx).recursive -> gt
        | `Call (fsym, iargs) ->
          let gd = Ctx.find fsym ctx in
          aux
            (Term.subst
               (IT.make_subst (List.combine (List.map fst gd.iargs) iargs))
               gd.body)
        | `Asgn ((it_addr, sct), it_val, gt_rest) ->
          let gt_rest = aux gt_rest in
          Term.asgn_ ((it_addr, sct), it_val, gt_rest) () loc
        | `LetStar ((x, gt_inner), gt_rest) ->
          let gt_inner = aux gt_inner in
          let gt_rest = aux gt_rest in
          Term.let_star_ ((x, gt_inner), gt_rest) () loc
        | `Assert (lc, gt_rest) ->
          let gt_rest = aux gt_rest in
          Term.assert_ (lc, gt_rest) () loc
        | `ITE (it_if, gt_then, gt_else) ->
          let gt_then = aux gt_then in
          let gt_else = aux gt_else in
          Term.ite_ (it_if, gt_then, gt_else) () loc
        | `Map ((i, i_bt, it_perm), gt_inner) ->
          let gt_inner = aux gt_inner in
          Term.map_ ((i, i_bt, it_perm), gt_inner) () loc
      in
      aux gt


    let transform_gd (ctx : Ctx.t) (gd : Def.t) : Def.t =
      { gd with body = transform_gt ctx gd.body }


    let transform (ctx : Ctx.t) : Ctx.t =
      List.map_snd (fun (gd : Def.t) -> if gd.spec then transform_gd ctx gd else gd) ctx
  end

  module InlineRecursive = struct
    let transform_gt (ctx : Ctx.t) (dont_unfold : Sym.Set.t) (gt : Term.t) : Term.t =
      let rec aux (gt : Term.t) : Term.t =
        let (Annot (gt_, (), bt, loc)) = gt in
        match gt_ with
        | `Arbitrary | `Symbolic | `Return _ -> gt
        | `Pick wgts -> Term.pick_ (List.map aux wgts) () bt loc
        | `Call (fsym, _) when Sym.Set.mem fsym dont_unfold -> gt
        | `Call (fsym, iargs) ->
          let gd = Ctx.find fsym ctx in
          aux
            (Term.subst
               (IT.make_subst (List.combine (List.map fst gd.iargs) iargs))
               gd.body)
        | `Asgn ((it_addr, sct), it_val, gt_rest) ->
          let gt_rest = aux gt_rest in
          Term.asgn_ ((it_addr, sct), it_val, gt_rest) () loc
        | `LetStar ((x, gt_inner), gt_rest) ->
          let gt_inner = aux gt_inner in
          let gt_rest = aux gt_rest in
          Term.let_star_ ((x, gt_inner), gt_rest) () loc
        | `Assert (lc, gt_rest) ->
          let gt_rest = aux gt_rest in
          Term.assert_ (lc, gt_rest) () loc
        | `ITE (it_if, gt_then, gt_else) ->
          let gt_then = aux gt_then in
          let gt_else = aux gt_else in
          Term.ite_ (it_if, gt_then, gt_else) () loc
        | `Map ((i, i_bt, it_perm), gt_inner) ->
          let gt_inner = aux gt_inner in
          Term.map_ ((i, i_bt, it_perm), gt_inner) () loc
      in
      aux gt


    let transform_gd (ctx : Ctx.t) (gd : Def.t) : Def.t =
      let recursive_fsyms =
        ctx
        |> List.filter (fun ((_, gd) : _ * Def.t) -> gd.recursive)
        |> List.map fst
        |> Sym.Set.of_list
      in
      let dont_unfold =
        if gd.spec then Sym.Set.add gd.name recursive_fsyms else Sym.Set.singleton gd.name
      in
      { gd with body = transform_gt ctx dont_unfold gd.body }


    let transform (ctx : Ctx.t) =
      let cg = Ctx.get_call_graph ctx in
      ctx
      |> List.filter (fun ((_, gd) : _ * Def.t) ->
        gd.spec
        || (gd.recursive
            && List.exists
                 (fun pred_fsym -> (List.assoc Sym.equal pred_fsym ctx).spec)
                 (Sym.Digraph.pred cg gd.name)))
      |> List.map_snd (transform_gd ctx)
  end

  let transform (ctx : Ctx.t) =
    ctx |> InlineNonRecursive.transform |> InlineRecursive.transform
end
