module IT = IndexTerms
module SymGraph = Graph.Persistent.Digraph.Concrete (Sym)

module InlineNonRecursive = struct
  let transform_gt (ctx : Ctx.t) (gt : Term.t) : Term.t =
    let rec aux (gt : Term.t) : Term.t =
      let (GT (gt_, bt, loc)) = gt in
      match gt_ with
      | Uniform | Alloc | Return _ -> gt
      | Pick wgts -> Term.pick_ (List.map_snd aux wgts) bt loc
      | Call (fsym, _) when (List.assoc Sym.equal fsym ctx).recursive -> gt
      | Call (fsym, xits) ->
        let gd = ctx |> List.assoc Sym.equal fsym in
        aux (Term.subst (IT.make_subst xits) gd.body)
      | Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest = aux gt_rest in
        Term.asgn_ ((it_addr, sct), it_val, gt_rest) loc
      | LetStar ((x, gt_inner), gt_rest) ->
        let gt_inner = aux gt_inner in
        let gt_rest = aux gt_rest in
        Term.let_star_ ((x, gt_inner), gt_rest) loc
      | Assert (lc, gt_rest) ->
        let gt_rest = aux gt_rest in
        Term.assert_ (lc, gt_rest) loc
      | ITE (it_if, gt_then, gt_else) ->
        let gt_then = aux gt_then in
        let gt_else = aux gt_else in
        Term.ite_ (it_if, gt_then, gt_else) loc
      | Map ((i, i_bt, it_perm), gt_inner) ->
        let gt_inner = aux gt_inner in
        Term.map_ ((i, i_bt, it_perm), gt_inner) loc
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
      let (GT (gt_, bt, loc)) = gt in
      match gt_ with
      | Uniform | Alloc | Return _ -> gt
      | Pick wgts -> Term.pick_ (List.map_snd aux wgts) bt loc
      | Call (fsym, _) when Sym.Set.mem fsym dont_unfold -> gt
      | Call (fsym, xits) ->
        let gd = ctx |> List.assoc Sym.equal fsym in
        aux (Term.subst (IT.make_subst xits) gd.body)
      | Asgn ((it_addr, sct), it_val, gt_rest) ->
        let gt_rest = aux gt_rest in
        Term.asgn_ ((it_addr, sct), it_val, gt_rest) loc
      | LetStar ((x, gt_inner), gt_rest) ->
        let gt_inner = aux gt_inner in
        let gt_rest = aux gt_rest in
        Term.let_star_ ((x, gt_inner), gt_rest) loc
      | Assert (lc, gt_rest) ->
        let gt_rest = aux gt_rest in
        Term.assert_ (lc, gt_rest) loc
      | ITE (it_if, gt_then, gt_else) ->
        let gt_then = aux gt_then in
        let gt_else = aux gt_else in
        Term.ite_ (it_if, gt_then, gt_else) loc
      | Map ((i, i_bt, it_perm), gt_inner) ->
        let gt_inner = aux gt_inner in
        Term.map_ ((i, i_bt, it_perm), gt_inner) loc
    in
    aux gt


  open struct
    let get_calls (gd : Def.t) : Sym.Set.t =
      let rec aux (gt : Term.t) : Sym.Set.t =
        let (GT (gt_, _, _)) = gt in
        match gt_ with
        | Uniform | Alloc | Return _ -> Sym.Set.empty
        | Pick wgts ->
          wgts
          |> List.map snd
          |> List.map aux
          |> List.fold_left Sym.Set.union Sym.Set.empty
        | Call (fsym, _) -> Sym.Set.singleton fsym
        | Asgn (_, _, gt') | Assert (_, gt') | Map (_, gt') -> aux gt'
        | LetStar ((_, gt1), gt2) | ITE (_, gt1, gt2) -> Sym.Set.union (aux gt1) (aux gt2)
      in
      aux gd.body
  end

  let get_call_graph (ctx : Ctx.t) : SymGraph.t =
    ctx
    |> List.map_snd get_calls
    |> List.fold_left
         (fun cg (fsym, calls) ->
            Sym.Set.fold (fun fsym' cg' -> SymGraph.add_edge cg' fsym fsym') calls cg)
         SymGraph.empty


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
    let cg = get_call_graph ctx in
    ctx
    |> List.filter (fun ((_, gd) : _ * Def.t) ->
      gd.spec
      || (gd.recursive
          && List.exists
               (fun pred_fsym -> (List.assoc Sym.equal pred_fsym ctx).spec)
               (SymGraph.pred cg gd.name)))
    |> List.map_snd (transform_gd ctx)
end

let transform (ctx : Ctx.t) =
  ctx |> InlineNonRecursive.transform |> InlineRecursive.transform
