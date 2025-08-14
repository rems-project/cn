module IT = IndexTerms
module LC = LogicalConstraints

module Make (GT : GenTerms.T) (I : Domain.T with type t = GT.AD.t) = struct
  open struct
    module AD = GT.AD
    module Def = GenDefinitions.Make (GT)
    module Ctx = GenContext.Make (GT)
  end

  let annotate_gd (ctx : AD.t Sym.Map.t) (gd : Def.t) : GT.t * AD.t =
    let rec aux (ctx : AD.t Sym.Map.t) (tm : GT.t) (d : AD.t) should_assert : GT.t * AD.t =
      let (GenTerms.Annot (tm_, tag, bt, loc)) = tm in
      match tm_ with
      | `Arbitrary -> (GT.arbitrary_ tag bt loc, d)
      | `ArbitraryDomain _ -> failwith ("unreachable @ " ^ __LOC__)
      | `Return _ ->
        let tm' = if should_assert then GT.assert_domain_ (d, tm) tag loc else tm in
        (tm', d)
      | `Call (fsym, _) | `CallSized (fsym, _, _) ->
        let d' =
          match Sym.Map.find_opt fsym ctx with
          | Some d' ->
            (* Add return symbol's ownership from callee to current domain *)
            AD.meet d d'
          | None ->
            (* function has no ownership info *)
            d
        in
        let tm' = if should_assert then GT.assert_domain_ (d', tm) tag loc else tm in
        (tm', d')
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        let d' = I.abs_assert (LC.T it_perm) d in
        let gt_inner, d'' = aux ctx gt_inner d' should_assert in
        (GT.map_ ((i_sym, i_bt, it_perm), gt_inner) tag loc, AD.remove i_sym d'')
      | `MapElab ((i_sym, i_bt, it_bounds, it_perm), gt_inner) ->
        let d' = I.abs_assert (LC.T it_perm) d in
        let gt_inner, d'' = aux ctx gt_inner d' should_assert in
        ( GT.map_elab_ ((i_sym, i_bt, it_bounds, it_perm), gt_inner) tag loc,
          AD.remove i_sym d'' )
      | `Asgn ((it_addr, sct), it_val, gt') ->
        let d' = I.abs_assign ((it_addr, sct), it_val) d in
        let gt', d'' = aux ctx gt' d' should_assert in
        (GT.asgn_ ((it_addr, sct), it_val, gt') tag loc, d'')
      | `AsgnElab (backtrack_var, ((pointer, it_addr), sct), it_val, gt') ->
        let d' = I.abs_assign ((it_addr, sct), it_val) d in
        let gt', d'' = aux ctx gt' d' should_assert in
        ( GT.asgn_elab_ (backtrack_var, ((pointer, it_addr), sct), it_val, gt') tag loc,
          d'' )
      | `Assert (lc, gt') ->
        let d' = I.abs_assert lc d in
        let gt', d'' = aux ctx gt' d' should_assert in
        (GT.assert_ (lc, gt') tag loc, d'')
      | `AssertDomain (ad, gt') ->
        (* Delete `assert_domain` to avoid dupes *)
        aux ctx gt' (AD.meet ad d) should_assert
      | `SplitSize (syms, gt') ->
        let gt', d'' = aux ctx gt' d should_assert in
        (GT.split_size_ (syms, gt') tag loc, d'')
      | `SplitSizeElab (marker_var, syms, gt') ->
        let gt', d'' = aux ctx gt' d should_assert in
        (GT.split_size_elab_ (marker_var, syms, gt') tag loc, d'')
      | `LetStar ((x, gt1), gt2) ->
        let gt1, d' = aux ctx gt1 d false in
        let d'' = AD.rename ~from:Domain.ret_sym ~to_:x d' in
        let gt2, d'' = aux ctx gt2 d'' should_assert in
        (GT.let_star_ ((x, gt1), gt2) tag loc, AD.remove x d'')
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then, d_then =
          let d' = I.abs_assert (LC.T it_if) d in
          aux ctx gt_then d' should_assert
        in
        let not_it_if = IT.not_ it_if (IT.get_loc it_if) in
        let gt_else, d_else =
          let d' = I.abs_assert (LC.T not_it_if) d in
          aux ctx gt_else d' should_assert
        in
        if AD.equal d_then AD.bottom then
          (GT.assert_ (T not_it_if, gt_else) tag loc, d_else)
        else if AD.equal d_else AD.bottom then
          (GT.assert_ (T it_if, gt_then) tag loc, d_then)
        else (
          let d' = AD.join d_then d_else in
          (GT.ite_ (it_if, gt_then, gt_else) tag loc, d'))
      | `Pick gts ->
        let gts, d' =
          gts
          |> List.filter_map (fun gt ->
            let gt, d' = aux ctx gt d should_assert in
            (* Prune branches that require bottom *)
            if AD.equal d' AD.bottom then None else Some (gt, d'))
          |> List.fold_left
               (fun (gts, d') (gt, d'') -> (gt :: gts, AD.join d' d''))
               ([], AD.bottom)
        in
        (GT.pick_ gts tag bt loc, d')
      | `PickSized wgts ->
        let wgts, d' =
          wgts
          |> List.filter_map (fun (w, gt) ->
            let gt, d' = aux ctx gt d should_assert in
            (* Prune branches that require bottom *)
            if AD.equal d' AD.bottom then None else Some ((w, gt), d'))
          |> List.fold_left
               (fun (gts, d') ((w, gt), d'') -> ((w, gt) :: gts, AD.join d' d''))
               ([], AD.bottom)
        in
        (GT.pick_sized_ wgts tag bt loc, d')
      | `PickSizedElab (_, wgts) ->
        let wgts, d' =
          wgts
          |> List.filter_map (fun (w, gt) ->
            let gt, d' = aux ctx gt d should_assert in
            (* Prune branches that require bottom *)
            if AD.equal d' AD.bottom then None else Some ((w, gt), d'))
          |> List.fold_left
               (fun (gts, d') ((w, gt), d'') -> ((w, gt) :: gts, AD.join d' d''))
               ([], AD.bottom)
        in
        (GT.pick_sized_elab_ wgts tag bt loc, d')
    in
    let rec loop d =
      let gt, d' = aux ctx gd.body d true in
      if AD.equal d d' then (gt, d) else loop d'
    in
    loop (Option.value ~default:AD.top (Sym.Map.find_opt gd.name ctx))


  let annotate (ctx : Ctx.t) : Ctx.t =
    let cg = Ctx.get_call_graph ctx in
    let cg_order =
      let module T = Graph.Topological.Make (Sym.Digraph) in
      T.fold List.cons cg []
    in
    let rec loop (worklist : Sym.t list) (abs_ctx : AD.t Sym.Map.t) : Ctx.t =
      match worklist with
      | fsym :: worklist' ->
        let gd = Ctx.find fsym ctx in
        let existing_d = Option.value ~default:AD.top (Sym.Map.find_opt fsym abs_ctx) in
        let gt, d = annotate_gd abs_ctx gd in
        let worklist'' =
          if AD.equal existing_d d then
            worklist'
          else (
            let successors =
              Sym.Digraph.fold_succ List.cons cg fsym []
              |> List.filter (fun x -> not (List.mem Sym.equal x worklist'))
            in
            List.filter (fun x -> List.mem Sym.equal x successors) cg_order @ worklist')
        in
        let ctx = loop worklist'' (Sym.Map.add fsym d abs_ctx) in
        if List.mem_assoc Sym.equal fsym ctx then
          ctx
        else
          (fsym, { gd with body = gt }) :: ctx
      | [] -> Ctx.empty
    in
    loop cg_order Sym.Map.empty
end
