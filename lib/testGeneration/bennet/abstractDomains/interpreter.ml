module IT = IndexTerms
module LC = LogicalConstraints

module Make (GT : GenTerms.T) (I : Domain.T with type t = GT.AD.t) = struct
  open struct
    module AD = GT.AD
    module Def = GenDefinitions.Make (GT)
    module Ctx = GenContext.Make (GT)
  end

  (* Optimized domain accumulation helpers for associative meet operations *)
  let domain_cons d d_list =
    if AD.is_meet_assoc then (
      match d_list with
      | [] -> [ d ]
      | [ d_acc ] -> [ AD.meet d d_acc ]
      | _ -> [ AD.meet d (AD.meet_many d_list) ])
    else
      d :: d_list


  let domain_finalize d_list =
    if AD.is_meet_assoc then (
      match d_list with [ d ] -> d | _ -> AD.meet_many d_list)
    else
      AD.meet_many d_list


  let domain_map_remove sym d_list =
    if AD.is_meet_assoc then
      [ AD.remove sym (domain_finalize d_list) ]
    else
      List.map (AD.remove sym) d_list


  let domain_map_rename ~from ~to_ d_list =
    if AD.is_meet_assoc then
      [ AD.rename ~from ~to_ (domain_finalize d_list) ]
    else
      List.map (AD.rename ~from ~to_) d_list


  let annotate_gd (abs_ctx : AD.t Sym.Map.t) (defs_ctx : Ctx.t) (gd : Def.t)
    : GT.t * AD.t list
    =
    let rec aux
              (abs_ctx : AD.t Sym.Map.t)
              (defs_ctx : Ctx.t)
              (tm : GT.t)
              (d : AD.t)
              should_assert
      : GT.t * AD.t list
      =
      let (GenTerms.Annot (tm_, tag, bt, loc)) = tm in
      match tm_ with
      | `Arbitrary -> (GT.arbitrary_ tag bt loc, [ d ])
      | `Symbolic -> (GT.symbolic_ tag bt loc, [ d ])
      | `ArbitrarySpecialized bounds ->
        (GT.arbitrary_specialized_ bounds tag bt loc, [ d ])
      | `ArbitraryDomain _ -> failwith ("unreachable @ " ^ __LOC__)
      | `Return _ ->
        let tm' = if should_assert then GT.assert_domain_ (d, tm) tag loc else tm in
        (tm', [ d ])
      | `Call (fsym, actual_args) | `CallSized (fsym, actual_args, _) ->
        let d' =
          match Sym.Map.find_opt fsym abs_ctx with
          | Some callee_d ->
            (* Use backwards abstract interpretation to propagate callee constraints *)
            (match Ctx.find_opt fsym defs_ctx with
             | Some callee_gd ->
               (* Convert callee's domain to a constraint *)
               let callee_constraint = AD.to_it callee_d in
               (* Create substitution: formal_param -> actual_arg *)
               let subst =
                 IT.make_subst
                   (List.map2
                      (fun (formal_sym, _) actual_arg -> (formal_sym, actual_arg))
                      callee_gd.iargs
                      actual_args)
               in
               (* Substitute to get constraint in caller's terms *)
               let caller_constraint = IT.subst subst callee_constraint in
               (* Propagate constraint to refine caller's domain *)
               I.abs_assert (LC.T caller_constraint) d
             | None -> AD.meet d callee_d)
          | None ->
            (* function has no ownership info *)
            d
        in
        let tm' = if should_assert then GT.assert_domain_ (d', tm) tag loc else tm in
        (tm', [ d' ])
      | `Map ((i_sym, i_bt, it_perm), gt_inner) ->
        let d' = I.abs_assert (LC.T it_perm) d in
        let gt_inner, d_list = aux abs_ctx defs_ctx gt_inner d' should_assert in
        ( GT.map_ ((i_sym, i_bt, it_perm), gt_inner) tag loc,
          domain_map_remove i_sym d_list )
      | `MapElab ((i_sym, i_bt, it_bounds, it_perm), gt_inner) ->
        let d' = I.abs_assert (LC.T it_perm) d in
        let gt_inner, d_list = aux abs_ctx defs_ctx gt_inner d' should_assert in
        ( GT.map_elab_ ((i_sym, i_bt, it_bounds, it_perm), gt_inner) tag loc,
          domain_map_remove i_sym d_list )
      | `Asgn ((it_addr, sct), it_val, gt') ->
        let d' = I.abs_assign ((it_addr, sct), it_val) d in
        let gt', d_list = aux abs_ctx defs_ctx gt' d' should_assert in
        (GT.asgn_ ((it_addr, sct), it_val, gt') tag loc, domain_cons d' d_list)
      | `AsgnElab (backtrack_var, ((pointer, it_addr), sct), it_val, gt') ->
        let d' = I.abs_assign ((it_addr, sct), it_val) d in
        let gt', d_list = aux abs_ctx defs_ctx gt' d' should_assert in
        ( GT.asgn_elab_ (backtrack_var, ((pointer, it_addr), sct), it_val, gt') tag loc,
          domain_cons d' d_list )
      | `Assert (lc, gt') ->
        let d' = I.abs_assert lc d in
        let gt', d_list = aux abs_ctx defs_ctx gt' d' should_assert in
        (GT.assert_ (lc, gt') tag loc, domain_cons d' d_list)
      | `AssertDomain (ad, gt') ->
        (* Delete `assert_domain` to avoid dupes *)
        let gt', d_list = aux abs_ctx defs_ctx gt' (AD.meet ad d) should_assert in
        (gt', domain_cons ad d_list)
      | `SplitSize (syms, gt') ->
        let gt', d_list = aux abs_ctx defs_ctx gt' d should_assert in
        (GT.split_size_ (syms, gt') tag loc, d_list)
      | `SplitSizeElab (marker_var, syms, gt') ->
        let gt', d_list = aux abs_ctx defs_ctx gt' d should_assert in
        (GT.split_size_elab_ (marker_var, syms, gt') tag loc, d_list)
      | `Instantiate ((x, gt_inner), gt') ->
        (* Invisible to abstract interpreter *)
        let gt', d_list = aux abs_ctx defs_ctx gt' d should_assert in
        (GT.instantiate_ ((x, gt_inner), gt') tag loc, d_list)
      | `InstantiateElab (backtrack_var, (x, gt_inner), gt') ->
        (* Invisible to abstract interpreter *)
        let gt', d_list = aux abs_ctx defs_ctx gt' d should_assert in
        (GT.instantiate_elab_ (backtrack_var, (x, gt_inner), gt') tag loc, d_list)
      | `LetStar ((x, gt1), gt2) ->
        let gt1, d_list1 = aux abs_ctx defs_ctx gt1 d false in
        let d_list1' = domain_map_rename ~from:Domain.ret_sym ~to_:x d_list1 in
        let gt2, d_list2 =
          aux abs_ctx defs_ctx gt2 (domain_finalize d_list1') should_assert
        in
        (GT.let_star_ ((x, gt1), gt2) tag loc, domain_map_remove x d_list2)
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then, d_then_list =
          let d' = I.abs_assert (LC.T it_if) d in
          aux abs_ctx defs_ctx gt_then d' should_assert
        in
        let not_it_if = IT.not_ it_if (IT.get_loc it_if) in
        let gt_else, d_else_list =
          let d' = I.abs_assert (LC.T not_it_if) d in
          aux abs_ctx defs_ctx gt_else d' should_assert
        in
        let d_then = domain_finalize d_then_list in
        let d_else = domain_finalize d_else_list in
        if AD.equal d_then AD.bottom then
          (GT.assert_ (T not_it_if, gt_else) tag loc, d_else_list)
        else if AD.equal d_else AD.bottom then
          (GT.assert_ (T it_if, gt_then) tag loc, d_then_list)
        else (
          let d' = AD.join d_then d_else in
          (GT.ite_ (it_if, gt_then, gt_else) tag loc, [ d' ]))
      | `Pick gts ->
        let branch_results =
          gts
          |> List.filter_map (fun gt ->
            let gt, d_list = aux abs_ctx defs_ctx gt d should_assert in
            let d_meet = domain_finalize d_list in
            (* Prune branches that require bottom *)
            if AD.equal d_meet AD.bottom then None else Some (gt, d_meet))
        in
        let gts, d_meets = List.split branch_results in
        let d' = AD.join_many d_meets in
        (GT.pick_ gts tag bt loc, [ d' ])
      | `PickSized wgts ->
        let branch_results =
          wgts
          |> List.filter_map (fun (w, gt) ->
            let gt, d_list = aux abs_ctx defs_ctx gt d should_assert in
            let d_meet = domain_finalize d_list in
            (* Prune branches that require bottom *)
            if AD.equal d_meet AD.bottom then None else Some ((w, gt), d_meet))
        in
        let wgts, d_meets = List.split branch_results in
        let d' = AD.join_many d_meets in
        (GT.pick_sized_ wgts tag bt loc, [ d' ])
      | `PickSizedElab (choice_var, wgts) ->
        let branch_results =
          wgts
          |> List.filter_map (fun (w, gt) ->
            let gt, d_list = aux abs_ctx defs_ctx gt d should_assert in
            let d_meet = domain_finalize d_list in
            (* Prune branches that require bottom *)
            if AD.equal d_meet AD.bottom then None else Some ((w, gt), d_meet))
        in
        let wgts, d_meets = List.split branch_results in
        let d' = AD.join_many d_meets in
        (GT.pick_sized_elab_ choice_var wgts tag bt loc, [ d' ])
    in
    let rec loop d =
      let gt, d_list = aux abs_ctx defs_ctx gd.body d true in
      let d' = domain_finalize (domain_cons d d_list) in
      if AD.equal d d' then (gt, d_list) else loop d'
    in
    loop (Option.value ~default:AD.top (Sym.Map.find_opt gd.name abs_ctx))


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
        let gt, d_list = annotate_gd abs_ctx ctx gd in
        let d = domain_finalize d_list in
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
