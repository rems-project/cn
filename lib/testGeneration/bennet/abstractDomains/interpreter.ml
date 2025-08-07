module type Part = sig
  module AD : Domain.T

  val abs_stmt
    :  AD.t Sym.Map.t ->
    ('tag, [< ('tag, 'recur) GenTerms.Make(AD).Inner.ast ]) GenTerms.annot ->
    AD.t ->
    AD.t
end

module Make (GT : GenTerms.T) (I : Part with module AD = GT.AD) = struct
  open struct
    module AD = GT.AD
    module Def = GenDefinitions.Make (GT)
    module Ctx = GenContext.Make (GT)
  end

  let annotate_gd (ctx : AD.t Sym.Map.t) (gd : Def.t) : GT.t * AD.t =
    let rec aux (ctx : AD.t Sym.Map.t) (tm : GT.t) (d : AD.t) : GT.t * AD.t =
      let (GenTerms.Annot (tm_, tag, bt, loc)) = tm in
      match tm_ with
      | `Arbitrary _ -> (tm, I.abs_stmt ctx tm d)
      | `Return _ -> (tm, I.abs_stmt ctx tm d)
      | `Call (_, _) -> (tm, I.abs_stmt ctx tm d)
      | `CallSized (_, _, _) -> (tm, I.abs_stmt ctx tm d)
      | `Map _ -> (tm, I.abs_stmt ctx tm d)
      | `MapElab _ -> (tm, I.abs_stmt ctx tm d)
      | `Asgn ((it_addr, sct), it_val, gt') ->
        let d' = I.abs_stmt ctx tm d in
        let gt', d'' = aux ctx gt' d' in
        (GT.asgn_ ((it_addr, sct), it_val, gt') tag loc, d'')
      | `AsgnElab (_, _, _, gt') ->
        let d' = I.abs_stmt ctx tm d in
        aux ctx gt' d'
      | `Assert (_, gt') ->
        let d' = I.abs_stmt ctx tm d in
        aux ctx gt' d'
      | `SplitSize (_, gt') ->
        let d' = I.abs_stmt ctx tm d in
        aux ctx gt' d'
      | `SplitSizeElab (_, _, gt') ->
        let d' = I.abs_stmt ctx tm d in
        aux ctx gt' d'
      | `LetStar ((x, gt1), gt2) ->
        let gt1, d' = aux ctx gt1 d in
        let d'' = AD.rename ~from:Domain.ret_sym ~to_:x d' in
        let gt2, d'' = aux ctx gt2 d'' in
        (GT.let_star_ ((x, gt1), gt2) tag loc, d'')
      | `ITE (it_if, gt_then, gt_else) ->
        let gt_then, d_then = aux ctx gt_then d in
        let gt_else, d_else = aux ctx gt_else d in
        let d' = AD.join d_then d_else in
        (GT.ite_ (it_if, gt_then, gt_else) tag loc, d')
      | `Pick gts ->
        let gts, d' =
          List.fold_left
            (fun (gts, d') gt ->
               let gt, d'' = aux ctx gt d in
               (gt :: gts, AD.join d' d''))
            ([], AD.bottom)
            gts
        in
        (GT.pick_ gts tag bt loc, d')
      | `PickSized wgts ->
        let wgts, d' =
          List.fold_left
            (fun (gts, d') (w, gt) ->
               let gt, d'' = aux ctx gt d in
               ((w, gt) :: gts, AD.join d' d''))
            ([], AD.bottom)
            wgts
        in
        (GT.pick_sized_ wgts tag bt loc, d')
      | `PickSizedElab (_, wgts) ->
        let wgts, d' =
          List.fold_left
            (fun (gts, d') (w, gt) ->
               let gt, d'' = aux ctx gt d in
               ((w, gt) :: gts, AD.join d' d''))
            ([], AD.bottom)
            wgts
        in
        (GT.pick_sized_elab_ wgts tag bt loc, d')
    in
    let rec loop d =
      let gt, d' = aux ctx gd.body d in
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
