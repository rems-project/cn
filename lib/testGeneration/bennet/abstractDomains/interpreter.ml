module Make (GT : GenTerms.T) (I : Domain.Interpreter with module AD = GT.AD) = struct
  open struct
    module AD = GT.AD
    module Def = GenDefinitions.Make (GT)
    module Ctx = GenContext.Make (GT)
  end

  let interpret_gd (ctx : AD.t Sym.Map.t) (gd : Def.t) : AD.t =
    let rec interp (ctx : AD.t Sym.Map.t) (tm : GT.t) (d : AD.t) : AD.t =
      let (GenTerms.Annot (tm_, _tag, _bt, _loc)) = tm in
      match tm_ with
      | `Arbitrary _ | `Return _
      | `Call (_, _)
      | `CallSized (_, _, _)
      | `Map _ | `MapElab _ ->
        I.abs_stmt ctx tm d
      | `Asgn (_, _, gt')
      | `AsgnElab (_, _, _, gt')
      | `Assert (_, gt')
      | `SplitSize (_, gt')
      | `SplitSizeElab (_, _, gt') ->
        let d' = I.abs_stmt ctx tm d in
        interp ctx gt' d'
      | `LetStar ((x, gt1), gt2) ->
        let d' = interp ctx gt1 d in
        let d'' = AD.rename ~from:GenTerms.Domain.ret_sym ~to_:x d' in
        interp ctx gt2 d''
      | `ITE (_it_if, gt_then, gt_else) ->
        let d_then = interp ctx gt_then d in
        let d_else = interp ctx gt_else d in
        AD.join d_then d_else
      | `Pick gts ->
        (match gts with
         | [] -> AD.bottom
         | gt :: gts' ->
           List.fold_left
             (fun acc gt ->
                let d' = interp ctx gt d in
                AD.join acc d')
             (interp ctx gt d)
             gts')
      | `PickSized wgts | `PickSizedElab (_, wgts) ->
        (match wgts with
         | [] -> AD.bottom
         | (_, gt) :: wgts' ->
           List.fold_left
             (fun acc (_, gt) ->
                let d' = interp ctx gt d in
                AD.join acc d')
             (interp ctx gt d)
             wgts')
    in
    let rec loop d =
      let d' = interp ctx gd.body d in
      if AD.equal d d' then d else loop d'
    in
    loop (Option.value ~default:AD.top (Sym.Map.find_opt gd.name ctx))


  let interpret (ctx : Ctx.t) : AD.t Sym.Map.t =
    let cg = Ctx.get_call_graph ctx in
    let cg_order =
      let module T = Graph.Topological.Make (Sym.Digraph) in
      T.fold List.cons cg []
    in
    let rec loop (worklist : Sym.t list) (abs_ctx : AD.t Sym.Map.t) : AD.t Sym.Map.t =
      match worklist with
      | fsym :: worklist' ->
        let existing_d = Option.value ~default:AD.top (Sym.Map.find_opt fsym abs_ctx) in
        let d = interpret_gd abs_ctx (Ctx.find fsym ctx) in
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
        loop worklist'' (Sym.Map.add fsym d abs_ctx)
      | [] -> Sym.Map.empty
    in
    loop cg_order Sym.Map.empty
end
