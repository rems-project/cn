module Make (AD : Domain.T) = struct
  module Ctx = Ctx.Make (AD)
  module Def = Def.Make (AD)

  let transform (ctx : Ctx.t) : Ctx.t =
    (* Get all spec generators *)
    let spec_generators =
      ctx
      |> List.filter (fun (_, (gd : Def.t)) -> gd.spec)
      |> List.map fst
      |> Sym.Set.of_list
    in
    (* Compute transitive closure of the call graph *)
    let module Oper = Graph.Oper.P (Sym.Digraph) in
    let cg = Oper.transitive_closure (Ctx.get_call_graph ctx) in
    (* Collect all generators reachable from spec generators (includes spec generators themselves and their successors) *)
    let reachable_generators =
      Sym.Set.fold
        (fun spec_gen acc ->
           (* Add the spec generator itself *)
           let acc = Sym.Set.add spec_gen acc in
           (* Add all its successors in the transitive closure *)
           Sym.Digraph.fold_succ Sym.Set.add cg spec_gen acc)
        spec_generators
        Sym.Set.empty
    in
    (* Filter context to only include reachable generators *)
    List.filter (fun (name, _) -> Sym.Set.mem name reachable_generators) ctx
end
