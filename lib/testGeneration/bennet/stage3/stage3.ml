module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
    module Specialize = Specialize.Make (AD)
    module SpecializeDomain = SpecializeDomain.Make (AD)
    module SmtPruning = SmtPruning.Make (AD)
    module PruneCallGraph = PruneCallGraph.Make (Term.Make (AD))
  end

  module Stage2 = Stage2.Make (AD)
  module AI = AbstractDomains.Make (Term.Make (AD))
  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (paused : _ Typing.pause) (ctx : Stage2.Ctx.t) : Ctx.t =
    ctx
    |> Convert.transform
    |> (match TestGenConfig.has_smt_pruning_before_absinst () with
      | `Fast -> SmtPruning.transform paused true
      | `Slow -> SmtPruning.transform paused false
      | `None -> fun ctx -> ctx)
    |> (if TestGenConfig.is_symbolic_enabled () then
          fun ctx -> ctx
        else
          Specialize.Integer.transform)
    |> (if List.non_empty (TestGenConfig.has_static_absint ()) then
          fun ctx -> ctx |> AI.annotate |> SpecializeDomain.transform
        else
          fun ctx -> ctx)
    |> (match TestGenConfig.has_smt_pruning_after_absinst () with
      | `Fast -> SmtPruning.transform paused true
      | `Slow -> SmtPruning.transform paused false
      | `None -> fun ctx -> ctx)
    |> PruneCallGraph.transform
  (* |> SimplifyGen.transform prog5 *)
end
