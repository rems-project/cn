module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
    module SpecializeDomain = SpecializeDomain.Make (AD)
  end

  module Stage2 = Stage2.Make (AD)
  module AI = AbstractDomains.Make (Term.Make (AD))
  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (_paused : _ Typing.pause) (ctx : Stage2.Ctx.t) : Ctx.t =
    ctx
    |> Convert.transform
    |>
    if TestGenConfig.has_static_absint () then
      fun ctx -> ctx |> AI.annotate |> SpecializeDomain.transform
    else
      fun ctx -> ctx
  (* |> (match TestGenConfig.has_smt_pruning () with
      | `Fast -> SmtPruning.transform paused true
      | `Slow -> SmtPruning.transform paused false
      | `None -> fun ctx -> ctx)
    |> SimplifyGen.transform prog5 *)
  (* |> MinimizePickWeights.transform *)
end
