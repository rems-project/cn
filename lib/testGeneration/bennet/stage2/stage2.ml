module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
    module SimplifyGen = SimplifyGen.Make (AD)
    module InlineGen = InlineGen.Make (AD)
    module EachFusion = EachFusion.Make (AD)
    module FlipIfs = FlipIfs.Make (AD)
    module Reorder = Reorder.Make (AD)
    module SpecializeEquality = SpecializeEquality.Make (AD)
    module SimplifyNames = SimplifyNames.Make (AD)
  end

  module Stage1 = Stage1.Make (AD)
  module Term = Term.Make (AD)
  module Def = Def.Make (AD)
  module Ctx = Ctx.Make (AD)

  let transform (prog5 : unit Mucore.file) (ctx : Stage1.Ctx.t) : Ctx.t =
    ctx
    |> Convert.transform
    |> SimplifyGen.MemberIndirection.transform
    |> SimplifyGen.transform prog5
    |> InlineGen.transform prog5
    |> (if TestGenConfig.is_symbolic_enabled () then fun x -> x else EachFusion.transform)
    |> SimplifyGen.transform prog5
    |> FlipIfs.transform
    |> SimplifyGen.transform prog5
    |> Reorder.transform
    |> SpecializeEquality.transform
    |> SimplifyGen.transform prog5
    |> SimplifyNames.transform
end
