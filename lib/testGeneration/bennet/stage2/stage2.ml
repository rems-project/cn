module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage1.Ctx.t) : Ctx.t =
  ctx
  |> Convert.transform
  |> LiftConstraints.transform
  |> FlipIfs.transform
  |> Reorder.transform
  |> SpecializeEquality.transform
  |> SimplifyNames.transform
