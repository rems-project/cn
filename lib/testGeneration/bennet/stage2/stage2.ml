module Term = Term
module Def = Def
module Ctx = Ctx

let transform (prog5 : unit Mucore.file) (ctx : Stage1.Ctx.t) : Ctx.t =
  ctx
  |> Convert.transform
  |> SimplifyGen.transform prog5
  |> (fun ctx ->
  if TestGenConfig.has_inline_everything () then
    ctx |> InlineGen.transform |> SimplifyGen.transform prog5
  else
    ctx)
  |> EachFusion.transform
  |> SimplifyGen.transform prog5
  |> FlipIfs.transform
  |> SimplifyGen.transform prog5
  |> Reorder.transform
  |> SpecializeEquality.transform
  |> SimplifyGen.transform prog5
  |> SimplifyNames.transform
