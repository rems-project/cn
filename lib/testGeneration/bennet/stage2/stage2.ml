module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage1.Ctx.t) : Ctx.t =
  ctx |> Convert.transform |> Reorder.transform |> Specialize_equality.transform
