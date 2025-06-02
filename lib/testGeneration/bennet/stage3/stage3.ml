module IT = IndexTerms
module StringMap = Map.Make (String)
module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage2.Ctx.t) : Ctx.t = ctx |> Convert.transform |> Sizing.transform
