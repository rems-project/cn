module IT = IndexTerms
module StringMap = Map.Make (String)
module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage3.Ctx.t) : Ctx.t = Convert.transform ctx
