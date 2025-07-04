(** This stage tags the AST with the last place to backtrack to *)

module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage4.Ctx.t) : Ctx.t = Convert.transform ctx
