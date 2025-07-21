(** This stage adds sizing *)

module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage2.Ctx.t) : Ctx.t = Convert.transform ctx
