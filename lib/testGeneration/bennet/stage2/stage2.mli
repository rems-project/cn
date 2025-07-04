module Term = Term
module Def = Def
module Ctx = Ctx

val transform : unit Mucore.file -> _ Typing.pause -> Stage1.Ctx.t -> Ctx.t
