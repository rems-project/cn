module Term = Term
module Def = Def
module Ctx = Ctx

val transform : string -> unit Mucore.file -> Test.t list -> GenContext.Make(Term).t
