module Make (AD : Domain.T) : sig
  module Term : module type of Term.Make (AD)

  module Def : module type of Def.Make (AD)

  module Ctx : module type of Ctx.Make (AD)

  val transform : string -> unit Mucore.file -> Test.t list -> Ctx.t
end
