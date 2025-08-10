module Make (AD : Domain.T) : sig
  module Term : module type of Term.Make (AD)

  module Def : module type of Def.Make (AD)

  module Ctx : module type of Ctx.Make (AD)

  val transform : unit Mucore.file -> Stage1.Make(AD).Ctx.t -> Ctx.t
end
