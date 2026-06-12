module Private : sig
  module LiftConstraints = LiftConstraints
end

module Make (AD : Domain.T) : sig
  val transform : unit Mucore.file -> Ctx.Make(AD).t -> Ctx.Make(AD).t
end
