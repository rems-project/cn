module Make (AD : Domain.T) : sig
  module MemberIndirection : module type of MemberIndirection.Make (AD)

  val transform : unit Mucore.file -> Ctx.Make(AD).t -> Ctx.Make(AD).t
end
