module Make (AD : Domain.T) : sig
  val transform : unit Mucore.file -> Stage3.Make(AD).Ctx.t -> Pp.document
end
