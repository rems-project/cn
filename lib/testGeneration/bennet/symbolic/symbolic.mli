module Make (AD : Domain.T) : sig
  val transform : Stage2.Make(AD).Ctx.t -> Pp.document
end
