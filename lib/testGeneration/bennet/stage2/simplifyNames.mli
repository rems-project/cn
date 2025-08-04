module Make (AD : GenTerms.Domain.T) : sig
  val transform : Ctx.Make(AD).t -> Ctx.Make(AD).t
end
