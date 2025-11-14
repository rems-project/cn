module Make (AD : Domain.T) : sig
  val transform_gt : Term.Make(AD).t -> Term.Make(AD).t

  val transform : Ctx.Make(AD).t -> Ctx.Make(AD).t
end
