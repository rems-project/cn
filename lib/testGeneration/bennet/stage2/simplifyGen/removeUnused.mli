module Make (AD : GenTerms.Domain.T) : sig
  val transform_gt : Term.Make(AD).t -> Term.Make(AD).t
end
