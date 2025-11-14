module Make (AD : Domain.T) : sig
  val transform_gt : Term.Make(AD).t -> Term.Make(AD).t
end
