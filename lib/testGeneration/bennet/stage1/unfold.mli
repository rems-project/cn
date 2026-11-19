module Make (AD : Domain.T) : sig
  val unfold : int -> Ctx.Make(AD).t -> Ctx.Make(AD).t
end
