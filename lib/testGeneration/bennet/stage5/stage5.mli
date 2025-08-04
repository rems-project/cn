module Make (AD : GenTerms.Domain.T) : sig
  val transform
    :  Cerb_frontend.GenTypes.genTypeCategory Convert.A.sigma ->
    Stage4.Make(AD).Ctx.t ->
    Pp.document
end
