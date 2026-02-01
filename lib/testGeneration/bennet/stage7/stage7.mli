module Make (AD : Domain.T) : sig
  val transform
    :  Cerb_frontend.GenTypes.genTypeCategory Convert.A.sigma ->
    unit Mucore.file ->
    Stage6.Make(AD).Ctx.t ->
    Pp.document
end
