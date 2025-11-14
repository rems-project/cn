module Make (AD : Domain.T) : sig
  val transform
    :  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
    unit Mucore.file ->
    Stage4.Make(AD).Ctx.t ->
    Pp.document
end
