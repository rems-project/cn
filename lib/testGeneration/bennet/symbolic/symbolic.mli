(* Re-export Smt functor for external access *)
module Smt = Smt

module Make (AD : Domain.T) : sig
  val transform
    :  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
    unit Mucore.file ->
    Stage5.Make(AD).Ctx.t ->
    Pp.document
end
