module Make (AD : Domain.T) : sig
  val transform
    :  Cerb_frontend.GenTypes.genTypeCategory Cerb_frontend.AilSyntax.sigma ->
    string ->
    Sym.t list ->
    (Sym.t * Definition.Predicate.t) list ->
    Test.t list ->
    GenContext.Make(Term.Make(AD)).t
end
