module Make (AD : Domain.T) : sig
  val transform
    :  string ->
    Sym.t list ->
    (Sym.t * Definition.Predicate.t) list ->
    Test.t list ->
    GenContext.Make(Term.Make(AD)).t
end
