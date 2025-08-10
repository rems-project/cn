module Ownership : Domain.T

module Make (GT : GenTerms.T) : sig
  val annotate : GenContext.Make(GT).t -> GenContext.Make(GT).t
end
