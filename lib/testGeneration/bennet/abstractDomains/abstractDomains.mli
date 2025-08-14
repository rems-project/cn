module Private : sig
  module Ownership = Ownership
  module Interval = Interval_
  module NonRelational = NonRelational
end

module Ownership : Domain.T

module Interval : Domain.T

module Make (GT : GenTerms.T) : sig
  val annotate : GenContext.Make(GT).t -> GenContext.Make(GT).t
end

(** Create a product domain from a list of domains *)
val product_domains : (module Domain.T) list -> (module Domain.T)
