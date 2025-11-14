module Private = struct
  module Ownership = Ownership
  module Interval = Interval_
  module WrappedInterval = WrappedInterval
  module NonRelational = NonRelational
end

module Make (GT : GenTerms.T) = struct
  let annotate (ctx : GenContext.Make(GT).t) : GenContext.Make(GT).t =
    let module AI = Interpreter.Make (GT) (GT.AD) in
    AI.annotate ctx
end

module Ownership : Domain.T = Ownership.Inner

module Interval : Domain.T = Interval_.Inner

module WrappedInterval : Domain.T = WrappedInterval.Inner

(** Create a product domain from a list of domains *)
let product_domains = Product.product_domains
