module Private = struct
  module Ownership = Ownership
  module Interval = Interval_
  module NonRelational = NonRelational
end

module Make (GT : GenTerms.T) = struct
  let annotate (ctx : GenContext.Make(GT).t) : GenContext.Make(GT).t =
    (* Since interpretation functionality is now part of Domain.T, 
       we can directly use GT.AD which implements the full Domain.T interface *)
    let module AI = Interpreter.Make (GT) (GT.AD) in
    AI.annotate ctx
end

module Ownership : Domain.T = Ownership.Inner

module Interval : Domain.T = Interval_.Inner

(** Create a product domain from a list of domains *)
let product_domains = Product.product_domains
