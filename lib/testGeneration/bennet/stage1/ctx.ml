module Make (AD : GenTerms.Domain.T) = struct
  include GenContext.Make (Term.Make (AD))
end
