module Make (AD : GenTerms.Domain.T) = struct
  include GenDefinitions.Make (Term.Make (AD))
end
