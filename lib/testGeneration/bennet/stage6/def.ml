module Make (AD : Domain.T) = struct
  include GenDefinitions.Make (Term.Make (AD))
end
