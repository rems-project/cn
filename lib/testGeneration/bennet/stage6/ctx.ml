module Make (AD : Domain.T) = struct
  include GenContext.Make (Term.Make (AD))
end
