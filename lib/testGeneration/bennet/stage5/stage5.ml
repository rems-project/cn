(** This stage generates C code *)

module Make (AD : GenTerms.Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
  end

  let transform sigma ctx = Convert.transform sigma ctx
end
