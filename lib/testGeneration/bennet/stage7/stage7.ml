(** Stage 7: C code generation *)

module Make (AD : Domain.T) = struct
  open struct
    module Convert = Convert.Make (AD)
  end

  let transform sigma prog5 ctx = Convert.transform sigma prog5 ctx
end
