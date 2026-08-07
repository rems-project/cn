module CF = Cerb_frontend
module A = CF.AilSyntax

(* Re-export Smt functor at top level for external access (e.g., from stage7/convert.ml) *)
module Smt = Smt

module Make (AD : Domain.T) = struct
  module Convert = Convert.Make (AD)

  let transform
        (sigma : CF.GenTypes.genTypeCategory A.sigma)
        (prog5 : unit Mucore.file)
        (ctx : Stage5.Make(AD).Ctx.t)
    =
    Convert.transform sigma prog5 ctx
end
