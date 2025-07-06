(** This stage tags the AST with backtracking information *)

module Term = Term
module Def = Def
module Ctx = Ctx

let transform (ctx : Stage4.Ctx.t) : Ctx.t = Convert.transform ctx

(** TODO: Clean up *)
type bound_type = Abstract.bound_type
