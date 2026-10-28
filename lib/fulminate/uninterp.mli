val u_fn_syms : Sym.Set.t ref

val u_pred_syms : Sym.Set.t ref

val set_uninterp_closure
  :  (Sym.t * Definition.Function.t) list ->
  (Sym.t * Definition.Predicate.t) list ->
  unit

val accept_uninterp_instr : Extract.instrumentation list -> Extract.instrumentation list
