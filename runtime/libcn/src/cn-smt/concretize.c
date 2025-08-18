#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include <cn-smt/eval.h>
#include <cn-smt/from_smt.h>
#include <cn-smt/gather.h>
#include <cn-smt/solver.h>
#include <cn-smt/to_smt.h>

static size_t cn_concretize_var_count = 0;

void cn_smt_concretize_init(void) {
  cn_concretize_var_count = 0;
}

cn_term* cn_smt_concretize_lookup_symbolic_var(
    struct cn_smt_solver* smt_solver, const char* name, cn_base_type type) {
  // Increment the counter in the context
  cn_concretize_var_count++;

  cn_sym sym = {.name = strdup(name), .id = cn_concretize_var_count};

  sexp_t* sym_smt = translate_term(smt_solver, cn_smt_sym(sym, type));

  sexp_t* result_smt = get_expr(smt_solver, sym_smt);

  cn_term* result_tm = get_value(type, result_smt);

  return result_tm;
}
