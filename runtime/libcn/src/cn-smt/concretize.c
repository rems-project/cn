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

/**
 * Global variable to control use of solver for evaluation.
 * When true, enables solver-based evaluation during concretization.
 */
bool cn_use_solver_eval = false;

void cn_smt_concretize_init(void) {
  cn_concretize_var_count = 0;
}

bool cn_get_use_solver_eval(void) {
  return cn_use_solver_eval;
}

void cn_set_use_solver_eval(bool value) {
  cn_use_solver_eval = value;
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

void* cn_smt_concretize_eval_term(struct cn_smt_solver* smt_solver, cn_term* term) {
  if (cn_use_solver_eval) {
    // For constants, we can evaluate directly without querying the solver
    // This avoids expensive solver queries and potential timeouts
    if (term->type == CN_TERM_CONST) {
      return cn_eval_term(term);
    }

    // For terms with symbolic variables, use the solver to evaluate them
    sexp_t* term_smt = translate_term(smt_solver, term);
    sexp_t* result_smt = eval_expr(smt_solver, term_smt);
    cn_term* result_term = get_value(term->base_type, result_smt);
    return cn_eval_term(result_term);
  }

  return cn_eval_term(term);
}
