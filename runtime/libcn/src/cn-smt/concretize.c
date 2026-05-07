#include <assert.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include <bennet/internals/domains/sized.h>
#include <cn-smt/eval.h>
#include <cn-smt/from_smt.h>
#include <cn-smt/gather.h>
#include <cn-smt/memory/test_alloc.h>
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

  cn_sym sym = {.name = cn_test_strdup(name), .id = cn_concretize_var_count};

  sexp_t* sym_smt = translate_term(smt_solver, cn_smt_sym(sym, type));

  sexp_t* result_smt = get_expr(smt_solver, sym_smt);

  cn_term* result_tm = get_value(type, result_smt);

  return result_tm;
}

cn_term* cn_smt_concretize_arbitrary(cn_base_type type) {
  switch (type.tag) {
    case CN_BASE_LOC:
      return cn_smt_pointer(bennet_arbitrary_sized_uintptr_t_top());
    case CN_BASE_BITS: {
      cn_bits_info bits_info = cn_base_type_get_bits_info(type);
      if (bits_info.is_signed) {
        switch (bits_info.size_bits) {
          case 8:
            return cn_smt_bits(true, 8, bennet_arbitrary_sized_int8_t_top());
          case 16:
            return cn_smt_bits(true, 16, bennet_arbitrary_sized_int16_t_top());
          case 32:
            return cn_smt_bits(true, 32, bennet_arbitrary_sized_int32_t_top());
          case 64:
            return cn_smt_bits(true, 64, bennet_arbitrary_sized_int64_t_top());
          default:
            assert(false && "Unsupported signed bit width for concretize arbitrary");
            return NULL;
        }
      }
      switch (bits_info.size_bits) {
        case 8:
          return cn_smt_bits(false, 8, bennet_arbitrary_sized_uint8_t_top());
        case 16:
          return cn_smt_bits(false, 16, bennet_arbitrary_sized_uint16_t_top());
        case 32:
          return cn_smt_bits(false, 32, bennet_arbitrary_sized_uint32_t_top());
        case 64:
          return cn_smt_bits(false, 64, bennet_arbitrary_sized_uint64_t_top());
        default:
          assert(false && "Unsupported unsigned bit width for concretize arbitrary");
          return NULL;
      }
    }
    default:
      assert(false && "Unsupported base type for cn_smt_concretize_arbitrary");
      return NULL;
  }
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
