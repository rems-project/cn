#ifndef CN_SMT_CONCRETIZE_H
#define CN_SMT_CONCRETIZE_H

#include <stdbool.h>
#include <stddef.h>

#include <bennet/internals/rand.h>
#include <cn-smt/context.h>
#include <cn-smt/eval.h>
#include <cn-smt/solver.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

void cn_smt_concretize_init(void);

#define CN_SMT_CONCRETIZE_INIT()                                                         \
  size_t bennet_rec_size = bennet_get_size();                                            \
  CN_SMT_GATHER_INIT_SIZED();

#define CN_SMT_CONCRETIZE_INIT_SIZED()                                                   \
  if (0) {                                                                               \
  bennet_label_bennet_backtrack:                                                         \
    bennet_decrement_depth();                                                            \
    return NULL;                                                                         \
  }                                                                                      \
                                                                                         \
  bennet_increment_depth();                                                              \
  if (bennet_rec_size <= 0 || bennet_get_depth() == bennet_max_depth()) {                \
    bennet_failure_set_failure_type(BENNET_FAILURE_DEPTH);                               \
    goto bennet_label_bennet_backtrack;                                                  \
  }

void* cn_smt_concretize_lookup_symbolic_var(
    struct cn_smt_solver* smt_solver, const char* name, cn_base_type type);

#define CN_SMT_CONCRETIZE_ASSERT(cond)                                                   \
  do {                                                                                   \
    if (!convert_from_cn_bool(cn_eval_term(cond))) {                                     \
      cn_smt_gather_add_logical_term(cond);                                              \
                                                                                         \
      bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                      \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      return NULL;                                                                       \
    }                                                                                    \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                              \
  } while (0);

#define CN_SMT_CONCRETIZE_ASSIGN(ty, convert_from, pointer_term, value_term)             \
  do {                                                                                   \
    ty* cn_smt_concretize_addr =                                                         \
        (ty*)convert_from_cn_pointer(cn_eval_term(pointer_term));                        \
    size_t bytes = sizeof(ty);                                                           \
    if (!bennet_ownership_check(cn_smt_concretize_addr, bytes)) {                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
                                                                                         \
      return NULL;                                                                       \
    }                                                                                    \
                                                                                         \
    ty cn_smt_concretize_value = convert_from(cn_eval_term(value_term));                 \
    memcpy(cn_smt_concretize_addr, &cn_smt_concretize_value, bytes);                     \
    bennet_ownership_update(cn_smt_concretize_addr, bytes);                              \
  } while (0);

// Let symbolic variable lookup (nicer names)
#define CN_SMT_CONCRETIZE_LET_SYMBOLIC(symbol, base_type)                                \
  cn_term* symbol = cn_smt_concretize_lookup_symbolic_var(smt_solver, #symbol, base_type);

#define CN_SMT_CONCRETIZE_LET_STAR(symbol, term) cn_term* symbol = term;

#define CN_SMT_CONCRETIZE_SYMBOLIC(base_type)                                            \
  cn_smt_concretize_lookup_symbolic_var(smt_solver, "_sym", base_type)

#define CN_SMT_CONCRETIZE_CALL(function_symbol, ...)                                     \
  ({                                                                                     \
    cn_term* var =                                                                       \
        cn_smt_concretize_##function_symbol(smt_solver, branch_hist, __VA_ARGS__);       \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_ASSERT ||               \
             bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                 \
                                                                                         \
      goto bennet_label_bennet_backtrack;                                                \
    }                                                                                    \
    assert(var);                                                                         \
    var;                                                                                 \
  })

#define CN_SMT_CONCRETIZE_RETURN(value) return (value);

// PICK functionality for concretization - simplified version without backtracking
#define CN_SMT_CONCRETIZE_PICK_BEGIN(var)                                                \
  cn_term* var = NULL;                                                                   \
  switch (branch_history_next(branch_hist)) {                                            \
  /* Case per choice */

#define CN_SMT_CONCRETIZE_PICK_CASE_BEGIN(index) case index:;

#define CN_SMT_CONCRETIZE_PICK_CASE_END(var, e)                                          \
  var = e;                                                                               \
  break;

#define CN_SMT_CONCRETIZE_PICK_END()                                                     \
  case UINT64_MAX:                                                                       \
    fprintf(stderr, "\nRan out of choices\n");                                           \
    fflush(stderr);                                                                      \
    assert(false);                                                                       \
  default:                                                                               \
    fprintf(stderr, "\nInvalid generated value\n");                                      \
    fflush(stderr);                                                                      \
    assert(false);                                                                       \
    }

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_CONCRETIZE_H
