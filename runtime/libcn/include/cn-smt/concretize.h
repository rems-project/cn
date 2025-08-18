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

#define CN_SMT_CONCRETIZE_INIT() cn_smt_concretize_init();

void* cn_smt_concretize_lookup_symbolic_var(
    struct cn_smt_solver* smt_solver, const char* name, cn_base_type type);

#define CN_SMT_CONCRETIZE_ASSERT(cond)                                                   \
  do {                                                                                   \
    if (!convert_from_cn_bool(cn_eval_term(cond))) {                                     \
      cn_smt_gather_add_logical_term(cond);                                              \
                                                                                         \
      return NULL;                                                                       \
    }                                                                                    \
  } while (0);

#define CN_SMT_CONCRETIZE_ASSIGN(ty, convert_from, pointer_term, value_term)             \
  do {                                                                                   \
    ty* cn_smt_concretize_addr =                                                         \
        (ty*)convert_from_cn_pointer(cn_eval_term(pointer_term));                        \
    ty cn_smt_concretize_value = convert_from(cn_eval_term(value_term));                 \
    memcpy(cn_smt_concretize_addr, &cn_smt_concretize_value, sizeof(ty));                \
  } while (0);

// Let symbolic variable lookup (nicer names)
#define CN_SMT_CONCRETIZE_LET_SYMBOLIC(symbol, base_type)                                \
  cn_term* symbol = cn_smt_concretize_lookup_symbolic_var(smt_solver, #symbol, base_type);

#define CN_SMT_CONCRETIZE_LET_STAR(symbol, term) cn_term* symbol = term;

#define CN_SMT_CONCRETIZE_SYMBOLIC(base_type)                                            \
  cn_smt_concretize_lookup_symbolic_var(smt_solver, "_sym", base_type)

#define CN_SMT_CONCRETIZE_CALL(function_symbol, ...)                                     \
  cn_smt_concretize_##function_symbol(smt_solver, __VA_ARGS__)

#define CN_SMT_CONCRETIZE_RETURN(value) return (value);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_CONCRETIZE_H
