#ifndef CN_SMT_EVAL_H
#define CN_SMT_EVAL_H

#include <stdbool.h>

#include <cn-smt/context.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Evaluates a cn_term and returns the result as a Fulminate value.
 *
 * This function walks the cn_term AST and computes a concrete value using the
 * Fulminate runtime functions. The caller is responsible for knowing the
 * expected return type and casting the void* pointer accordingly.
 *
 * @param term The cn_term to evaluate.
 * @return A pointer to a Fulminate value (e.g., cn_integer*, cn_bool*), or NULL
 *         if the term cannot be evaluated.
 */
void* cn_eval_term(cn_term* term);

/**
 * @brief Evaluates a constraint context and returns whether it is satisfiable.
 *
 * This function evaluates all logical constraints in the context and determines
 * if they are collectively satisfied. For resource constraints, it checks that
 * the constraints are consistent (e.g., no overlapping memory regions).
 *
 * @param ctx The constraint context to evaluate.
 * @return true if the context is satisfiable (all constraints are satisfied),
 *         false otherwise.
 */
bool cn_eval_context(cn_constraint_context* ctx);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_EVAL_H
