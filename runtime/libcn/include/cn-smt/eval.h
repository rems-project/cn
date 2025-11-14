#ifndef CN_SMT_EVAL_H
#define CN_SMT_EVAL_H

#include <stdbool.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <cn-smt/context.h>
#include <cn-smt/structs.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

// Struct handler definitions moved to structs.h

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

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_EVAL_H
