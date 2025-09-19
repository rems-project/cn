#ifndef CN_SMT_GATHER_H
#define CN_SMT_GATHER_H

#include <stdbool.h>
#include <stddef.h>

#include <bennet/internals/rand.h>
#include <cn-smt/branch_history.h>
#include <cn-smt/context.h>
#include <cn-smt/solver.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
struct cn_smt_solver;

// Forward declarations for functions used by macros
void cn_smt_gather_init(void);
bool cn_smt_gather_is_initialized(void);
cn_constraint_context* cn_smt_gather_get_context(void);
void cn_smt_gather_cleanup(void);
void cn_smt_gather_reset(void);
size_t cn_smt_gather_resource_count(void);
size_t cn_smt_gather_logical_count(void);
void cn_smt_gather_add_logical_term(cn_term* term);
void cn_smt_gather_add_logical_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body);
void cn_smt_gather_add_assignment(cn_term* pointer, size_t bytes, size_t alignment);
void cn_smt_gather_add_array_assignment(
    cn_term* start_addr, cn_term* end_addr, size_t alignment);

cn_term* cn_smt_gather_create_symbolic_var(const char* name, cn_base_type type);
cn_term* cn_smt_gather_call_function(
    cn_sym function_symbol, cn_term** args, size_t arg_count);

// PICK functionality - following BENNET_PICK pattern
uint64_t cn_smt_gather_weighted_choice(uint64_t* choices, size_t num_choices);

// Assert macro - adds logical constraint to context
#define CN_SMT_GATHER_ASSERT(cond)                                                       \
  do {                                                                                   \
    cn_smt_gather_add_logical_term(cond);                                                \
  } while (0);

// Assign macros - add assignment constraints to context
#define CN_SMT_GATHER_ASSIGN(ty, pointer_expr)                                           \
  do {                                                                                   \
    cn_smt_gather_add_assignment(pointer_expr, sizeof(ty), alignof(ty));                 \
  } while (0);

// Array assign macros - add array constraints to context
#define CN_SMT_GATHER_ASSIGN_ARRAY(ty, start, end)                                       \
  do {                                                                                   \
    cn_smt_gather_add_array_assignment(start, end, alignof(ty));                         \
  } while (0);

// Let symbolic variable creation (nicer names)
#define CN_SMT_GATHER_LET_SYMBOLIC(symbol, base_type)                                    \
  cn_term* symbol = cn_smt_gather_create_symbolic_var(#symbol, base_type);

// Let macro - adds substitution to context
#define CN_SMT_GATHER_LET_STAR(symbol, term) cn_term* symbol = term;

// Symbolic variable creation
#define CN_SMT_GATHER_SYMBOLIC(base_type)                                                \
  cn_smt_gather_create_symbolic_var("_sym", base_type)

// Function calls
#define CN_SMT_GATHER_CALL(function_symbol, ...)                                         \
  cn_smt_gather_##function_symbol(branch_hist, __VA_ARGS__)

// Return values
#define CN_SMT_GATHER_RETURN(value) return (value);

// Weighted choice selection
#define CN_SMT_GATHER_PICK_BEGIN(var)                                                    \
  cn_term* var = NULL;                                                                   \
  switch (branch_history_next(branch_hist)) {                                            \
  /* Case per choice */

#define CN_SMT_GATHER_PICK_CASE_BEGIN(index) case index:;

#define CN_SMT_GATHER_PICK_CASE_END(var, e)                                              \
  var = e;                                                                               \
  break;

#define CN_SMT_GATHER_PICK_END()                                                         \
  case UINT64_MAX:                                                                       \
    fprintf(stderr, "\nRan out of choices\n");                                           \
    fflush(stderr);                                                                      \
    assert(false);                                                                       \
  default:                                                                               \
    fprintf(stderr, "\nInvalid generated value\n");                                      \
    fflush(stderr);                                                                      \
    assert(false);                                                                       \
    }

enum cn_smt_solver_result cn_smt_gather_model(struct cn_smt_solver* smt_solver);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_GATHER_H
