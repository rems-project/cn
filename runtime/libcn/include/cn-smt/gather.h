#ifndef CN_SMT_GATHER_H
#define CN_SMT_GATHER_H

#include <stdbool.h>
#include <stddef.h>

#include <bennet/internals/rand.h>
#include <cn-smt/context.h>
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
bool cn_smt_gather_add_logical_term(cn_term* term);
bool cn_smt_gather_add_logical_forall(
    cn_sym var_name, cn_base_type var_type, cn_term* body);
bool cn_smt_gather_add_assignment(size_t bytes, cn_term* pointer, cn_term* value);
bool cn_smt_gather_add_substitution(uint64_t symbol_id, cn_term* term);
cn_term* cn_smt_gather_apply_substitutions(cn_term* term);

cn_term* cn_smt_gather_create_symbolic_var(const char* name, cn_base_type type);
cn_term* cn_smt_gather_call_function(
    cn_sym function_symbol, cn_term** args, size_t arg_count);

// PICK functionality - following BENNET_PICK pattern
uint64_t cn_smt_gather_weighted_choice(uint64_t* choices, size_t num_choices);

// Initialize gather context
#define CN_SMT_GATHER_INIT()                                                             \
  size_t bennet_rec_size = bennet_get_size();                                            \
  CN_SMT_GATHER_INIT_SIZED();

#define CN_SMT_GATHER_INIT_SIZED()                                                       \
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

// Assert macro - adds logical constraint to context
#define CN_SMT_GATHER_ASSERT(cond)                                                       \
  do {                                                                                   \
    cn_smt_gather_add_logical_term(cond);                                                \
  } while (0);

// Assign macros - add assignment constraints to context
#define CN_SMT_GATHER_ASSIGN(bytes, pointer_expr, value_term)                            \
  do {                                                                                   \
    cn_smt_gather_add_assignment(bytes, pointer_expr, value_term);                       \
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
#define CN_SMT_GATHER_CALL(last_var, function_symbol, ...)                               \
  ({                                                                                     \
    cn_term* var = cn_smt_gather_##function_symbol(__VA_ARGS__);                         \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                 \
                                                                                         \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
    var;                                                                                 \
  })

// Return values
#define CN_SMT_GATHER_RETURN(value) return (value);

// Weighted choice selection
#define CN_SMT_GATHER_PICK_BEGIN(var, tmp, last_var, ...)                                \
  cn_term* var = NULL;                                                                   \
  uint64_t tmp##_choices[] = {__VA_ARGS__, UINT64_MAX};                                  \
  uint8_t tmp##_num_choices = 0;                                                         \
  while (tmp##_choices[tmp##_num_choices] != UINT64_MAX) {                               \
    tmp##_num_choices += 2;                                                              \
  }                                                                                      \
  tmp##_num_choices /= 2;                                                                \
  struct bennet_int_urn* tmp##_urn = urn_from_array(tmp##_choices, tmp##_num_choices);   \
  bennet_checkpoint tmp##_checkpoint = bennet_checkpoint_save();                         \
  bennet_label_##tmp##_gen :;                                                            \
  cn_bits_u64* tmp = convert_to_cn_bits_u64(urn_remove(tmp##_urn));                      \
  if (0) {                                                                               \
    bennet_label_##tmp##_backtrack :;                                                    \
    bennet_checkpoint_restore(&tmp##_checkpoint);                                        \
    if (tmp##_urn->size != 0) {                                                          \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                 \
      bennet_failure_reset();                                                            \
      goto bennet_label_##tmp##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }                                                                                      \
  switch (convert_from_cn_bits_u64(tmp)) {                                               \
  /* Case per choice */

#define CN_SMT_GATHER_PICK_CASE_BEGIN(index) case index:;

#define CN_SMT_GATHER_PICK_CASE_END(var, e)                                              \
  var = e;                                                                               \
  break;

#define CN_SMT_GATHER_PICK_END(tmp)                                                      \
  default:                                                                               \
    printf("Invalid generated value");                                                   \
    assert(false);                                                                       \
    }                                                                                    \
    urn_free(tmp##_urn);

enum cn_smt_solver_result cn_smt_gather_model(struct cn_smt_solver* smt_solver);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_GATHER_H
