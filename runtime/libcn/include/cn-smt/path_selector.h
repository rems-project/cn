#ifndef CN_SMT_PATH_SELECTOR_H
#define CN_SMT_PATH_SELECTOR_H

#include <stdbool.h>
#include <stddef.h>

#include <bennet/internals/rand.h>
#include <cn-smt/branch_history.h>
#include <cn-smt/context.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

// Initialize path selector context
#define CN_SMT_PATH_SELECTOR_INIT()                                                      \
  size_t bennet_rec_size = bennet_get_size();                                            \
  CN_SMT_PATH_SELECTOR_INIT_SIZED();

#define CN_SMT_PATH_SELECTOR_INIT_SIZED()                                                \
  if (0) {                                                                               \
  bennet_label_bennet_backtrack:                                                         \
    bennet_decrement_depth();                                                            \
    return;                                                                              \
  }                                                                                      \
                                                                                         \
  bennet_increment_depth();                                                              \
  if (bennet_rec_size <= 0 || bennet_get_depth() == bennet_max_depth()) {                \
    bennet_failure_set_failure_type(BENNET_FAILURE_DEPTH);                               \
    goto bennet_label_bennet_backtrack;                                                  \
  }

// Function calls
#define CN_SMT_PATH_SELECTOR_CALL(last_var, function_symbol)                             \
  ({                                                                                     \
    cn_smt_path_selector_##function_symbol(branch_hist);                                 \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                 \
                                                                                         \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  })

// Weighted choice selection
#define CN_SMT_PATH_SELECTOR_PICK_BEGIN(var, tmp, last_var, ...)                         \
  uint64_t tmp##_choices[] = {__VA_ARGS__, UINT64_MAX};                                  \
  uint8_t tmp##_num_choices = 0;                                                         \
  while (tmp##_choices[tmp##_num_choices] != UINT64_MAX) {                               \
    tmp##_num_choices += 2;                                                              \
  }                                                                                      \
  tmp##_num_choices /= 2;                                                                \
  struct bennet_int_urn* tmp##_urn = urn_from_array(tmp##_choices, tmp##_num_choices);   \
  branch_history_checkpoint tmp##_branch_checkpoint =                                    \
      branch_history_checkpoint_current(branch_hist);                                    \
  bennet_label_##tmp##_gen :;                                                            \
  uint64_t tmp = urn_remove(tmp##_urn);                                                  \
  branch_history_record(branch_hist, tmp);                                               \
  if (0) {                                                                               \
    bennet_label_##tmp##_backtrack :;                                                    \
    branch_history_restore(branch_hist, tmp##_branch_checkpoint);                        \
    if (tmp##_urn->size != 0) {                                                          \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                 \
      bennet_failure_reset();                                                            \
      goto bennet_label_##tmp##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }                                                                                      \
  switch (tmp) {                                                                         \
  /* Case per choice */

#define CN_SMT_PATH_SELECTOR_PICK_CASE_BEGIN(index) case index:;

#define CN_SMT_PATH_SELECTOR_PICK_CASE_END() break;

#define CN_SMT_PATH_SELECTOR_PICK_END(tmp)                                               \
  default:                                                                               \
    printf("Invalid generated value");                                                   \
    assert(false);                                                                       \
    }                                                                                    \
    urn_free(tmp##_urn);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_PATH_SELECTOR_H
