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
    cn_smt_path_selector_##function_symbol(branch_hist, unsat_paths);                    \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                 \
                                                                                         \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  })

// Weighted choice selection
#define CN_SMT_PATH_SELECTOR_PICK_BEGIN(var, tmp, last_var, ...)                            \
  uint64_t tmp##_choices[] = {__VA_ARGS__, UINT64_MAX};                                     \
  uint8_t tmp##_num_choices = 0;                                                            \
  while (tmp##_choices[tmp##_num_choices] != UINT64_MAX) {                                  \
    if (cn_smt_pruning_at_runtime) { /* Check if this choice leads to an unsat path */      \
      uint64_t choice_value = tmp##_choices[tmp##_num_choices + 1];                         \
      cn_trie_node* current_node = unsat_paths->root;                                       \
      /* Navigate through current branch history */                                         \
      struct branch_history_node* hist_curr = branch_hist->head;                            \
      while (hist_curr != NULL && current_node != NULL) {                                   \
        current_node = cn_trie_get_subtrie(current_node, hist_curr->data);                  \
        hist_curr = hist_curr->next;                                                        \
      }                                                                                     \
      /* Check if adding this choice creates an unsat path */                               \
      if (current_node != NULL && cn_trie_is_leaf(current_node, choice_value)) {            \
        tmp##_choices[tmp##_num_choices] = 0; /* Set weight to zero */                      \
      }                                                                                     \
    }                                                                                       \
    tmp##_num_choices += 2;                                                                 \
  }                                                                                         \
  tmp##_num_choices /= 2;                                                                   \
                                                                                            \
  struct bennet_int_urn* tmp##_urn = urn_from_array(tmp##_choices, tmp##_num_choices);      \
                                                                                            \
  if (cn_smt_pruning_at_runtime) { /* All choices lead to unsat paths, optimize the trie */ \
    if (tmp##_urn->tree == NULL) {                                                          \
      cn_trie_node* unsat_node = unsat_paths->root;                                         \
      struct branch_history_node* hist_curr = branch_hist->head;                            \
      while (hist_curr != NULL && unsat_node != NULL) {                                     \
        unsat_node = cn_trie_get_subtrie(unsat_node, hist_curr->data);                      \
        hist_curr = hist_curr->next;                                                        \
      }                                                                                     \
      if (unsat_node != NULL) {                                                             \
        cn_trie_node_clear_children(unsat_node);                                            \
        cn_trie_node_set_terminal(unsat_node);                                              \
      }                                                                                     \
      bennet_failure_set_failure_type(                                                      \
          BENNET_FAILURE_DEPTH); /* Technically not, but... */                              \
      goto bennet_label_##last_var##_backtrack;                                             \
    }                                                                                       \
  } else {                                                                                  \
    assert(tmp##_urn->tree != NULL);                                                        \
  }                                                                                         \
                                                                                            \
  branch_history_checkpoint tmp##_branch_checkpoint =                                       \
      branch_history_checkpoint_current(branch_hist);                                       \
  bennet_label_##tmp##_gen :;                                                               \
  uint64_t tmp = urn_remove(tmp##_urn);                                                     \
  branch_history_record(branch_hist, tmp);                                                  \
  if (0) {                                                                                  \
    bennet_label_##tmp##_backtrack :;                                                       \
    branch_history_restore(branch_hist, tmp##_branch_checkpoint);                           \
    if (tmp##_urn->size != 0) {                                                             \
      assert(bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH);                    \
      bennet_failure_reset();                                                               \
      goto bennet_label_##tmp##_gen;                                                        \
    } else {                                                                                \
      goto bennet_label_##last_var##_backtrack;                                             \
    }                                                                                       \
  }                                                                                         \
  switch (tmp) {                                                                            \
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
