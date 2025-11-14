#ifndef BENNET_DSL_H
#define BENNET_DSL_H

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/dsl/assert.h>
#include <bennet/dsl/assign.h>
#include <bennet/dsl/backtrack.h>
#include <bennet/state/checkpoint.h>
#include <bennet/state/failure.h>

#define BENNET_CHECK_TIMEOUT()                                                           \
  if (bennet_get_input_timeout() != 0 &&                                                 \
      bennet_get_milliseconds() - bennet_get_input_timer() >                             \
          bennet_get_input_timeout()) {                                                  \
    bennet_failure_reset();                                                              \
    bennet_failure_set_failure_type(BENNET_FAILURE_TIMEOUT);                             \
    goto bennet_label_bennet_backtrack;                                                  \
  }

#define BENNET_INIT()                                                                    \
  size_t bennet_rec_size = bennet_get_size();                                            \
  BENNET_INIT_SIZED();

#define BENNET_INIT_SIZED()                                                              \
  if (0) {                                                                               \
  bennet_label_bennet_backtrack:                                                         \
    bennet_decrement_depth();                                                            \
    return NULL;                                                                         \
  }                                                                                      \
  BENNET_CHECK_TIMEOUT();                                                                \
  bennet_increment_depth();                                                              \
  if (bennet_rec_size <= 0 || bennet_get_depth() == bennet_max_depth()) {                \
    bennet_failure_set_failure_type(BENNET_FAILURE_DEPTH);                               \
    goto bennet_label_bennet_backtrack;                                                  \
  }

#define BENNET_ARBITRARY(cn_ty, c_ty)                                                    \
  ({                                                                                     \
    bennet_domain(c_ty)* domain = bennet_domain_top(c_ty);                               \
    bennet_arbitrary_##cn_ty(domain);                                                    \
  })

#define BENNET_ARBITRARY_POINTER() BENNET_ARBITRARY(cn_pointer, uintptr_t)

#define BENNET_ARBITRARY_UNSIGNED(bits) BENNET_ARBITRARY(cn_bits_u##bits, uint##bits##_t)

#define BENNET_ARBITRARY_SIGNED(bits) BENNET_ARBITRARY(cn_bits_i##bits, int##bits##_t)

#define BENNET_CALL(ty, last_var, ...)                                                   \
  ({                                                                                     \
    ty* var = __VA_ARGS__;                                                               \
    if (bennet_failure_get_failure_type() != BENNET_FAILURE_NONE) {                      \
      BENNET_CHECK_TIMEOUT();                                                            \
                                                                                         \
      if (bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH) {                   \
        bennet_failure_blame_many(path_vars);                                            \
      }                                                                                  \
                                                                                         \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
    var;                                                                                 \
  })

#define BENNET_ASSIGN(id, ptr, ptr_ty, addr, val_ty, value, last_var, ...)               \
  {                                                                                      \
    val_ty value_redir = value;                                                          \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assign(ptr_ty, id, ptr, addr, &value_redir, sizeof(val_ty), vars)) {      \
      bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                      \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
                                                                                         \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                              \
  }

#define BENNET_LET_ARBITRARY_DOMAIN(backtracks, cn_ty, c_ty, var, last_var, ...)         \
  bool var##_restore_randomness = false;                                                 \
  int var##_backtracks = backtracks;                                                     \
  bennet_checkpoint var##_checkpoint = bennet_checkpoint_save();                         \
  bennet_rand_checkpoint var##_rand_checkpoint_before = bennet_rand_save();              \
  bennet_rand_checkpoint var##_rand_checkpoint_after = NULL;                             \
                                                                                         \
  bennet_domain(c_ty)* var##_cs = __VA_ARGS__;                                           \
  bennet_domain(c_ty)* var##_cs_tmp = var##_cs;                                          \
                                                                                         \
  bennet_label_##var##_gen :;                                                            \
  cn_ty* var = bennet_arbitrary_##cn_ty(var##_cs_tmp);                                   \
                                                                                         \
  var##_cs_tmp = var##_cs;                                                               \
                                                                                         \
  if (var##_restore_randomness) {                                                        \
    bennet_rand_restore(var##_rand_checkpoint_after);                                    \
    var##_restore_randomness = false;                                                    \
  }                                                                                      \
  var##_rand_checkpoint_after = bennet_rand_save();                                      \
                                                                                         \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    bool var##_should_restore_randomness =                                               \
        bennet_failure_get_failure_type() == BENNET_FAILURE_ASSIGN;                      \
    bool var##_is_young = bennet_failure_is_young();                                     \
    if (bennet_backtrack_arbitrary_##cn_ty(                                              \
            &var##_backtracks, &var##_cs, &var##_cs_tmp, &var##_checkpoint, var)) {      \
      var##_restore_randomness = var##_should_restore_randomness;                        \
      if (!var##_restore_randomness) {                                                   \
        var##_restore_randomness =                                                       \
            !var##_is_young && !bennet_domain_equal(c_ty, var##_cs, var##_cs_tmp);       \
      }                                                                                  \
                                                                                         \
      goto bennet_label_##var##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_LET_ARBITRARY_DOMAIN_POINTER(backtracks, var, last_var, ...)              \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_pointer, uintptr_t, var, last_var, __VA_ARGS__)

#define BENNET_LET_ARBITRARY_DOMAIN_UNSIGNED(backtracks, bits, var, last_var, ...)       \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_bits_u##bits, uint##bits##_t, var, last_var, __VA_ARGS__)

#define BENNET_LET_ARBITRARY_DOMAIN_SIGNED(backtracks, bits, var, last_var, ...)         \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_bits_i##bits, int##bits##_t, var, last_var, __VA_ARGS__)

#define BENNET_LET_ARBITRARY(backtracks, cn_ty, c_ty, var, last_var)                     \
  BENNET_LET_ARBITRARY_DOMAIN(                                                           \
      backtracks, cn_ty, c_ty, var, last_var, bennet_domain_top(c_ty))

#define BENNET_LET_ARBITRARY_POINTER(backtracks, var, last_var)                          \
  BENNET_LET_ARBITRARY(backtracks, cn_pointer, uintptr_t, var, last_var)

#define BENNET_LET_ARBITRARY_UNSIGNED(backtracks, bits, var, last_var)                   \
  BENNET_LET_ARBITRARY(backtracks, cn_bits_u##bits, uint##bits##_t, var, last_var)

#define BENNET_LET_ARBITRARY_SIGNED(backtracks, bits, var, last_var)                     \
  BENNET_LET_ARBITRARY(backtracks, cn_bits_i##bits, int##bits##_t, var, last_var)

#define BENNET_LET_RETURN(ty, var, expr, last_var, ...)                                  \
  ty* var = expr;                                                                        \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    if (bennet_failure_is_blamed(var)) {                                                 \
      const void* toAdd[] = {__VA_ARGS__};                                               \
      bool is_young = bennet_failure_is_young();                                         \
      bennet_failure_remove_blame(var);                                                  \
      bennet_failure_blame_many(toAdd);                                                  \
      if (is_young) {                                                                    \
        bennet_failure_mark_young();                                                     \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    goto bennet_label_##last_var##_backtrack;                                            \
  }

#define BENNET_LET(backtracks, cn_ty, var, last_var, ...)                                \
  int var##_backtracks = backtracks;                                                     \
  bennet_checkpoint var##_checkpoint = bennet_checkpoint_save();                         \
  bennet_label_##var##_gen :;                                                            \
  cn_ty* var = __VA_ARGS__;                                                              \
                                                                                         \
  if (0) {                                                                               \
    bennet_label_##var##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
                                                                                         \
    if (bennet_backtrack(&var##_backtracks, &var##_checkpoint, var)) {                   \
      goto bennet_label_##var##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_MAP_BEGIN(map, i, i_ty, perm, max, last_var, ...)                         \
  cn_map* map = map_create();                                                            \
  {                                                                                      \
    i_ty* i = max;                                                                       \
                                                                                         \
    if (0) {                                                                             \
      bennet_label_##i##_backtrack :;                                                    \
      BENNET_CHECK_TIMEOUT();                                                            \
      if (bennet_failure_is_blamed(i)) {                                                 \
        const void* toAdd[] = {__VA_ARGS__};                                             \
        bennet_failure_remove_blame(i);                                                  \
        bennet_failure_blame_many(toAdd);                                                \
      }                                                                                  \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
                                                                                         \
    while (convert_from_cn_bool(perm)) {                                                 \
    /* Generate each item */
#define BENNET_MAP_END(map, i, i_ty, min, val)                                           \
  cn_map_set(map, cast_##i_ty##_to_cn_integer(i), val);                                  \
                                                                                         \
  if (convert_from_cn_bool(i_ty##_equality(i, min))) {                                   \
    break;                                                                               \
  }                                                                                      \
                                                                                         \
  i = i_ty##_sub(i, convert_to_##i_ty(1));                                               \
  }                                                                                      \
  }

#define BENNET_PICK_BEGIN(ty, var, tmp, last_var, ...)                                   \
  ty* var = NULL;                                                                        \
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
    BENNET_CHECK_TIMEOUT();                                                              \
    bennet_checkpoint_restore(&tmp##_checkpoint);                                        \
    bennet_failure_mark_old();                                                           \
    if ((bennet_failure_get_failure_type() == BENNET_FAILURE_ASSERT ||                   \
            bennet_failure_get_failure_type() == BENNET_FAILURE_DEPTH) &&                \
        tmp##_urn->size != 0) {                                                          \
      bennet_failure_reset();                                                            \
      goto bennet_label_##tmp##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }                                                                                      \
  switch (convert_from_cn_bits_u64(tmp)) {                                               \
  /* Case per choice */

#define BENNET_PICK_CASE_BEGIN(index) case index:

#define BENNET_PICK_CASE_END(var, e)                                                     \
  var = e;                                                                               \
  break;

#define BENNET_PICK_END(tmp)                                                             \
  default:                                                                               \
    printf("Invalid generated value");                                                   \
    assert(false);                                                                       \
    }                                                                                    \
    urn_free(tmp##_urn);

#define BENNET_SPLIT_BEGIN(tmp, ...)                                                     \
  void* tmp = malloc(1);                                                                 \
  int tmp##_backtracks = bennet_get_size_split_backtracks_allowed();                     \
  bennet_checkpoint tmp##_checkpoint = bennet_checkpoint_save();                         \
  bennet_label_##tmp##_gen : {                                                           \
    size_t* vars[] = {__VA_ARGS__};                                                      \
    int count = 0;                                                                       \
    for (int i = 0; vars[i] != NULL; i++) {                                              \
      count += 1;                                                                        \
    }

#define BENNET_SPLIT_END(tmp, last_var, ...)                                             \
  if (count >= bennet_rec_size) {                                                        \
    bennet_failure_set_failure_type(BENNET_FAILURE_DEPTH);                               \
    const void* toAdd[] = {__VA_ARGS__};                                                 \
    bennet_failure_blame_many(toAdd);                                                    \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
  bennet_split(bennet_rec_size - count - 1, vars, count);                                \
  for (int i = 0; i < count; i++) {                                                      \
    *(vars[i]) = *(vars[i]) + 1;                                                         \
  }                                                                                      \
  }                                                                                      \
  if (0) {                                                                               \
    bennet_label_##tmp##_backtrack :;                                                    \
    BENNET_CHECK_TIMEOUT();                                                              \
    if (bennet_failure_is_blamed(tmp)) {                                                 \
      bennet_checkpoint_restore(&tmp##_checkpoint);                                      \
      bennet_failure_remove_blame(tmp);                                                  \
      free(tmp);                                                                         \
                                                                                         \
      const void* toAdd[] = {__VA_ARGS__};                                               \
      bennet_failure_blame_many(toAdd);                                                  \
      if (tmp##_backtracks <= 0) {                                                       \
        goto bennet_label_##last_var##_backtrack;                                        \
      }                                                                                  \
      tmp##_backtracks--;                                                                \
      bennet_failure_reset();                                                            \
      goto bennet_label_##tmp##_gen;                                                     \
    } else {                                                                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#endif  // BENNET_DSL_H
