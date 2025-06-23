#ifndef BENNET_EXP_DSL_H
#define BENNET_EXP_DSL_H

#include <assert.h>
#include <stdlib.h>

#include <bennet-exp/failure.h>

#define BENNET_CHECK_TIMEOUT()                                                           \
  if (bennet_get_input_timeout() != 0 &&                                                 \
      bennet_get_milliseconds() - bennet_get_input_timer() >                             \
          bennet_get_input_timeout()) {                                                  \
    bennet_failure_reset();                                                              \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_ASSERT);                            \
    goto cn_label_bennet_backtrack;                                                      \
  }

#define BENNET_INIT()                                                                    \
  size_t bennet_rec_size = bennet_get_size();                                            \
  BENNET_INIT_SIZED();

#define BENNET_INIT_SIZED()                                                              \
  if (0) {                                                                               \
  cn_label_bennet_backtrack:                                                             \
    bennet_decrement_depth();                                                            \
    return NULL;                                                                         \
  }                                                                                      \
  BENNET_CHECK_TIMEOUT();                                                                \
  bennet_increment_depth();                                                              \
  if (bennet_rec_size <= 0 || bennet_depth() == bennet_max_depth()) {                    \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_DEPTH);                             \
    goto cn_label_bennet_backtrack;                                                      \
  }

#define BENNET_UNIFORM(ty, var)                                                          \
  ({                                                                                     \
    ty* result;                                                                          \
    if (bennet_failure_get_failure_type() == BENNET_BACKTRACK_ALLOC) {                   \
      result = cast_cn_pointer_to_##ty(BENNET_ALLOC(var));                               \
    } else {                                                                             \
      result = bennet_uniform_##ty(0);                                                   \
    }                                                                                    \
    result;                                                                              \
  })

#define BENNET_ALLOC(var)                                                                \
  ({                                                                                     \
    cn_pointer* ptr;                                                                     \
                                                                                         \
    uint8_t null_in_every = get_null_in_every();                                         \
    if (is_sized_null()) {                                                               \
      set_null_in_every(bennet_rec_size);                                                \
    }                                                                                    \
    if (bennet_failure_get_failure_type() != BENNET_BACKTRACK_ALLOC &&                   \
        bennet_rec_size <= 1) {                                                          \
      ptr = convert_to_cn_pointer(NULL);                                                 \
    } else {                                                                             \
      if (bennet_failure_get_failure_type() == BENNET_BACKTRACK_ALLOC) {                 \
        size_t alt_lower_offset_bound = bennet_failure_get_lower_offset_bound();         \
        if (alt_lower_offset_bound > var##_lower_offset_bound) {                         \
          var##_lower_offset_bound = alt_lower_offset_bound;                             \
        }                                                                                \
        size_t alt_upper_offset_bound = bennet_failure_get_upper_offset_bound();         \
        if (alt_upper_offset_bound > var##_upper_offset_bound) {                         \
          var##_upper_offset_bound = alt_upper_offset_bound;                             \
        }                                                                                \
        bennet_failure_reset();                                                          \
      }                                                                                  \
      ptr = bennet_alloc(var##_lower_offset_bound, var##_upper_offset_bound, false);     \
    }                                                                                    \
    if (is_sized_null()) {                                                               \
      set_null_in_every(null_in_every);                                                  \
    }                                                                                    \
                                                                                         \
    ptr;                                                                                 \
  })

#define BENNET_LT_(ty, max) bennet_lt_##ty(max)

#define BENNET_GT_(ty, min) bennet_gt_##ty(min)

#define BENNET_LE_(ty, max) bennet_max_##ty(max)

#define BENNET_GE_(ty, min) bennet_min_##ty(min)

#define BENNET_RANGE(ty, min, max) bennet_range_##ty(min, max)

#define BENNET_MULT_RANGE(ty, mul, min, max) bennet_mult_range_##ty(mul, min, max)

#define BENNET_MULT(ty, mul) bennet_mult_##ty(mul)

#define BENNET_CALL_FROM(...)                                                            \
  {                                                                                      \
    char* from[] = {__VA_ARGS__, NULL};

#define BENNET_CALL_TO(...)                                                              \
  char* to[] = {__VA_ARGS__, NULL};                                                      \
  bennet_failure_remap_blamed_many(from, to);                                            \
  }

#define BENNET_CALL_PATH_VARS(...)                                                       \
  if (bennet_failure_get_failure_type() == BENNET_BACKTRACK_DEPTH) {                     \
    char* toAdd[] = {__VA_ARGS__, NULL};                                                 \
    bennet_failure_blame_many(toAdd);                                                    \
  }

#define BENNET_ASSIGN(                                                                   \
    pointer, pointer_val, addr, addr_ty, value, tmp, gen_name, last_var, ...)            \
  if (convert_from_cn_pointer(pointer_val) == NULL) {                                    \
    bennet_failure_blame((char*)#pointer);                                               \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_ALLOC);                             \
    if (sizeof(addr_ty) > sizeof(intmax_t)) {                                            \
      bennet_failure_set_offset_bounds(NULL, NULL, sizeof(addr_ty));                     \
    } else {                                                                             \
      bennet_failure_set_offset_bounds(NULL, NULL, sizeof(intmax_t));                    \
    }                                                                                    \
    goto cn_label_##last_var##_backtrack;                                                \
  }                                                                                      \
  void* tmp##_ptr = convert_from_cn_pointer(addr);                                       \
  if (!bennet_alloc_check(tmp##_ptr, sizeof(addr_ty))) {                                 \
    bennet_failure_blame((char*)#pointer);                                               \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_ALLOC);                             \
    bennet_failure_set_offset_bounds(                                                    \
        convert_from_cn_pointer(pointer_val), tmp##_ptr, sizeof(addr_ty));               \
    goto cn_label_##last_var##_backtrack;                                                \
  }                                                                                      \
  if (!bennet_ownership_check(tmp##_ptr, sizeof(addr_ty))) {                             \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_ASSERT);                            \
    char* toAdd[] = {__VA_ARGS__};                                                       \
    bennet_failure_blame_many(toAdd);                                                    \
    goto cn_label_##last_var##_backtrack;                                                \
  }                                                                                      \
  *(addr_ty*)tmp##_ptr = value;                                                          \
  bennet_ownership_update(tmp##_ptr, sizeof(addr_ty));

#define BENNET_LET_BEGIN(backtracks, var)                                                \
  int var##_backtracks = backtracks;                                                     \
  cn_bump_frame_id var##_checkpoint = cn_bump_get_frame_id();                            \
  void* var##_alloc_checkpoint = bennet_alloc_save();                                    \
  void* var##_ownership_checkpoint = bennet_ownership_save();                            \
  size_t var##_lower_offset_bound = 0;                                                   \
  size_t var##_upper_offset_bound = 0;                                                   \
  cn_label_##var##_gen :;

#define BENNET_LET_BODY(ty, var, gen)                                                    \
  bennet_rand_checkpoint var##_rand_checkpoint_before = bennet_rand_save();              \
  ty* var = gen;                                                                         \
  bennet_rand_checkpoint var##_rand_checkpoint_after = bennet_rand_save();

#define BENNET_LET_END(var, last_var, ...)                                               \
  if (bennet_failure_get_failure_type() != BENNET_BACKTRACK_NONE) {                      \
    cn_label_##var##_backtrack : BENNET_CHECK_TIMEOUT();                                 \
    cn_bump_free_after(var##_checkpoint);                                                \
    bennet_alloc_restore(var##_alloc_checkpoint);                                        \
    bennet_ownership_restore(var##_ownership_checkpoint);                                \
    if (bennet_failure_is_blamed((char*)#var)) {                                         \
      char* toAdd[] = {__VA_ARGS__};                                                     \
      bennet_failure_blame_many(toAdd);                                                  \
      if (var##_backtracks <= 0) {                                                       \
        goto cn_label_##last_var##_backtrack;                                            \
      }                                                                                  \
      if (bennet_failure_get_failure_type() == BENNET_BACKTRACK_ASSERT ||                \
          bennet_failure_get_failure_type() == BENNET_BACKTRACK_DEPTH) {                 \
        var##_backtracks--;                                                              \
        bennet_failure_reset();                                                          \
      } else if (bennet_failure_get_failure_type() == BENNET_BACKTRACK_ALLOC) {          \
        if (toAdd[0] != NULL) {                                                          \
          goto cn_label_##last_var##_backtrack;                                          \
        }                                                                                \
        if (!bennet_failure_get_should_be_null()) {                                      \
          bennet_rand_restore(var##_rand_checkpoint_after);                              \
        }                                                                                \
      }                                                                                  \
      goto cn_label_##var##_gen;                                                         \
    } else {                                                                             \
      goto cn_label_##last_var##_backtrack;                                              \
    }                                                                                    \
  }

#define BENNET_ASSERT(cond, last_var, ...)                                               \
  if (!convert_from_cn_bool(cond)) {                                                     \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_ASSERT);                            \
    char* toAdd[] = {__VA_ARGS__};                                                       \
    bennet_failure_blame_many(toAdd);                                                    \
    goto cn_label_##last_var##_backtrack;                                                \
  }

#define BENNET_MAP_BEGIN(map, i, i_ty, perm, max, last_var, ...)                         \
  cn_map* map = map_create();                                                            \
  {                                                                                      \
    if (0) {                                                                             \
      cn_label_##i##_backtrack : BENNET_CHECK_TIMEOUT();                                 \
      if (bennet_failure_is_blamed((char*)#i)) {                                         \
        char* toAdd[] = {__VA_ARGS__};                                                   \
        bennet_failure_blame_many(toAdd);                                                \
      }                                                                                  \
      goto cn_label_##last_var##_backtrack;                                              \
    }                                                                                    \
                                                                                         \
    i_ty* i = max;                                                                       \
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
  cn_bump_frame_id tmp##_checkpoint = cn_bump_get_frame_id();                            \
  void* tmp##_alloc_checkpoint = bennet_alloc_save();                                    \
  void* tmp##_ownership_checkpoint = bennet_ownership_save();                            \
  cn_label_##tmp##_gen :;                                                                \
  uint64_t tmp = urn_remove(tmp##_urn);                                                  \
  if (0) {                                                                               \
    cn_label_##tmp##_backtrack : BENNET_CHECK_TIMEOUT();                                 \
    cn_bump_free_after(tmp##_checkpoint);                                                \
    bennet_alloc_restore(tmp##_alloc_checkpoint);                                        \
    bennet_ownership_restore(tmp##_ownership_checkpoint);                                \
    if ((bennet_failure_get_failure_type() == BENNET_BACKTRACK_ASSERT ||                 \
            bennet_failure_get_failure_type() == BENNET_BACKTRACK_DEPTH) &&              \
        tmp##_urn->size != 0) {                                                          \
      bennet_failure_reset();                                                            \
      goto cn_label_##tmp##_gen;                                                         \
    } else {                                                                             \
      goto cn_label_##last_var##_backtrack;                                              \
    }                                                                                    \
  }                                                                                      \
  switch (tmp) {                                                                         \
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
  int tmp##_backtracks = bennet_get_size_split_backtracks_allowed();                     \
  cn_bump_frame_id tmp##_checkpoint = cn_bump_get_frame_id();                            \
  void* tmp##_alloc_checkpoint = bennet_alloc_save();                                    \
  void* tmp##_ownership_checkpoint = bennet_ownership_save();                            \
  cn_label_##tmp##_gen : {                                                               \
    size_t* vars[] = {__VA_ARGS__};                                                      \
    int count = 0;                                                                       \
    for (int i = 0; vars[i] != NULL; i++) {                                              \
      count += 1;                                                                        \
    }

#define BENNET_SPLIT_END(tmp, last_var, ...)                                             \
  if (count >= bennet_rec_size) {                                                        \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_DEPTH);                             \
    char* toAdd[] = {__VA_ARGS__};                                                       \
    bennet_failure_blame_many(toAdd);                                                    \
    goto cn_label_##last_var##_backtrack;                                                \
  }                                                                                      \
  bennet_split(bennet_rec_size - count - 1, vars, count);                                \
  for (int i = 0; i < count; i++) {                                                      \
    *(vars[i]) = *(vars[i]) + 1;                                                         \
  }                                                                                      \
  }                                                                                      \
  if (0) {                                                                               \
    cn_label_##tmp##_backtrack : BENNET_CHECK_TIMEOUT();                                 \
    cn_bump_free_after(tmp##_checkpoint);                                                \
    bennet_alloc_restore(tmp##_alloc_checkpoint);                                        \
    bennet_ownership_restore(tmp##_ownership_checkpoint);                                \
    if (bennet_failure_is_blamed(#tmp)) {                                                \
      char* toAdd[] = {__VA_ARGS__};                                                     \
      bennet_failure_blame_many(toAdd);                                                  \
      if (tmp##_backtracks <= 0) {                                                       \
        goto cn_label_##last_var##_backtrack;                                            \
      }                                                                                  \
      tmp##_backtracks--;                                                                \
      bennet_failure_reset();                                                            \
      goto cn_label_##tmp##_gen;                                                         \
    } else {                                                                             \
      goto cn_label_##last_var##_backtrack;                                              \
    }                                                                                    \
  }

#endif  // BENNET_EXP_DSL_H
