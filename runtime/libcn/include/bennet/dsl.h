#ifndef BENNET_DSL_H
#define BENNET_DSL_H

#include <assert.h>
#include <stdlib.h>

#include <bennet/failure.h>

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

#define BENNET_UNIFORM(ty)                                                               \
  ({                                                                                     \
    ty* result;                                                                          \
    if (bennet_failure_get_failure_type() == BENNET_BACKTRACK_ALLOC) {                   \
      result = cast_cn_pointer_to_##ty(BENNET_ALLOC(convert_to_cn_bits_u64(0)));         \
    } else {                                                                             \
      result = bennet_uniform_##ty(0);                                                   \
    }                                                                                    \
    result;                                                                              \
  })

#define BENNET_ALLOC(sz)                                                                 \
  ({                                                                                     \
    cn_pointer* ptr;                                                                     \
    if (sz != 0) {                                                                       \
      ptr = bennet_alloc(sz);                                                            \
    } else {                                                                             \
      uint8_t null_in_every = get_null_in_every();                                       \
      if (bennet_failure_get_failure_type() != BENNET_BACKTRACK_ALLOC &&                 \
          bennet_rec_size <= 1) {                                                        \
        ptr = convert_to_cn_pointer(NULL);                                               \
      } else {                                                                           \
        ptr = bennet_alloc(sz);                                                          \
      }                                                                                  \
    }                                                                                    \
    ptr;                                                                                 \
  })

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
  if (convert_from_cn_pointer(pointer_val) == 0) {                                       \
    bennet_failure_blame((char*)#pointer);                                               \
    if (sizeof(addr_ty) > sizeof(intmax_t)) {                                            \
      bennet_failure_set_failure_type(BENNET_BACKTRACK_ALLOC);                           \
      bennet_failure_set_allocation_needed(sizeof(addr_ty));                             \
    } else {                                                                             \
      bennet_failure_set_failure_type(BENNET_BACKTRACK_ALLOC);                           \
      bennet_failure_set_allocation_needed(sizeof(intmax_t));                            \
    }                                                                                    \
    goto cn_label_##last_var##_backtrack;                                                \
  }                                                                                      \
  void* tmp##_ptr = convert_from_cn_pointer(addr);                                       \
  if (!bennet_alloc_check(tmp##_ptr, sizeof(addr_ty))) {                                 \
    bennet_failure_blame((char*)#pointer);                                               \
    size_t tmp##_size = (uintptr_t)tmp##_ptr + sizeof(addr_ty) -                         \
                        (uintptr_t)convert_from_cn_pointer(pointer_val);                 \
    bennet_failure_set_failure_type(BENNET_BACKTRACK_ALLOC);                             \
    bennet_failure_set_allocation_needed(tmp##_size);                                    \
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
        if (bennet_failure_get_allocation_needed() > 0) {                                \
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

#endif  // BENNET_DSL_H
