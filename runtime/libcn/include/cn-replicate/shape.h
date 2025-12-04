#ifndef CN_REPLICA_SHAPE_H
#define CN_REPLICA_SHAPE_H

#include <stddef.h>
#include <string.h>

#include <cn-executable/eval.h>

#ifdef __cplusplus
extern "C" {
#endif

void cn_replica_alloc_reset(void);

// First Pass //
void cn_analyze_shape_owned(void* ptr, size_t sz);

#define CN_ANALYZE_SHAPE_EACH_BEGIN(map, i, i_ty, perm, min)                             \
  cn_map* map = map_create();                                                            \
  {                                                                                      \
    i_ty* i = min;                                                                       \
    while (convert_from_cn_bool(perm)) {                                                 \
    /* Inner analyzer */

#define CN_ANALYZE_SHAPE_EACH_END(map, i, i_ty, val, max)                                \
  cn_map_set(map, cast_##i_ty##_to_cn_integer(i), val);                                  \
                                                                                         \
  if (convert_from_cn_bool(i_ty##_equality(i, max))) {                                   \
    break;                                                                               \
  }                                                                                      \
                                                                                         \
  i_ty##_increment(i);                                                                   \
  }                                                                                      \
  }

// Second Pass //
void cn_replicate_owned(char* addr_str, char* value_str);

char* cn_replicate_owned_cn_pointer_aux(cn_pointer* p);

char* cn_replicate_owned_cn_bits_i8_aux(cn_pointer* p);
char* cn_replicate_owned_cn_bits_i16_aux(cn_pointer* p);
char* cn_replicate_owned_cn_bits_i32_aux(cn_pointer* p);
char* cn_replicate_owned_cn_bits_i64_aux(cn_pointer* p);

char* cn_replicate_owned_cn_bits_u8_aux(cn_pointer* p);
char* cn_replicate_owned_cn_bits_u16_aux(cn_pointer* p);
char* cn_replicate_owned_cn_bits_u32_aux(cn_pointer* p);
char* cn_replicate_owned_cn_bits_u64_aux(cn_pointer* p);

#define cn_replicate_owned_array_aux(member_owned_aux, arr, n)                           \
  ({                                                                                     \
    char* buf = malloc(5);                                                               \
    snprintf(buf, 5, "{ ");                                                              \
    for (int i = 0; i < n; i++) {                                                        \
      char* mem_buf = member_owned_aux(convert_to_cn_pointer(&arr[i]));                  \
      buf = realloc(buf, strlen(buf) + strlen(mem_buf) + 1);                             \
      strcat(buf, mem_buf);                                                              \
      free(mem_buf);                                                                     \
    }                                                                                    \
    buf = realloc(buf, strlen(buf) + strlen(" }") + 1);                                  \
    strcat(buf, " }");                                                                   \
    buf;                                                                                 \
  })

#define CN_REPLICATE_EACH_BEGIN(map, i, i_ty, perm, min)                                 \
  CN_ANALYZE_SHAPE_EACH_BEGIN(map, i, i_ty, perm, min)

#define CN_REPLICATE_EACH_END(map, i, i_ty, val, max)                                    \
  CN_ANALYZE_SHAPE_EACH_END(map, i, i_ty, val, max)

#ifdef __cplusplus
}
#endif

#endif /* CN_REPLICA_SHAPE_H */
