#ifndef CN_EVAL_H
#define CN_EVAL_H

#include "bump_alloc.h"
#include "hash_table.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Wrappers for C types */

/* Signed bitvectors */

typedef struct cn_bits_i8 {
  int8_t val;
} cn_bits_i8;

typedef struct cn_bits_i16 {
  int16_t val;
} cn_bits_i16;

typedef struct cn_bits_i32 {
  int32_t val;
} cn_bits_i32;

typedef struct cn_bits_i64 {
  int64_t val;
} cn_bits_i64;

/* Unsigned bitvectors */

typedef struct cn_bits_u8 {
  uint8_t val;
} cn_bits_u8;

typedef struct cn_bits_u16 {
  uint16_t val;
} cn_bits_u16;

typedef struct cn_bits_u32 {
  uint32_t val;
} cn_bits_u32;

typedef struct cn_bits_u64 {
  uint64_t val;
} cn_bits_u64;

typedef struct cn_integer {
  int64_t val;
} cn_integer;

typedef struct cn_pointer {
  void* ptr;
} cn_pointer;

typedef struct cn_bool {
  bool val;
} cn_bool;

typedef struct cn_alloc_id {
  uint8_t val;
} cn_alloc_id;

typedef hash_table cn_map;

/* Conversion functions */

cn_bool* convert_to_cn_bool(bool b);
bool convert_from_cn_bool(cn_bool* b);
cn_bool* cn_bool_and(cn_bool* b1, cn_bool* b2);
cn_bool* cn_bool_or(cn_bool* b1, cn_bool* b2);
cn_bool* cn_bool_not(cn_bool* b);
cn_bool* cn_bool_implies(cn_bool* b1, cn_bool* b2);
cn_bool* cn_bool_equality(cn_bool* b1, cn_bool* b2);
void* cn_ite(cn_bool* b, void* e1, void* e2);

cn_map* map_create(void);
cn_map* cn_map_set(cn_map* m, cn_integer* key, void* value);
cn_map* cn_map_deep_copy(cn_map* m1);
cn_bool* cn_map_equality(
    cn_map* m1, cn_map* m2, cn_bool*(value_equality_fun)(void*, void*));

#define convert_to_cn_map(c_ptr, cntype_conversion_fn, num_elements)                     \
  ({                                                                                     \
    cn_map* m = map_create();                                                            \
    for (int i = 0; i < num_elements; i++) {                                             \
      cn_map_set(m, convert_to_cn_integer(i), cntype_conversion_fn((c_ptr)[i]));         \
    }                                                                                    \
    m;                                                                                   \
  })
#define convert_from_cn_map(arr, m, cntype, num_elements)                                \
  for (int i = 0; i < num_elements; i++) {                                               \
    (arr)[i] = convert_from_##cntype(cn_map_get_##cntype(m, convert_to_cn_integer(i)));  \
  }

cn_bool* cn_pointer_equality(void* i1, void* i2);
cn_bool* cn_pointer_is_null(cn_pointer*);
cn_bool* cn_pointer_le(cn_pointer* i1, cn_pointer* i2);
cn_bool* cn_pointer_lt(cn_pointer* i1, cn_pointer* i2);
cn_bool* cn_pointer_ge(cn_pointer* i1, cn_pointer* i2);
cn_bool* cn_pointer_gt(cn_pointer* i1, cn_pointer* i2);

#define cn_pointer_deref(CN_PTR, CTYPE) *((CTYPE*)CN_PTR->ptr)

/* CN integer type auxilliary functions */

#define CN_GEN_EQUALITY(CNTYPE) CN_GEN_EQUALITY_(CNTYPE)

#define CN_GEN_EQUALITY_(CNTYPE)                                                         \
  static inline cn_bool* CNTYPE##_equality(void* i1, void* i2) {                         \
    return convert_to_cn_bool(((CNTYPE*)i1)->val == ((CNTYPE*)i2)->val);                 \
  }

#define CN_GEN_CONVERT(CTYPE, CNTYPE)                                                    \
  static inline CNTYPE* convert_to_##CNTYPE(CTYPE i) {                                   \
    CNTYPE* ret = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    ret->val = i;                                                                        \
    return ret;                                                                          \
  }

#define CN_GEN_CONVERT_FROM(CTYPE, CNTYPE)                                               \
  static inline CTYPE convert_from_##CNTYPE(CNTYPE* i) {                                 \
    return i->val;                                                                       \
  }

/* Arithmetic operators */

#define CN_GEN_LT(CNTYPE)                                                                \
  static inline cn_bool* CNTYPE##_lt(CNTYPE* i1, CNTYPE* i2) {                           \
    return convert_to_cn_bool(i1->val < i2->val);                                        \
  }

#define CN_GEN_LE(CNTYPE)                                                                \
  static inline cn_bool* CNTYPE##_le(CNTYPE* i1, CNTYPE* i2) {                           \
    return convert_to_cn_bool(i1->val <= i2->val);                                       \
  }

#define CN_GEN_GT(CNTYPE)                                                                \
  static inline cn_bool* CNTYPE##_gt(CNTYPE* i1, CNTYPE* i2) {                           \
    return convert_to_cn_bool(i1->val > i2->val);                                        \
  }

#define CN_GEN_GE(CNTYPE)                                                                \
  static inline cn_bool* CNTYPE##_ge(CNTYPE* i1, CNTYPE* i2) {                           \
    return convert_to_cn_bool(i1->val >= i2->val);                                       \
  }

#define CN_GEN_NEGATE(CNTYPE)                                                            \
  static inline CNTYPE* CNTYPE##_negate(CNTYPE* i) {                                     \
    return convert_to_##CNTYPE(-(i->val));                                               \
  }

#define CN_GEN_BW_COMPL(CNTYPE)                                                          \
  static inline CNTYPE* CNTYPE##_bw_compl(CNTYPE* i) {                                   \
    return convert_to_##CNTYPE(~(i->val));                                               \
  }

#define CN_GEN_ADD(UCTYPE, CTYPE, CNTYPE)                                                \
  static inline CNTYPE* CNTYPE##_add(CNTYPE* i1, CNTYPE* i2) {                           \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = (UCTYPE)i1->val + (UCTYPE)i2->val;                                        \
    return res;                                                                          \
  }

#define CN_GEN_SUB(UCTYPE, CTYPE, CNTYPE)                                                \
  static inline CNTYPE* CNTYPE##_sub(CNTYPE* i1, CNTYPE* i2) {                           \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = (UCTYPE)i1->val - (UCTYPE)i2->val;                                        \
    return res;                                                                          \
  }

#define CN_GEN_MUL(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE* CNTYPE##_multiply(CNTYPE* i1, CNTYPE* i2) {                      \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val * i2->val;                                                        \
    return res;                                                                          \
  }

#define CN_GEN_DIV(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE* CNTYPE##_divide(CNTYPE* i1, CNTYPE* i2) {                        \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val / i2->val;                                                        \
    return res;                                                                          \
  }

#define CN_GEN_SHIFT_LEFT(CTYPE, CNTYPE)                                                 \
  static inline CNTYPE* CNTYPE##_shift_left(CNTYPE* i1, CNTYPE* i2) {                    \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val << i2->val;                                                       \
    return res;                                                                          \
  }

#define CN_GEN_SHIFT_RIGHT(CTYPE, CNTYPE)                                                \
  static inline CNTYPE* CNTYPE##_shift_right(CNTYPE* i1, CNTYPE* i2) {                   \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val >> i2->val;                                                       \
    return res;                                                                          \
  }

#define CN_GEN_MIN(CNTYPE)                                                               \
  static inline CNTYPE* CNTYPE##_min(CNTYPE* i1, CNTYPE* i2) {                           \
    return convert_from_cn_bool(CNTYPE##_lt(i1, i2)) ? i1 : i2;                          \
  }

#define CN_GEN_MAX(CNTYPE)                                                               \
  static inline CNTYPE* CNTYPE##_max(CNTYPE* i1, CNTYPE* i2) {                           \
    return convert_from_cn_bool(CNTYPE##_gt(i1, i2)) ? i1 : i2;                          \
  }

/* TODO: Account for UB: https://stackoverflow.com/a/20638659 */
#define CN_GEN_MOD(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE* CNTYPE##_mod(CNTYPE* i1, CNTYPE* i2) {                           \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val % i2->val;                                                        \
    if (res->val < 0) {                                                                  \
      res->val = (i2->val < 0) ? res->val - i2->val : res->val + i2->val;                \
    }                                                                                    \
    return res;                                                                          \
  }

#define CN_GEN_REM(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE* CNTYPE##_rem(CNTYPE* i1, CNTYPE* i2) {                           \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val % i2->val;                                                        \
    return res;                                                                          \
  }

#define CN_GEN_XOR(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE* CNTYPE##_xor(CNTYPE* i1, CNTYPE* i2) {                           \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val ^ i2->val;                                                        \
    return res;                                                                          \
  }

#define CN_GEN_BWAND(CTYPE, CNTYPE)                                                      \
  static inline CNTYPE* CNTYPE##_bwand(CNTYPE* i1, CNTYPE* i2) {                         \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val & i2->val;                                                        \
    return res;                                                                          \
  }

#define CN_GEN_BWOR(CTYPE, CNTYPE)                                                       \
  static inline CNTYPE* CNTYPE##_bwor(CNTYPE* i1, CNTYPE* i2) {                          \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = i1->val | i2->val;                                                        \
    return res;                                                                          \
  }

cn_bits_u32* cn_bits_u32_fls(cn_bits_u32* i1);
cn_bits_u64* cn_bits_u64_flsl(cn_bits_u64* i1);

static inline int ipow(int base, int exp) {
  int result = 1;
  for (;;) {
    if (exp & 1)
      result *= base;
    exp >>= 1;
    if (!exp)
      break;
    base *= base;
  }

  return result;
}

#define CN_GEN_POW(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE* CNTYPE##_pow(CNTYPE* i1, CNTYPE* i2) {                           \
    CNTYPE* res = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    res->val = ipow(i1->val, i2->val);                                                   \
    return res;                                                                          \
  }

#define cn_array_shift(cn_ptr, size, index)                                              \
  convert_to_cn_pointer(                                                                 \
      (char*)((uintptr_t)cn_ptr->ptr + (uintptr_t)index->val * (uintptr_t)size))

#define cn_member_shift(cn_ptr, tag, member_name)                                        \
  convert_to_cn_pointer(                                                                 \
      (void*)(((uintptr_t)cn_ptr->ptr) + offsetof(struct tag, member_name)))

#define CN_GEN_INCREMENT(CNTYPE)                                                         \
  static inline CNTYPE* CNTYPE##_increment(CNTYPE* i) {                                  \
    i->val = i->val + 1;                                                                 \
    return i;                                                                            \
  }

#define CN_GEN_PTR_ADD(CNTYPE)                                                           \
  static inline cn_pointer* cn_pointer_add_##CNTYPE(cn_pointer* ptr, CNTYPE* i) {        \
    cn_pointer* res = (cn_pointer*)cn_bump_malloc(sizeof(cn_pointer));                   \
    res->ptr = (char*)ptr->ptr + i->val;                                                 \
    return res;                                                                          \
  }

/* Casting functions */

#define CN_GEN_CAST_TO_PTR(CNTYPE, INTPTR_TYPE)                                          \
  static inline cn_pointer* cast_##CNTYPE##_to_cn_pointer(CNTYPE* i) {                   \
    cn_pointer* res = (cn_pointer*)cn_bump_malloc(sizeof(cn_pointer));                   \
    res->ptr = (void*)(INTPTR_TYPE)i->val;                                               \
    return res;                                                                          \
  }

#define CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, INTPTR_TYPE)                                 \
  static inline CNTYPE* cast_cn_pointer_to_##CNTYPE(cn_pointer* ptr) {                   \
    CNTYPE* res = (CNTYPE*)cn_bump_malloc(sizeof(CNTYPE));                               \
    res->val = (CTYPE)(INTPTR_TYPE)(ptr->ptr);                                           \
    return res;                                                                          \
  }

#define CN_GEN_CAST_INT_TYPES(CNTYPE1, CTYPE2, CNTYPE2)                                  \
  static inline CNTYPE2* cast_##CNTYPE1##_to_##CNTYPE2(CNTYPE1* i) {                     \
    CNTYPE2* res = (CNTYPE2*)cn_bump_aligned_alloc(alignof(CNTYPE2), sizeof(CNTYPE2));   \
    res->val = (CTYPE2)i->val;                                                           \
    return res;                                                                          \
  }

#define CN_GEN_DEFAULT(CNTYPE)                                                           \
  static inline CNTYPE* default_##CNTYPE(void) {                                         \
    return convert_to_##CNTYPE(0);                                                       \
  }

cn_map* default_cn_map(void);
cn_bool* default_cn_bool(void);

#define CN_GEN_MAP_GET(CNTYPE)                                                           \
  static inline void* cn_map_get_##CNTYPE(cn_map* m, cn_integer* key) {                  \
    int64_t* key_ptr = (int64_t*)fulm_malloc(sizeof(int64_t), &fulm_default_alloc);      \
    *key_ptr = key->val;                                                                 \
    void* res = ht_get(m, key_ptr);                                                      \
    fulm_free(key_ptr, &fulm_default_alloc);                                             \
    if (!res) {                                                                          \
      return (void*)default_##CNTYPE();                                                  \
    }                                                                                    \
    return res;                                                                          \
  }

#define CN_GEN_CASTS_INNER(CTYPE, CNTYPE)                                                \
  CN_GEN_CAST_INT_TYPES(cn_bits_i8, CTYPE, CNTYPE)                                       \
  CN_GEN_CAST_INT_TYPES(cn_bits_i16, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_i32, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_i64, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_u8, CTYPE, CNTYPE)                                       \
  CN_GEN_CAST_INT_TYPES(cn_bits_u16, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_u32, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_u64, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_integer, CTYPE, CNTYPE)

#define CN_GEN_PTR_CASTS_UNSIGNED(CTYPE, CNTYPE)                                         \
  CN_GEN_CAST_TO_PTR(CNTYPE, uintptr_t)                                                  \
  CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, uintptr_t)

#define CN_GEN_PTR_CASTS_SIGNED(CTYPE, CNTYPE)                                           \
  CN_GEN_CAST_TO_PTR(CNTYPE, intptr_t)                                                   \
  CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, intptr_t)

#define CN_GEN_ALL(UCTYPE, CTYPE, CNTYPE) CN_GEN_ALL_(UCTYPE, CTYPE, CNTYPE)

#define CN_GEN_ALL_(UCTYPE, CTYPE, CNTYPE)                                               \
  CN_GEN_CONVERT(CTYPE, CNTYPE)                                                          \
  CN_GEN_CONVERT_FROM(CTYPE, CNTYPE)                                                     \
  CN_GEN_EQUALITY(CNTYPE)                                                                \
  CN_GEN_LT(CNTYPE)                                                                      \
  CN_GEN_LE(CNTYPE)                                                                      \
  CN_GEN_GT(CNTYPE)                                                                      \
  CN_GEN_GE(CNTYPE)                                                                      \
  CN_GEN_NEGATE(CNTYPE)                                                                  \
  CN_GEN_BW_COMPL(CNTYPE)                                                                \
  CN_GEN_ADD(UCTYPE, CTYPE, CNTYPE)                                                      \
  CN_GEN_SUB(UCTYPE, CTYPE, CNTYPE)                                                      \
  CN_GEN_MUL(CTYPE, CNTYPE)                                                              \
  CN_GEN_DIV(CTYPE, CNTYPE)                                                              \
  CN_GEN_SHIFT_LEFT(CTYPE, CNTYPE)                                                       \
  CN_GEN_SHIFT_RIGHT(CTYPE, CNTYPE)                                                      \
  CN_GEN_MIN(CNTYPE)                                                                     \
  CN_GEN_MAX(CNTYPE)                                                                     \
  CN_GEN_MOD(CTYPE, CNTYPE)                                                              \
  CN_GEN_REM(CTYPE, CNTYPE)                                                              \
  CN_GEN_XOR(CTYPE, CNTYPE)                                                              \
  CN_GEN_BWAND(CTYPE, CNTYPE)                                                            \
  CN_GEN_BWOR(CTYPE, CNTYPE)                                                             \
  CN_GEN_POW(CTYPE, CNTYPE)                                                              \
  CN_GEN_INCREMENT(CNTYPE)                                                               \
  CN_GEN_PTR_ADD(CNTYPE)                                                                 \
  CN_GEN_CASTS_INNER(CTYPE, CNTYPE)                                                      \
  CN_GEN_DEFAULT(CNTYPE)                                                                 \
  CN_GEN_MAP_GET(CNTYPE)

CN_GEN_ALL(uint8_t, int8_t, cn_bits_i8)
CN_GEN_ALL(uint16_t, int16_t, cn_bits_i16)
CN_GEN_ALL(uint32_t, int32_t, cn_bits_i32)
CN_GEN_ALL(uint64_t, int64_t, cn_bits_i64)
CN_GEN_ALL(uint8_t, uint8_t, cn_bits_u8)
CN_GEN_ALL(uint16_t, uint16_t, cn_bits_u16)
CN_GEN_ALL(uint32_t, uint32_t, cn_bits_u32)
CN_GEN_ALL(uint64_t, uint64_t, cn_bits_u64)
CN_GEN_ALL(unsigned long, signed long, cn_integer)

CN_GEN_PTR_CASTS_SIGNED(int8_t, cn_bits_i8)
CN_GEN_PTR_CASTS_SIGNED(int16_t, cn_bits_i16)
CN_GEN_PTR_CASTS_SIGNED(int32_t, cn_bits_i32)
CN_GEN_PTR_CASTS_SIGNED(int64_t, cn_bits_i64)
CN_GEN_PTR_CASTS_UNSIGNED(uint8_t, cn_bits_u8)
CN_GEN_PTR_CASTS_UNSIGNED(uint16_t, cn_bits_u16)
CN_GEN_PTR_CASTS_UNSIGNED(uint32_t, cn_bits_u32)
CN_GEN_PTR_CASTS_UNSIGNED(uint64_t, cn_bits_u64)
CN_GEN_PTR_CASTS_SIGNED(signed long, cn_integer)

cn_pointer* convert_to_cn_pointer(const void* ptr);
void* convert_from_cn_pointer(cn_pointer* cn_ptr);
cn_pointer* cn_pointer_min(cn_pointer* p, cn_pointer* q);
cn_pointer* cn_pointer_max(cn_pointer* p, cn_pointer* q);
cn_pointer* cn_pointer_mod(cn_pointer* ptr, cn_pointer* n);
cn_pointer* cast_cn_pointer_to_cn_pointer(cn_pointer* p);

CN_GEN_CONVERT(uint8_t, cn_alloc_id)
CN_GEN_EQUALITY(cn_alloc_id)
CN_GEN_DEFAULT(cn_pointer)
CN_GEN_MAP_GET(cn_pointer)
CN_GEN_MAP_GET(cn_map)

#ifdef __cplusplus
}
#endif

#endif  // CN_EVAL_H
