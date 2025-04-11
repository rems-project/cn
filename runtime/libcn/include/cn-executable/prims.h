/*
This file defines all primitive operations on CN types.

Since there are quite a lot of these, and they are pretty formulaic, we
define them using macros.   To havind to duplicate all declarations and
definitions, we use the `PRIM_DEF` macro.  The idea is that this file
is included *both* in the header, and the corresponding `.c` implementation:
  * before it is included in the header, we define `PRIM_DEF` to just ignore
    its argument and emit a `;`,
  * before it is included in the `.c` file, we define `PRIM_DEF` to emit its
    argument, so we get the actual definition.
*/


/* Conversions */

#define CN_GEN_CONVERT(CTYPE, CNTYPE)                                                    \
CNTYPE *convert_to_##CNTYPE(CTYPE i) PRIM_DEF({                                          \
  CNTYPE *ret = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  ret->val = i;                                                                          \
  return ret;                                                                            \
})

#define CN_GEN_CONVERT_FROM(CTYPE, CNTYPE)                                               \
CTYPE convert_from_##CNTYPE(CNTYPE *i) PRIM_DEF({                                        \
  return i->val;                                                                         \
})



/* Comparisons */

#define CN_GEN_EQUALITY(CNTYPE)                                                          \
cn_bool *CNTYPE##_equality(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                            \
  return convert_to_cn_bool(i1->val == i2->val);                                         \
})


#define CN_GEN_LT(CNTYPE)                                                                \
cn_bool *CNTYPE##_lt(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  return convert_to_cn_bool(i1->val < i2->val);                                          \
})

#define CN_GEN_LE(CNTYPE)                                                                \
cn_bool *CNTYPE##_le(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  return convert_to_cn_bool(i1->val <= i2->val);                                         \
})

#define CN_GEN_GT(CNTYPE)                                                                \
cn_bool *CNTYPE##_gt(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  return convert_to_cn_bool(i1->val > i2->val);                                          \
})

#define CN_GEN_GE(CNTYPE)                                                                \
cn_bool *CNTYPE##_ge(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  return convert_to_cn_bool(i1->val >= i2->val);                                         \
})

#define CN_GEN_MIN(CNTYPE)                                                               \
CNTYPE *CNTYPE##_min (CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                 \
  return convert_from_cn_bool(CNTYPE##_lt(i1, i2)) ? i1 : i2;                            \
})

#define CN_GEN_MAX(CNTYPE)                                                               \
CNTYPE  *CNTYPE##_max(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                 \
  return convert_from_cn_bool(CNTYPE##_gt(i1, i2)) ? i1 : i2;                            \
})


/* Arithmetic */

#define CN_GEN_NEGATE(CNTYPE)                                                            \
CNTYPE *CNTYPE##_negate(CNTYPE *i) PRIM_DEF({                                            \
  return convert_to_##CNTYPE(-(i->val));                                                 \
})

#define CN_GEN_ADD(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_add(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val + i2->val;                                                          \
  return res;                                                                            \
})

#define CN_GEN_SUB(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_sub(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val - i2->val;                                                          \
  return res;                                                                            \
})

#define CN_GEN_MUL(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_multiply (CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                            \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val * i2->val;                                                          \
  return res;                                                                            \
})

#define CN_GEN_DIV(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_divide(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                               \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val / i2->val;                                                          \
  return res;                                                                            \
})

/* TODO: Account for UB: https://stackoverflow.com/a/20638659 */
#define CN_GEN_MOD(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_mod(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val % i2->val;                                                          \
  if (res->val < 0) {                                                                    \
    res->val = (i2->val < 0) ? res->val - i2->val : res->val + i2->val;                  \
  }                                                                                      \
  return res;                                                                            \
})

#define CN_GEN_REM(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_rem(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val % i2->val;                                                          \
  return res;                                                                            \
})

#define CN_GEN_POW(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_pow(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = ipow(i1->val, i2->val);                                                     \
  return res;                                                                            \
})
  
#define CN_GEN_INCREMENT(CNTYPE)                                                         \
CNTYPE *CNTYPE##_increment(CNTYPE *i) PRIM_DEF({                                         \
  i->val = i->val + 1;                                                                   \
  return i;                                                                              \
})




/* Bit operations */

#define CN_GEN_SHIFT_LEFT(CTYPE, CNTYPE)                                                 \
CNTYPE *CNTYPE##_shift_left (CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                          \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val << i2->val;                                                         \
  return res;                                                                            \
})

#define CN_GEN_SHIFT_RIGHT(CTYPE, CNTYPE)                                                \
CNTYPE *CNTYPE##_shift_right(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                          \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val >> i2->val;                                                         \
  return res;                                                                            \
})

#define CN_GEN_XOR(CTYPE, CNTYPE)                                                        \
CNTYPE *CNTYPE##_xor(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                  \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val ^ i2->val;                                                          \
  return res;                                                                            \
})

#define CN_GEN_BWAND(CTYPE, CNTYPE)                                                      \
CNTYPE *CNTYPE##_bwand(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val & i2->val;                                                          \
  return res;                                                                            \
})

#define CN_GEN_BWOR(CTYPE, CNTYPE)                                                       \
CNTYPE *CNTYPE##_bwor(CNTYPE *i1, CNTYPE *i2) PRIM_DEF({                                 \
  CNTYPE *res = (CNTYPE *)cn_bump_aligned_alloc(_Alignof(CNTYPE), sizeof(CNTYPE));       \
  res->val = i1->val | i2->val;                                                          \
  return res;                                                                            \
})


/* Pointer manipulation */

#define CN_GEN_PTR_ADD(CNTYPE)                                                           \
cn_pointer *cn_pointer_add_##CNTYPE(cn_pointer *ptr, CNTYPE *i) PRIM_DEF({               \
  cn_pointer *res = (cn_pointer *)cn_bump_malloc(sizeof(cn_pointer));                    \
  res->ptr = (char *)ptr->ptr + i->val;                                                  \
  return res;                                                                            \
})
  
#define CN_GEN_CAST_TO_PTR(CNTYPE, INTPTR_TYPE)                                          \
cn_pointer *cast_##CNTYPE##_to_cn_pointer(CNTYPE *i) PRIM_DEF({                          \
  cn_pointer *res = (cn_pointer *)cn_bump_malloc(sizeof(cn_pointer));                    \
  res->ptr = (void *)(INTPTR_TYPE)i->val;                                                \
  return res;                                                                            \
})

#define CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, INTPTR_TYPE)                                 \
CNTYPE *cast_cn_pointer_to_##CNTYPE(cn_pointer *ptr) PRIM_DEF({                          \
  CNTYPE *res = (CNTYPE *)cn_bump_malloc(sizeof(CNTYPE));                                \
  res->val = (CTYPE)(INTPTR_TYPE)(ptr->ptr);                                             \
  return res;                                                                            \
})

#define CN_GEN_CAST_INT_TYPES(CNTYPE1, CTYPE2, CNTYPE2)                                  \
CNTYPE2 *cast_##CNTYPE1##_to_##CNTYPE2(CNTYPE1 *i) PRIM_DEF({                            \
  CNTYPE2 *res = (CNTYPE2 *)cn_bump_aligned_alloc(_Alignof(CNTYPE2), sizeof(CNTYPE2));   \
  res->val = (CTYPE2)i->val;                                                             \
  return res;                                                                            \
})

#define CN_GEN_DEFAULT(CNTYPE)                                                           \
CNTYPE *default_##CNTYPE(void) PRIM_DEF({                                                \
  return convert_to_##CNTYPE(0);                                                         \
})

#define CN_GEN_MAP_GET(CNTYPE)                                                           \
void *cn_map_get_##CNTYPE(cn_map *m, cn_integer *key) PRIM_DEF({                         \
  int64_t *key_ptr = (int64_t*)cn_bump_malloc(sizeof(int64_t));                          \
  *key_ptr = key->val;                                                                   \
  void *res = ht_get(m, key_ptr);                                                        \
  if (!res) {                                                                            \
    return (void *)default_##CNTYPE();                                                   \
  }                                                                                      \
  return res;                                                                            \
})


/* These are all the variations we need */

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

#define CN_GEN_ALL(CTYPE, CNTYPE)                                                        \
  CN_GEN_CONVERT(CTYPE, CNTYPE)                                                          \
  CN_GEN_CONVERT_FROM(CTYPE, CNTYPE)                                                     \
  CN_GEN_EQUALITY(CNTYPE)                                                                \
  CN_GEN_LT(CNTYPE)                                                                      \
  CN_GEN_LE(CNTYPE)                                                                      \
  CN_GEN_GT(CNTYPE)                                                                      \
  CN_GEN_GE(CNTYPE)                                                                      \
  CN_GEN_NEGATE(CNTYPE)                                                                  \
  CN_GEN_ADD(CTYPE, CNTYPE)                                                              \
  CN_GEN_SUB(CTYPE, CNTYPE)                                                              \
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


CN_GEN_ALL(int8_t, cn_bits_i8)
CN_GEN_ALL(int16_t, cn_bits_i16)
CN_GEN_ALL(int32_t, cn_bits_i32)
CN_GEN_ALL(int64_t, cn_bits_i64)
CN_GEN_ALL(uint8_t, cn_bits_u8)
CN_GEN_ALL(uint16_t, cn_bits_u16)
CN_GEN_ALL(uint32_t, cn_bits_u32)
CN_GEN_ALL(uint64_t, cn_bits_u64)
CN_GEN_ALL(signed long, cn_integer)
  
CN_GEN_PTR_CASTS_SIGNED(int8_t, cn_bits_i8)
CN_GEN_PTR_CASTS_SIGNED(int16_t, cn_bits_i16)
CN_GEN_PTR_CASTS_SIGNED(int32_t, cn_bits_i32)
CN_GEN_PTR_CASTS_SIGNED(int64_t, cn_bits_i64)
CN_GEN_PTR_CASTS_UNSIGNED(uint8_t, cn_bits_u8)
CN_GEN_PTR_CASTS_UNSIGNED(uint16_t, cn_bits_u16)
CN_GEN_PTR_CASTS_UNSIGNED(uint32_t, cn_bits_u32)
CN_GEN_PTR_CASTS_UNSIGNED(uint64_t, cn_bits_u64)
CN_GEN_PTR_CASTS_SIGNED(signed long, cn_integer)

CN_GEN_CONVERT(uint8_t, cn_alloc_id)
CN_GEN_EQUALITY(cn_alloc_id)
CN_GEN_DEFAULT(cn_pointer)
CN_GEN_MAP_GET(cn_pointer)
CN_GEN_MAP_GET(cn_map)
