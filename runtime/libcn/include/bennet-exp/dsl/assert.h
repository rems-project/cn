#ifndef BENNET_EXP_ASSERT_H
#define BENNET_EXP_ASSERT_H

#include <stdint.h>

#include <bennet-exp/state/failure.h>
#include <bennet-exp/utils/optional.h>
#include <cn-executable/utils.h>

#define bennet_assert_le(cn_ty) bennet_assert_le_##cn_ty
#define bennet_assert_lt(cn_ty) bennet_assert_lt_##cn_ty
#define bennet_assert_ge(cn_ty) bennet_assert_ge_##cn_ty
#define bennet_assert_gt(cn_ty) bennet_assert_gt_##cn_ty

#define BENNET_ASSERT_IMPL(cn_ty, c_ty, min, max)                                        \
  static inline bool bennet_assert_le(cn_ty)(                                            \
      cn_ty * x, cn_ty * upper_bound, const void** vars) {                               \
    if (!convert_from_cn_bool(cn_ty##_le(x, upper_bound))) {                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_default();                 \
      c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound);                    \
      info.upper_bound_inc = bennet_optional_some(intmax_t, upper_bound_raw);            \
      bennet_failure_blame_domain(x, &info);                                             \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    return false;                                                                        \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_assert_lt(cn_ty)(                                            \
      cn_ty * x, cn_ty * upper_bound, const void** vars) {                               \
    c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound);                      \
    if (upper_bound_raw == min) {                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_default();                 \
      info.lower_bound_inc = bennet_optional_some(intmax_t, 1);                          \
      info.upper_bound_inc = bennet_optional_some(intmax_t, 0);                          \
      bennet_failure_blame_domain(x, &info);                                             \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
    return bennet_assert_le_##cn_ty(x,                                                   \
        cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(upper_bound_raw - 1)),        \
        vars);                                                                           \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_assert_ge(cn_ty)(                                            \
      cn_ty * x, cn_ty * lower_bound, const void** vars) {                               \
    if (!convert_from_cn_bool(cn_ty##_ge(x, lower_bound))) {                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_default();                 \
      c_ty lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound);                    \
      info.lower_bound_inc = bennet_optional_some(intmax_t, lower_bound_raw);            \
      bennet_failure_blame_domain(x, &info);                                             \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    return false;                                                                        \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_assert_gt(cn_ty)(                                            \
      cn_ty * x, cn_ty * lower_bound, const void** vars) {                               \
    c_ty lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound);                      \
    if (lower_bound_raw == max) {                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_default();                 \
      info.lower_bound_inc = bennet_optional_some(intmax_t, 1);                          \
      info.upper_bound_inc = bennet_optional_some(intmax_t, 0);                          \
      bennet_failure_blame_domain(x, &info);                                             \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
    return bennet_assert_ge_##cn_ty(x,                                                   \
        cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(lower_bound_raw + 1)),        \
        vars);                                                                           \
  }

BENNET_ASSERT_IMPL(cn_pointer, uintptr_t, 0, UINTPTR_MAX)

BENNET_ASSERT_IMPL(cn_bits_u8, uint8_t, 0, UINT8_MAX)
BENNET_ASSERT_IMPL(cn_bits_i8, int8_t, INT8_MIN, INT8_MAX)

BENNET_ASSERT_IMPL(cn_bits_u16, uint16_t, 0, UINT16_MAX)
BENNET_ASSERT_IMPL(cn_bits_i16, int16_t, INT16_MIN, INT16_MAX)

BENNET_ASSERT_IMPL(cn_bits_u32, uint32_t, 0, UINT32_MAX)
BENNET_ASSERT_IMPL(cn_bits_i32, int32_t, INT32_MIN, INT32_MAX)

BENNET_ASSERT_IMPL(cn_bits_u64, uint64_t, 0, UINT64_MAX)
BENNET_ASSERT_IMPL(cn_bits_i64, int64_t, INT64_MIN, INT64_MAX)

#endif  // BENNET_EXP_ASSERT_H
