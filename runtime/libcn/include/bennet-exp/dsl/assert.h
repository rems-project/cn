#ifndef BENNET_EXP_ASSERT_H
#define BENNET_EXP_ASSERT_H

#include <stdint.h>

#include <bennet-exp/state/failure.h>
#include <bennet-exp/utils/optional.h>
#include <cn-executable/utils.h>

#define BENNET_ASSERT(cond, last_var, ...)                                               \
  if (!convert_from_cn_bool(cond)) {                                                     \
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                              \
    const void* vars[] = {__VA_ARGS__};                                                  \
    bennet_failure_blame_many(vars);                                                     \
    goto bennet_label_##last_var##_backtrack;                                            \
  }

////////////
// Domain //
////////////

#define bennet_assert_domain_ii(cn_ty) bennet_assert_domain_ii_##cn_ty
#define bennet_assert_domain_ie(cn_ty) bennet_assert_domain_ie_##cn_ty
#define bennet_assert_domain_ei(cn_ty) bennet_assert_domain_ei_##cn_ty
#define bennet_assert_domain_ee(cn_ty) bennet_assert_domain_ee_##cn_ty

#define BENNET_ASSERT_DOMAIN_IMPL(cn_ty, c_ty, min, max)                                 \
  static inline bool bennet_assert_domain_ii(cn_ty)(cn_ty * sym,                         \
      cn_ty * lower_bound_inc,                                                           \
      cn_ty * upper_bound_inc,                                                           \
      cn_ty * multiple,                                                                  \
      const void* vars[]) {                                                              \
    bool failed = false;                                                                 \
    bennet_domain_failure_info info = bennet_domain_failure_default();                   \
                                                                                         \
    if (upper_bound_inc != NULL &&                                                       \
        !convert_from_cn_bool(cn_ty##_le(sym, upper_bound_inc))) {                       \
      c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound_inc);                \
      info.upper_bound_inc = bennet_optional_some(intmax_t, upper_bound_raw);            \
                                                                                         \
      failed = true;                                                                     \
    }                                                                                    \
                                                                                         \
    if (lower_bound_inc != NULL &&                                                       \
        !convert_from_cn_bool(cn_ty##_ge(sym, lower_bound_inc))) {                       \
      c_ty lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound_inc);                \
      info.lower_bound_inc = bennet_optional_some(intmax_t, lower_bound_raw);            \
                                                                                         \
      failed = true;                                                                     \
    }                                                                                    \
                                                                                         \
    if (multiple != NULL && !convert_from_cn_bool(cn_ty##_equality(                      \
                                cn_ty##_mod(sym, multiple), convert_to_##cn_ty(0)))) {   \
      c_ty multiple_raw = (c_ty)convert_from_##cn_ty(multiple);                          \
      info.multiple = bennet_optional_some(intmax_t, multiple_raw);                      \
                                                                                         \
      failed = true;                                                                     \
    }                                                                                    \
                                                                                         \
    if (failed) {                                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
      bennet_failure_blame_domain(sym, &info);                                           \
    }                                                                                    \
                                                                                         \
    return failed;                                                                       \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_assert_domain_ie(cn_ty)(cn_ty * sym,                         \
      cn_ty * lower_bound_inc,                                                           \
      cn_ty * upper_bound_ex,                                                            \
      cn_ty * multiple,                                                                  \
      const void* vars[]) {                                                              \
    c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound_ex);                   \
    if (upper_bound_raw == min) {                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_empty();                   \
      bennet_failure_blame_domain(sym, &info);                                           \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    cn_ty* upper_bound_inc =                                                             \
        cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(upper_bound_raw - 1));        \
                                                                                         \
    return bennet_assert_domain_ii(cn_ty)(                                               \
        sym, lower_bound_inc, upper_bound_inc, multiple, vars);                          \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_assert_domain_ei(cn_ty)(cn_ty * sym,                         \
      cn_ty * lower_bound_ex,                                                            \
      cn_ty * upper_bound_inc,                                                           \
      cn_ty * multiple,                                                                  \
      const void* vars[]) {                                                              \
    c_ty lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound_ex);                   \
    if (lower_bound_raw == max) {                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_empty();                   \
      bennet_failure_blame_domain(sym, &info);                                           \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    cn_ty* lower_bound_inc =                                                             \
        cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(lower_bound_raw + 1));        \
                                                                                         \
    return bennet_assert_domain_ii(cn_ty)(                                               \
        sym, lower_bound_inc, upper_bound_inc, multiple, vars);                          \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_assert_domain_ee(cn_ty)(cn_ty * sym,                         \
      cn_ty * lower_bound_ex,                                                            \
      cn_ty * upper_bound_ex,                                                            \
      cn_ty * multiple,                                                                  \
      const void* vars[]) {                                                              \
    c_ty lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound_ex);                   \
    if (lower_bound_raw == max) {                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_empty();                   \
      bennet_failure_blame_domain(sym, &info);                                           \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound_ex);                   \
    if (upper_bound_raw == min) {                                                        \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      bennet_domain_failure_info info = bennet_domain_failure_empty();                   \
      bennet_failure_blame_domain(sym, &info);                                           \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    cn_ty* lower_bound_inc =                                                             \
        cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(lower_bound_raw + 1));        \
                                                                                         \
    cn_ty* upper_bound_inc =                                                             \
        cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(upper_bound_raw - 1));        \
                                                                                         \
    return bennet_assert_domain_ii(cn_ty)(                                               \
        sym, lower_bound_inc, upper_bound_inc, multiple, vars);                          \
  }

BENNET_ASSERT_DOMAIN_IMPL(cn_pointer, uintptr_t, 0, UINTPTR_MAX)

BENNET_ASSERT_DOMAIN_IMPL(cn_bits_u8, uint8_t, 0, UINT8_MAX)
BENNET_ASSERT_DOMAIN_IMPL(cn_bits_i8, int8_t, INT8_MIN, INT8_MAX)

BENNET_ASSERT_DOMAIN_IMPL(cn_bits_u16, uint16_t, 0, UINT16_MAX)
BENNET_ASSERT_DOMAIN_IMPL(cn_bits_i16, int16_t, INT16_MIN, INT16_MAX)

BENNET_ASSERT_DOMAIN_IMPL(cn_bits_u32, uint32_t, 0, UINT32_MAX)
BENNET_ASSERT_DOMAIN_IMPL(cn_bits_i32, int32_t, INT32_MIN, INT32_MAX)

BENNET_ASSERT_DOMAIN_IMPL(cn_bits_u64, uint64_t, 0, UINT64_MAX)
BENNET_ASSERT_DOMAIN_IMPL(cn_bits_i64, int64_t, INT64_MIN, INT64_MAX)

#define BENNET_ASSERT_DOMAIN_II(cn_ty, sym, lb, ub, m, last_var, ...)                    \
  {                                                                                      \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assert_domain_ii(cn_ty)(sym, lb, ub, m, vars)) {                          \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_ASSERT_DOMAIN_IE(cn_ty, sym, lb, ub, m, last_var, ...)                    \
  {                                                                                      \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assert_domain_ie(cn_ty)(sym, lb, ub, m, vars)) {                          \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_ASSERT_DOMAIN_EI(cn_ty, sym, lb, ub, m, last_var, ...)                    \
  {                                                                                      \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assert_domain_ei(cn_ty)(sym, lb, ub, m, vars)) {                          \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#define BENNET_ASSERT_DOMAIN_EE(cn_ty, sym, lb, ub, m, last_var, ...)                    \
  {                                                                                      \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assert_domain_ee(cn_ty)(sym, lb, ub, m, vars)) {                          \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
  }

#endif  // BENNET_EXP_ASSERT_H
