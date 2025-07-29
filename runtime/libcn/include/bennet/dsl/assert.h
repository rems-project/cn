#ifndef BENNET_ASSERT_H
#define BENNET_ASSERT_H

#include <stdint.h>

#include <bennet/info/backtracks.h>
#include <bennet/state/failure.h>
#include <bennet/utils/optional.h>
#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BENNET_ASSERT(cond, last_var, ...)                                               \
  if (!convert_from_cn_bool(cond)) {                                                     \
    bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                        \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                               \
                                                                                         \
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                              \
    const void* vars[] = {__VA_ARGS__};                                                  \
    bennet_failure_blame_many(vars);                                                     \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
                                                                                         \
  bennet_info_unsatisfied_log(__FILE__, __LINE__, false);

////////////
// Domain //
////////////

#define bennet_assert_domain(cn_ty) bennet_assert_domain_##cn_ty

#define BENNET_ASSERT_DOMAIN_IMPL(cn_ty, c_ty, min, max)                                 \
  static inline bool bennet_assert_domain(cn_ty)(cn_ty * sym,                            \
      cn_ty * lower_bound_inc,                                                           \
      cn_ty * lower_bound_ex,                                                            \
      cn_ty * upper_bound_inc,                                                           \
      cn_ty * upper_bound_ex,                                                            \
      cn_ty * multiple,                                                                  \
      const void* vars[]) {                                                              \
    bool failed = false;                                                                 \
    bennet_domain_failure_info info = bennet_domain_failure_default();                   \
                                                                                         \
    if (lower_bound_ex != NULL) {                                                        \
      c_ty lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound_ex);                 \
      if (lower_bound_raw == max) {                                                      \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
        bennet_failure_remove_blame(sym);                                                \
                                                                                         \
        return true;                                                                     \
      } else {                                                                           \
        cn_ty* lower_bound_ex_now_inc =                                                  \
            cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(lower_bound_raw + 1));    \
        if (lower_bound_inc == NULL) {                                                   \
          lower_bound_inc = lower_bound_ex_now_inc;                                      \
        } else {                                                                         \
          lower_bound_inc = cn_ty##_max(lower_bound_inc, lower_bound_ex_now_inc);        \
        }                                                                                \
      }                                                                                  \
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
    if (upper_bound_ex != NULL) {                                                        \
      c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound_ex);                 \
      if (upper_bound_raw == min) {                                                      \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
        bennet_failure_remove_blame(sym);                                                \
                                                                                         \
        return true;                                                                     \
      } else {                                                                           \
        cn_ty* upper_bound_ex_now_inc =                                                  \
            cast_cn_bits_u64_to_##cn_ty(convert_to_cn_bits_u64(upper_bound_raw - 1));    \
                                                                                         \
        if (upper_bound_inc == NULL) {                                                   \
          upper_bound_inc = upper_bound_ex_now_inc;                                      \
        } else {                                                                         \
          upper_bound_inc = cn_ty##_min(upper_bound_inc, upper_bound_ex_now_inc);        \
        }                                                                                \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    if (upper_bound_inc != NULL &&                                                       \
        !convert_from_cn_bool(cn_ty##_le(sym, upper_bound_inc))) {                       \
      c_ty upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound_inc);                \
      info.upper_bound_inc = bennet_optional_some(intmax_t, upper_bound_raw);            \
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

#define BENNET_ASSERT_DOMAIN(cn_ty, sym, lbi, lbe, ubi, ube, m, last_var, ...)           \
  {                                                                                      \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assert_domain(cn_ty)(sym, lbi, lbe, ubi, ube, m, vars)) {                 \
      bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                      \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
                                                                                         \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                              \
  }

#define bennet_assert_domain_cast(cn_ty, cast_cn_ty)                                     \
  bennet_assert_domain_cast_##cn_ty##_from_##cast_cn_ty

#define BENNET_ASSERT_CAST_IMPL_AUX(cast_cn_ty, cast_c_ty, cn_ty, c_ty, min, max)        \
  static inline bool bennet_assert_domain_cast(cn_ty, cast_cn_ty)(cn_ty * sym,           \
      cast_cn_ty * lower_bound_inc,                                                      \
      cast_cn_ty * lower_bound_ex,                                                       \
      cast_cn_ty * upper_bound_inc,                                                      \
      cast_cn_ty * upper_bound_ex,                                                       \
      cast_cn_ty * multiple,                                                             \
      const void* vars[]) {                                                              \
    if (lower_bound_inc) {                                                               \
      cast_c_ty lower_bound_inc_raw =                                                    \
          (cast_c_ty)convert_from_##cast_cn_ty(lower_bound_inc);                         \
      if (lower_bound_inc_raw <= min) {                                                  \
        lower_bound_inc = NULL;                                                          \
      } else if (max < lower_bound_inc_raw) {                                            \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
        bennet_failure_remove_blame(sym);                                                \
                                                                                         \
        return true;                                                                     \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    if (lower_bound_ex) {                                                                \
      cast_c_ty lower_bound_ex_raw =                                                     \
          (cast_c_ty)convert_from_##cast_cn_ty(lower_bound_ex);                          \
      if (lower_bound_ex_raw < min) {                                                    \
        lower_bound_ex = NULL;                                                           \
      } else if (max <= lower_bound_ex_raw) {                                            \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
        bennet_failure_remove_blame(sym);                                                \
                                                                                         \
        return true;                                                                     \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    if (upper_bound_inc) {                                                               \
      cast_c_ty upper_bound_inc_raw =                                                    \
          (cast_c_ty)convert_from_##cast_cn_ty(upper_bound_inc);                         \
      if (max <= upper_bound_inc_raw) {                                                  \
        upper_bound_inc = NULL;                                                          \
      } else if (upper_bound_inc_raw < min) {                                            \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
        bennet_failure_remove_blame(sym);                                                \
                                                                                         \
        return true;                                                                     \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    if (upper_bound_ex) {                                                                \
      cast_c_ty upper_bound_ex_raw =                                                     \
          (cast_c_ty)convert_from_##cast_cn_ty(upper_bound_ex);                          \
      if (max < upper_bound_ex_raw) {                                                    \
        upper_bound_ex = NULL;                                                           \
      } else if (upper_bound_ex_raw <= min) {                                            \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
        bennet_failure_remove_blame(sym);                                                \
                                                                                         \
        return true;                                                                     \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    cn_ty* lower_bound_inc_casted =                                                      \
        (lower_bound_inc == NULL) ? NULL                                                 \
                                  : cast_##cast_cn_ty##_to_##cn_ty(lower_bound_inc);     \
    cn_ty* lower_bound_ex_casted = (lower_bound_ex == NULL)                              \
                                       ? NULL                                            \
                                       : cast_##cast_cn_ty##_to_##cn_ty(lower_bound_ex); \
    cn_ty* upper_bound_inc_casted =                                                      \
        (upper_bound_inc == NULL) ? NULL                                                 \
                                  : cast_##cast_cn_ty##_to_##cn_ty(upper_bound_inc);     \
    cn_ty* upper_bound_ex_casted = (upper_bound_ex == NULL)                              \
                                       ? NULL                                            \
                                       : cast_##cast_cn_ty##_to_##cn_ty(upper_bound_ex); \
    cn_ty* multiple_casted =                                                             \
        (multiple == NULL) ? NULL : cast_##cast_cn_ty##_to_##cn_ty(multiple);            \
                                                                                         \
    return bennet_assert_domain(cn_ty)(sym,                                              \
        lower_bound_inc_casted,                                                          \
        lower_bound_ex_casted,                                                           \
        upper_bound_inc_casted,                                                          \
        upper_bound_ex_casted,                                                           \
        multiple_casted,                                                                 \
        vars);                                                                           \
  }

#define BENNET_ASSERT_CAST_IMPL(                                                         \
    cast_cn_ty, cast_c_ty, cast_min, cast_max, cn_ty, c_ty, min, max)                    \
  BENNET_ASSERT_CAST_IMPL_AUX(cast_cn_ty, cast_c_ty, cn_ty, c_ty, min, max)              \
  BENNET_ASSERT_CAST_IMPL_AUX(cn_ty, c_ty, cast_cn_ty, cast_c_ty, cast_min, cast_max)

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunknown-pragmas"
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wtautological-constant-out-of-range-compare"

BENNET_ASSERT_CAST_IMPL(
    cn_bits_u64, uint64_t, 0, UINT64_MAX, cn_pointer, uintptr_t, 0, UINTPTR_MAX)

BENNET_ASSERT_CAST_IMPL(cn_bits_i64,
    int64_t,
    INT64_MIN,
    INT64_MAX,
    cn_bits_i32,
    int32_t,
    INT32_MIN,
    INT32_MAX)
BENNET_ASSERT_CAST_IMPL(
    cn_bits_u64, uint64_t, 0, UINT64_MAX, cn_bits_u32, uint32_t, 0, UINT32_MAX)

#pragma clang diagnostic pop
#pragma GCC diagnostic pop

#define BENNET_ASSERT_DOMAIN_CAST(                                                       \
    cast_cn_ty, cn_ty, sym, lbi, lbe, ubi, ube, m, last_var, ...)                        \
  {                                                                                      \
    const void* vars[] = {__VA_ARGS__};                                                  \
    if (bennet_assert_domain_cast(cn_ty, cast_cn_ty)(                                    \
            sym, lbi, lbe, ubi, ube, m, vars)) {                                         \
      bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                      \
      bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                             \
      goto bennet_label_##last_var##_backtrack;                                          \
    }                                                                                    \
                                                                                         \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, false);                              \
  }

#ifdef __cplusplus
}
#endif

#endif  // BENNET_ASSERT_H
