#include <stddef.h>
#include <stdint.h>

#include <bennet/internals/domains/wint.h>
#include <bennet/state/failure.h>
#include <cn-executable/utils.h>

#define BENNET_SPECIALIZED_IMPL(cn_ty, c_ty, min, max)                                   \
  cn_ty* bennet_specialized_##cn_ty(cn_ty* lower_bound_ex,                               \
      cn_ty* lower_bound_inc,                                                            \
      cn_ty* upper_bound_inc,                                                            \
      cn_ty* upper_bound_ex,                                                             \
      const void* vars[]) {                                                              \
    c_ty lower_bound_raw = min;                                                          \
    if (lower_bound_ex != NULL) {                                                        \
      lower_bound_raw = (c_ty)convert_from_##cn_ty(lower_bound_ex);                      \
      if (lower_bound_raw == max) {                                                      \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
                                                                                         \
        return NULL;                                                                     \
      } else {                                                                           \
        lower_bound_raw += 1;                                                            \
                                                                                         \
        if (lower_bound_inc != NULL) {                                                   \
          c_ty lower_bound_raw_2 = convert_from_##cn_ty(lower_bound_inc);                \
          lower_bound_raw = (lower_bound_raw > lower_bound_raw_2) ? lower_bound_raw      \
                                                                  : lower_bound_raw_2;   \
        }                                                                                \
      }                                                                                  \
    } else if (lower_bound_inc != NULL) {                                                \
      lower_bound_raw = convert_from_##cn_ty(lower_bound_inc);                           \
    }                                                                                    \
                                                                                         \
    c_ty upper_bound_raw = max;                                                          \
    if (upper_bound_ex != NULL) {                                                        \
      upper_bound_raw = (c_ty)convert_from_##cn_ty(upper_bound_ex);                      \
      if (upper_bound_raw == min) {                                                      \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                          \
        bennet_failure_blame_many(vars);                                                 \
                                                                                         \
        return NULL;                                                                     \
      } else {                                                                           \
        upper_bound_raw -= 1;                                                            \
                                                                                         \
        if (upper_bound_inc != NULL) {                                                   \
          c_ty upper_bound_raw_2 = convert_from_##cn_ty(upper_bound_inc);                \
          upper_bound_raw = (upper_bound_raw < upper_bound_raw_2) ? upper_bound_raw      \
                                                                  : upper_bound_raw_2;   \
        }                                                                                \
      }                                                                                  \
    } else if (upper_bound_inc != NULL) {                                                \
      upper_bound_raw = convert_from_##cn_ty(upper_bound_inc);                           \
    }                                                                                    \
                                                                                         \
    if (upper_bound_raw < lower_bound_raw) {                                             \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      return NULL;                                                                       \
    }                                                                                    \
                                                                                         \
    bennet_domain_wint(c_ty)* cs =                                                       \
        bennet_domain_wint_of(c_ty, lower_bound_raw, upper_bound_raw);                   \
                                                                                         \
    return convert_to_##cn_ty(bennet_arbitrary_wint(c_ty, cs));                          \
  }

cn_pointer* bennet_specialized_cn_pointer(cn_pointer* lower_bound_ex,
    cn_pointer* lower_bound_inc,
    cn_pointer* upper_bound_inc,
    cn_pointer* upper_bound_ex,
    const void* vars[]) {
  uintptr_t lower_bound_raw = 0;
  if (lower_bound_ex != NULL) {
    lower_bound_raw = (uintptr_t)convert_from_cn_pointer(lower_bound_ex);
    if (lower_bound_raw == UINTPTR_MAX) {
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);
      bennet_failure_blame_many(vars);

      return NULL;
    } else {
      lower_bound_raw += 1;
      if (lower_bound_inc != NULL) {
        uintptr_t lower_bound_raw_2 = (uintptr_t)convert_from_cn_pointer(lower_bound_inc);
        lower_bound_raw =
            (lower_bound_raw > lower_bound_raw_2) ? lower_bound_raw : lower_bound_raw_2;
      }
    }
  } else if (lower_bound_inc != NULL) {
    lower_bound_raw = (uintptr_t)convert_from_cn_pointer(lower_bound_inc);
  }

  uintptr_t upper_bound_raw = UINTPTR_MAX;
  if (upper_bound_ex != NULL) {
    upper_bound_raw = (uintptr_t)convert_from_cn_pointer(upper_bound_ex);
    if (upper_bound_raw == 0) {
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);
      bennet_failure_blame_many(vars);

      return NULL;
    } else {
      upper_bound_raw -= 1;
      if (upper_bound_inc != NULL) {
        uintptr_t upper_bound_raw_2 = (uintptr_t)convert_from_cn_pointer(upper_bound_inc);
        upper_bound_raw =
            (upper_bound_raw < upper_bound_raw_2) ? upper_bound_raw : upper_bound_raw_2;
      }
    }
  } else if (upper_bound_inc != NULL) {
    upper_bound_raw = (uintptr_t)convert_from_cn_pointer(upper_bound_inc);
  }

  if (upper_bound_raw < lower_bound_raw) {
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);
    bennet_failure_blame_many(vars);

    return NULL;
  }

  struct bennet_domain_wint_uintptr_t* cs =
      bennet_domain_wint_of_uintptr_t(lower_bound_raw, upper_bound_raw);
  return convert_to_cn_pointer((void*)bennet_arbitrary_wint_uintptr_t(cs));
}

#define BENNET_SPECIALIZED_IMPL_BV(sz)                                                   \
  BENNET_SPECIALIZED_IMPL(cn_bits_u##sz, uint##sz##_t, 0, UINT##sz##_MAX)                \
  BENNET_SPECIALIZED_IMPL(cn_bits_i##sz, int##sz##_t, INT##sz##_MIN, INT##sz##_MAX)

BENNET_SPECIALIZED_IMPL_BV(8)
BENNET_SPECIALIZED_IMPL_BV(16)
BENNET_SPECIALIZED_IMPL_BV(32)
BENNET_SPECIALIZED_IMPL_BV(64)
