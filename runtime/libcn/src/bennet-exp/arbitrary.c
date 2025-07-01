#include <assert.h>
#include <stdbool.h>

#include <bennet-exp/alloc.h>
#include <bennet-exp/arbitrary.h>
#include <bennet-exp/domain.h>
#include <bennet-exp/failure.h>
#include <bennet-exp/rand.h>
#include <bennet-exp/size.h>
#include <cn-executable/utils.h>

#define BENNET_ARBITRARY_IMPL(sz)                                                        \
  cn_bits_u##sz* bennet_arbitrary_cn_bits_u##sz(bennet_domain(uint##sz##_t) * cs) {      \
    if (cs->is_owned) {                                                                  \
      bennet_domain(uintptr_t) cast_cs = bennet_domain_cast(uintptr_t, cs);              \
                                                                                         \
      return cast_cn_pointer_to_cn_bits_u##sz(bennet_alloc_unsigned(&cast_cs));          \
    }                                                                                    \
    if (bennet_optional_is_some(cs->multiple)) {                                         \
      uint##sz##_t mult = bennet_optional_unwrap(cs->multiple);                          \
      if (bennet_optional_is_some(cs->lower_bound_inc) ||                                \
          bennet_optional_is_some(cs->upper_bound_inc)) {                                \
        uint##sz##_t min = bennet_optional_unwrap_or(cs->lower_bound_inc, 0);            \
        uint##sz##_t max =                                                               \
            bennet_optional_unwrap_or(cs->upper_bound_inc, UINT##sz##_MAX);              \
        return convert_to_cn_bits_u##sz(bennet_mult_range_u##sz(mult, min, max));        \
      }                                                                                  \
                                                                                         \
      return convert_to_cn_bits_u##sz(bennet_mult_u##sz(mult));                          \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc) &&                                  \
        bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_cn_bits_u##sz(                                                   \
          bennet_range_u##sz(bennet_optional_unwrap(cs->lower_bound_inc),                \
              bennet_optional_unwrap(cs->upper_bound_inc)));                             \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc)) {                                  \
      return convert_to_cn_bits_u##sz(                                                   \
          bennet_ge_u##sz(bennet_optional_unwrap(cs->lower_bound_inc)));                 \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_cn_bits_u##sz(                                                   \
          bennet_le_u##sz(bennet_optional_unwrap(cs->upper_bound_inc)));                 \
    }                                                                                    \
                                                                                         \
    return convert_to_cn_bits_u##sz(bennet_uniform_u##sz##_sized(0));                    \
  }                                                                                      \
                                                                                         \
  cn_bits_i##sz* bennet_arbitrary_cn_bits_i##sz(bennet_domain(int##sz##_t) * cs) {       \
    if (cs->is_owned) {                                                                  \
      bennet_domain(intptr_t) cast_cs = bennet_domain_cast(intptr_t, cs);                \
                                                                                         \
      return cast_cn_pointer_to_cn_bits_i##sz(bennet_alloc_signed(&cast_cs));            \
    }                                                                                    \
    if (bennet_optional_is_some(cs->multiple)) {                                         \
      int##sz##_t mult = bennet_optional_unwrap(cs->multiple);                           \
      if (bennet_optional_is_some(cs->lower_bound_inc) ||                                \
          bennet_optional_is_some(cs->upper_bound_inc)) {                                \
        int##sz##_t min = bennet_optional_unwrap_or(cs->lower_bound_inc, 0);             \
        int##sz##_t max = bennet_optional_unwrap_or(cs->upper_bound_inc, INT##sz##_MAX); \
        return convert_to_cn_bits_i##sz(bennet_mult_range_i##sz(mult, min, max));        \
      }                                                                                  \
                                                                                         \
      return convert_to_cn_bits_i##sz(bennet_mult_i##sz(mult));                          \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc) &&                                  \
        bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_cn_bits_i##sz(                                                   \
          bennet_range_i##sz(bennet_optional_unwrap(cs->lower_bound_inc),                \
              bennet_optional_unwrap(cs->upper_bound_inc)));                             \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc)) {                                  \
      return convert_to_cn_bits_i##sz(                                                   \
          bennet_ge_i##sz(bennet_optional_unwrap(cs->lower_bound_inc)));                 \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_cn_bits_i##sz(                                                   \
          bennet_le_i##sz(bennet_optional_unwrap(cs->upper_bound_inc)));                 \
    }                                                                                    \
                                                                                         \
    return convert_to_cn_bits_i##sz(bennet_uniform_i##sz##_sized(0));                    \
  }

BENNET_ARBITRARY_IMPL(8)
BENNET_ARBITRARY_IMPL(16)
BENNET_ARBITRARY_IMPL(32)
BENNET_ARBITRARY_IMPL(64)
