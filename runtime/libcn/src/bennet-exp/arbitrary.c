#include <assert.h>
#include <stdbool.h>

#include <bennet-exp/alloc.h>
#include <bennet-exp/arbitrary.h>
#include <bennet-exp/domain.h>
#include <bennet-exp/failure.h>
#include <bennet-exp/size.h>
#include <bennet-exp/uniform.h>
#include <cn-executable/utils.h>

#define BENNET_ARBITRARY_IMPL(sz)                                                        \
  cn_bits_u##sz* bennet_arbitrary_cn_bits_u##sz(bennet_domain(uint##sz##_t) * cs) {      \
    if (cs->is_owned) {                                                                  \
      bennet_domain(uintptr_t) cast_cs = bennet_domain_cast(uintptr_t, cs);              \
                                                                                         \
      return cast_cn_pointer_to_cn_bits_u##sz(bennet_alloc_unsigned(&cast_cs));          \
    }                                                                                    \
                                                                                         \
    return bennet_uniform_cn_bits_u##sz(0);                                              \
  }                                                                                      \
                                                                                         \
  cn_bits_i##sz* bennet_arbitrary_cn_bits_i##sz(bennet_domain(int##sz##_t) * cs) {       \
    if (cs->is_owned) {                                                                  \
      bennet_domain(intptr_t) cast_cs = bennet_domain_cast(intptr_t, cs);                \
                                                                                         \
      return cast_cn_pointer_to_cn_bits_i##sz(bennet_alloc_signed(&cast_cs));            \
    }                                                                                    \
                                                                                         \
    return bennet_uniform_cn_bits_i##sz(0);                                              \
  }

BENNET_ARBITRARY_IMPL(8)
BENNET_ARBITRARY_IMPL(16)
BENNET_ARBITRARY_IMPL(32)
BENNET_ARBITRARY_IMPL(64)
