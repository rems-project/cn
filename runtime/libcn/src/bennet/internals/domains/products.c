#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domains/products.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/state/alloc.h>
#include <bennet/state/rand_alloc.h>
#include <cn-executable/utils.h>

#define BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(cty)                                          \
  cty bennet_domain_ownership_wint_arbitrary_##cty(                                      \
      bennet_domain_ownership(cty) * d1, bennet_domain_wint(cty) * d2) {                 \
    assert(!d1->bottom && !d2->bottom);                                                  \
                                                                                         \
    /* Only allocate */                                                                  \
    if (d1->before != 0 || d1->after != 0) {                                             \
      size_t bytes = d1->before + d1->after;                                             \
      if (bytes < d1->before || bytes < d1->after) {                                     \
        cn_failure(CN_FAILURE_ALLOC, NON_SPEC);                                          \
      }                                                                                  \
                                                                                         \
      void* p = (d2->top) ? bennet_rand_alloc(bytes)                                     \
                          : bennet_rand_alloc_bounded(                                   \
                                bytes, d2->start - d1->before, d2->end - d1->before);    \
      if (!p) {                                                                          \
        cn_failure(CN_FAILURE_ALLOC, NON_SPEC);                                          \
      }                                                                                  \
      bennet_alloc_record(p, bytes);                                                     \
                                                                                         \
      return (cty)((uintptr_t)p + d1->before);                                           \
    }                                                                                    \
                                                                                         \
    return bennet_arbitrary_wint(cty, d2);                                               \
  }

BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint64_t)

uintptr_t bennet_domain_ownership_wint_arbitrary_uintptr_t(
    bennet_domain_ownership(uintptr_t) * d1, bennet_domain_wint(uintptr_t) * d2) {
  assert(!d1->bottom);

  /* Only allocate */
  if (d1->before != 0 || d1->after != 0) {
    size_t bytes = d1->before + d1->after;
    if (bytes < d1->before || bytes < d1->after) {
      cn_failure(CN_FAILURE_ALLOC, NON_SPEC);
    }

    void* p = (d2->top) ? bennet_rand_alloc(bytes)
                        : bennet_rand_alloc_bounded(
                              bytes, d2->start - d1->before, d2->end - d1->before);
    if (!p) {
      cn_failure(CN_FAILURE_ALLOC, NON_SPEC);
    }
    bennet_alloc_record(p, bytes);

    return (uintptr_t)((uintptr_t)p + d1->before);
  }

  // Weight towards `NULL` for pointers
  // TODO: Figure out general way for generators to learn that this is useful
  // TODO: OR make this unnecessary
  uint8_t rnd = bennet_uniform_uint8_t(get_null_in_every());
  if (rnd == 0) {
    return (uintptr_t)NULL;
  }

  return bennet_arbitrary_wint(uintptr_t, d2);
};
