#include <assert.h>
#include <stdlib.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/rand_alloc.h>
#include <fulminate/api.h>

#define OWNERSHIP_FROM_ASSIGN(cty, min, max)                                             \
  bennet_domain_ownership(cty) * bennet_domain_ownership_from_assignment_##cty(          \
                                     void* base_ptr, void* addr, size_t bytes) {         \
    assert(min <= (uintptr_t)bennet_rand_alloc_max_ptr());                               \
    assert((uintptr_t)bennet_rand_alloc_min_ptr() <= max);                               \
                                                                                         \
    /* We assume that for any pointer and an allocation, */                              \
    /* the offset was the shorter distance. */                                           \
    /* Ex: base_ptr = 0xffff, addr = 0x4 -> we assume it overflowed */                   \
                                                                                         \
    uintptr_t p_raw = (uintptr_t)addr;                                                   \
    uintptr_t p_bytes_raw = p_raw + bytes;                                               \
    uintptr_t base_ptr_raw = (uintptr_t)base_ptr;                                        \
                                                                                         \
    size_t lower_offset =                                                                \
        ((base_ptr_raw - p_raw) <= (p_raw - base_ptr_raw)) ? (base_ptr_raw - p_raw) : 0; \
    size_t upper_offset =                                                                \
        ((p_bytes_raw - base_ptr_raw) <= (base_ptr_raw - (p_bytes_raw)))                 \
            ? ((p_bytes_raw) - base_ptr_raw)                                             \
            : 0;                                                                         \
                                                                                         \
    assert(lower_offset > 0 || upper_offset > 0);                                        \
                                                                                         \
    bennet_domain_ownership(cty)* d =                                                    \
        (bennet_domain_ownership(cty)*)malloc(sizeof(bennet_domain_ownership(cty)));     \
    assert(d);                                                                           \
    d->bottom = 0;                                                                       \
    d->before = lower_offset;                                                            \
    d->after = upper_offset;                                                             \
    return d;                                                                            \
  }

#define OWNERSHIP_FROM_ASSIGN_BV(bits)                                                   \
  OWNERSHIP_FROM_ASSIGN(int##bits##_t, INT##bits##_MIN, INT##bits##_MAX)                 \
  OWNERSHIP_FROM_ASSIGN(uint##bits##_t, 0, UINT##bits##_MAX)

OWNERSHIP_FROM_ASSIGN_BV(8)
OWNERSHIP_FROM_ASSIGN_BV(16)
OWNERSHIP_FROM_ASSIGN_BV(32)
OWNERSHIP_FROM_ASSIGN_BV(64)

OWNERSHIP_FROM_ASSIGN(uintptr_t, 0, UINTPTR_MAX)

#define OWNERSHIP_GEN(cty)                                                               \
  cty bennet_domain_ownership_arbitrary_##cty(bennet_domain_ownership(cty) * d) {        \
    assert(!d->bottom);                                                                  \
                                                                                         \
    if (d->before != 0 && d->after != 0) {                                               \
      size_t bytes = d->before + d->after;                                               \
      if (bytes < d->before || bytes < d->after) {                                       \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      void* p = bennet_rand_alloc(bytes);                                                \
      if (!p) {                                                                          \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
      bennet_alloc_record(p, bytes);                                                     \
                                                                                         \
      return (cty)((uintptr_t)p + d->before);                                            \
    }                                                                                    \
                                                                                         \
    return bennet_arbitrary_sized(cty, 0);                                               \
  }

OWNERSHIP_GEN(uint8_t);
OWNERSHIP_GEN(uint16_t);
OWNERSHIP_GEN(uint32_t);
OWNERSHIP_GEN(uint64_t);

OWNERSHIP_GEN(int8_t);
OWNERSHIP_GEN(int16_t);
OWNERSHIP_GEN(int32_t);
OWNERSHIP_GEN(int64_t);

uintptr_t bennet_domain_ownership_arbitrary_uintptr_t(
    bennet_domain_ownership(uintptr_t) * d) {
  assert(!d->bottom);

  /* Only allocate */
  if (d->before != 0 || d->after != 0) {
    size_t bytes = d->before + d->after;
    if (bytes < d->before || bytes < d->after) {
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    }

    void* p = bennet_rand_alloc(bytes);
    if (!p) {
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    }

    bennet_alloc_record(p, bytes);

    return (uintptr_t)((uintptr_t)p + d->before);
  }

  // Weight towards `NULL` for pointers
  // TODO: Figure out general way for generators to learn that this is useful
  // TODO: OR make this unnecessary
  uint8_t rnd = bennet_uniform_uint8_t(get_null_in_every());
  if (rnd == 0) {
    return (uintptr_t)NULL;
  }

  return bennet_arbitrary_sized(uintptr_t, 0);
};

#define OWNERSHIP_CHECK_IMPL(cty)                                                        \
  bool bennet_domain_ownership_check_##cty(cty v, bennet_domain_ownership(cty) * d) {    \
    if (d->bottom) {                                                                     \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    if (d->before == 0 && d->after == 0) {                                               \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    return bennet_alloc_check((void*)((uintptr_t)v - d->before), d->after);              \
  }

OWNERSHIP_CHECK_IMPL(int8_t)
OWNERSHIP_CHECK_IMPL(int16_t)
OWNERSHIP_CHECK_IMPL(int32_t)
OWNERSHIP_CHECK_IMPL(int64_t)

OWNERSHIP_CHECK_IMPL(uint8_t)
OWNERSHIP_CHECK_IMPL(uint16_t)
OWNERSHIP_CHECK_IMPL(uint32_t)
OWNERSHIP_CHECK_IMPL(uint64_t)

OWNERSHIP_CHECK_IMPL(uintptr_t)
