#include <assert.h>
#include <stdlib.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/rand_alloc.h>

#define OWNERSHIP_FROM_ASSIGN(cty, min, max)                                             \
  bennet_domain_ownership(cty) * bennet_domain_ownership_from_assignment_##cty(          \
                                     void* base_ptr, void* addr, size_t bytes) {         \
    if (bytes == 0) {                                                                    \
      return bennet_domain_ownership_top(cty);                                           \
    }                                                                                    \
                                                                                         \
    /* If bytes exceeds allocator capacity, assignment is unsatisfiable */               \
    {                                                                                    \
      uintptr_t _alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                     \
      uintptr_t _alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                     \
      size_t _alloc_capacity = _alloc_max - _alloc_min + 1;                              \
      if (bytes > _alloc_capacity) {                                                     \
        return bennet_domain_ownership_bottom(cty);                                      \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
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
    /* When offsets are both zero, the assignment range is nonsensical              */   \
    /* (e.g., due to unsigned underflow in range size computation). Return top.     */   \
    if (lower_offset == 0 && upper_offset == 0) {                                        \
      return bennet_domain_ownership_top(cty);                                           \
    }                                                                                    \
                                                                                         \
    bennet_domain_ownership(cty)* d =                                                    \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
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

#define OWNERSHIP_TO_INTERVAL(cty)                                                       \
  bool bennet_domain_ownership_to_interval_##cty(                                        \
      bennet_domain_ownership(cty) * d, cty * lo_out, cty * hi_out) {                    \
    if (sizeof(cty) < sizeof(uintptr_t)) {                                               \
      return false;                                                                      \
    }                                                                                    \
    if (d->bottom) {                                                                     \
      return false;                                                                      \
    }                                                                                    \
    /* Top: no constraint */                                                             \
    if (d->before == 0 && d->after == 0) {                                               \
      return false;                                                                      \
    }                                                                                    \
    uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                        \
    uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                        \
    /* Check for overflow */                                                             \
    if (d->before > alloc_max || d->after > alloc_max) {                                 \
      return false;                                                                      \
    }                                                                                    \
    uintptr_t lo = alloc_min + d->before;                                                \
    if (lo < alloc_min) {                                                                \
      return false;                                                                      \
    }                                                                                    \
    if (d->after > alloc_max) {                                                          \
      return false;                                                                      \
    }                                                                                    \
    uintptr_t hi = alloc_max - d->after + 1;                                             \
    if (hi > alloc_max || hi < lo) {                                                     \
      return false;                                                                      \
    }                                                                                    \
    *lo_out = (cty)lo;                                                                   \
    *hi_out = (cty)hi;                                                                   \
    return true;                                                                         \
  }

#define OWNERSHIP_OF_INTERVAL(cty)                                                       \
  bennet_domain_ownership(cty) *                                                         \
      bennet_domain_ownership_of_interval_##cty(cty lo, cty hi) {                        \
    if (sizeof(cty) < sizeof(uintptr_t)) {                                               \
      return bennet_domain_ownership_top(cty);                                           \
    }                                                                                    \
    uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                        \
    uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                        \
    uintptr_t lo_ptr = (uintptr_t)lo;                                                    \
    uintptr_t hi_ptr = (uintptr_t)hi;                                                    \
    if (lo_ptr < alloc_min || hi_ptr > alloc_max || lo_ptr > hi_ptr) {                   \
      return bennet_domain_ownership_top(cty);                                           \
    }                                                                                    \
    size_t before = lo_ptr - alloc_min;                                                  \
    size_t after = alloc_max - hi_ptr;                                                   \
    return bennet_domain_ownership_of(cty, before, after);                               \
  }

OWNERSHIP_TO_INTERVAL(uint8_t)
OWNERSHIP_TO_INTERVAL(uint16_t)
OWNERSHIP_TO_INTERVAL(uint32_t)
OWNERSHIP_TO_INTERVAL(uint64_t)
OWNERSHIP_TO_INTERVAL(int8_t)
OWNERSHIP_TO_INTERVAL(int16_t)
OWNERSHIP_TO_INTERVAL(int32_t)
OWNERSHIP_TO_INTERVAL(int64_t)
OWNERSHIP_TO_INTERVAL(uintptr_t)

OWNERSHIP_OF_INTERVAL(uint8_t)
OWNERSHIP_OF_INTERVAL(uint16_t)
OWNERSHIP_OF_INTERVAL(uint32_t)
OWNERSHIP_OF_INTERVAL(uint64_t)
OWNERSHIP_OF_INTERVAL(int8_t)
OWNERSHIP_OF_INTERVAL(int16_t)
OWNERSHIP_OF_INTERVAL(int32_t)
OWNERSHIP_OF_INTERVAL(int64_t)
OWNERSHIP_OF_INTERVAL(uintptr_t)

#define OWNERSHIP_GEN(cty)                                                               \
  cty bennet_domain_ownership_arbitrary_##cty(bennet_domain_ownership(cty) * d) {        \
    assert(!d->bottom);                                                                  \
                                                                                         \
    if (d->before != 0 || d->after != 0) {                                               \
      size_t bytes = d->before + d->after;                                               \
      if (bytes < d->before || bytes < d->after) {                                       \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      void* p = bennet_alloc(bytes);                                                     \
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

    void* p = bennet_alloc(bytes);

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

#define OWNERSHIP_MEMBER_SHIFT(cty)                                                      \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_member_shift_##cty(                                               \
          bennet_domain_ownership(cty) * base, size_t offset) {                          \
    if (base->bottom) {                                                                  \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
                                                                                         \
    /* Top stays top - no ownership info to adjust */                                    \
    if (base->before == 0 && base->after == 0) {                                         \
      return bennet_domain_ownership_top(cty);                                           \
    }                                                                                    \
                                                                                         \
    /* Shifting forward by offset bytes: */                                              \
    /* - more bytes now before the pointer */                                            \
    /* - fewer bytes after the pointer */                                                \
    size_t new_before = base->before + offset;                                           \
    /* Check for overflow in before */                                                   \
    if (new_before < base->before) {                                                     \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
                                                                                         \
    /* Check that we don't shift past the allocation */                                  \
    if (offset > base->after) {                                                          \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
    size_t new_after = base->after - offset;                                             \
                                                                                         \
    return bennet_domain_ownership_of(cty, new_before, new_after);                       \
  }

#define OWNERSHIP_ARRAY_SHIFT(cty)                                                       \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_array_shift_##cty(                                                \
          bennet_domain_ownership(cty) * base, size_t elem_size, size_t index) {         \
    /* array_shift is just member_shift with offset = elem_size * index */               \
    size_t offset = elem_size * index;                                                   \
    /* Check for multiplication overflow */                                              \
    if (elem_size != 0 && offset / elem_size != index) {                                 \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
    return bennet_ownership_member_shift_##cty(base, offset);                            \
  }

#define OWNERSHIP_MEMBER_SHIFT_BACKWARD(cty)                                             \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_member_shift_backward_##cty(                                      \
          bennet_domain_ownership(cty) * shifted, size_t offset) {                       \
    if (shifted->bottom) {                                                               \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
                                                                                         \
    if (shifted->before == 0 && shifted->after == 0) {                                   \
      return bennet_domain_ownership_top(cty);                                           \
    }                                                                                    \
                                                                                         \
    /* Inverse of forward: undo the shift */                                             \
    /* shifted.before = base.before + offset -> base.before = shifted.before - offset */ \
    /* shifted.after = base.after - offset -> base.after = shifted.after + offset */     \
    size_t new_after = shifted->after + offset;                                          \
    /* Check for overflow in after */                                                    \
    if (new_after < shifted->after) {                                                    \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
    /* When offset > before, base is before the owned range but can still be */          \
    /* the start of a larger allocation covering [base, base + after + offset) */        \
    size_t new_before = (offset > shifted->before) ? 0 : shifted->before - offset;       \
                                                                                         \
    return bennet_domain_ownership_of(cty, new_before, new_after);                       \
  }

#define OWNERSHIP_ARRAY_SHIFT_BACKWARD(cty)                                              \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_array_shift_backward_##cty(                                       \
          bennet_domain_ownership(cty) * shifted, size_t elem_size, size_t index) {      \
    size_t offset = elem_size * index;                                                   \
    if (elem_size != 0 && offset / elem_size != index) {                                 \
      return bennet_domain_ownership_bottom(cty);                                        \
    }                                                                                    \
    return bennet_ownership_member_shift_backward_##cty(shifted, offset);                \
  }

#define OWNERSHIP_SHIFT_ALL(cty)                                                         \
  OWNERSHIP_MEMBER_SHIFT(cty)                                                            \
  OWNERSHIP_ARRAY_SHIFT(cty)                                                             \
  OWNERSHIP_MEMBER_SHIFT_BACKWARD(cty)                                                   \
  OWNERSHIP_ARRAY_SHIFT_BACKWARD(cty)

OWNERSHIP_SHIFT_ALL(uint8_t)
OWNERSHIP_SHIFT_ALL(uint16_t)
OWNERSHIP_SHIFT_ALL(uint32_t)
OWNERSHIP_SHIFT_ALL(uint64_t)
OWNERSHIP_SHIFT_ALL(int8_t)
OWNERSHIP_SHIFT_ALL(int16_t)
OWNERSHIP_SHIFT_ALL(int32_t)
OWNERSHIP_SHIFT_ALL(int64_t)
OWNERSHIP_SHIFT_ALL(uintptr_t)
