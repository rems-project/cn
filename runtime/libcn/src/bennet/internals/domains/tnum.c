#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils.h>
#include <cn-smt/memory/std_alloc.h>

// Helper: count number of 1 bits (popcount)
#define COUNT_ONES(sm)                                                                   \
  static int count_ones_##sm(uint##sm##_t n) {                                           \
    int count = 0;                                                                       \
    for (int i = 0; i < sm; i++) {                                                       \
      if (((n >> i) & 1)) {                                                              \
        count++;                                                                         \
      }                                                                                  \
    }                                                                                    \
    return count;                                                                        \
  }

COUNT_ONES(8)
COUNT_ONES(16)
COUNT_ONES(32)
COUNT_ONES(64)

// Arbitrary generation for tnum domains
#define TNUM_GEN(sm)                                                                     \
  uint##sm##_t bennet_arbitrary_tnum_uint##sm##_t(                                       \
      bennet_domain_tnum(uint##sm##_t) * d) {                                            \
    if (d->bottom) {                                                                     \
      assert(false && "Cannot generate value from bottom tnum");                         \
      return 0;                                                                          \
    }                                                                                    \
                                                                                         \
    assert((d->value & d->mask) == 0);                                                   \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
                                                                                         \
    uint##sm##_t end = 0;                                                                \
    for (int i = 0; i < count_ones_##sm(d->mask); i++) {                                 \
      end <<= 1;                                                                         \
      end |= 1;                                                                          \
                                                                                         \
      if (end > sz) {                                                                    \
        break;                                                                           \
      }                                                                                  \
    }                                                                                    \
    if (end > sz) {                                                                      \
      end = sz;                                                                          \
    }                                                                                    \
                                                                                         \
    uint##sm##_t bits = bennet_range_uint##sm##_t(0, end);                               \
    uint##sm##_t res = d->value;                                                         \
                                                                                         \
    for (uint##sm##_t i = 0; i < sm && bits != 0; i++) {                                 \
      if (d->mask & ((uint##sm##_t)1 << i)) {                                            \
        res |= (bits & 1) << i;                                                          \
        bits >>= 1;                                                                      \
      }                                                                                  \
    }                                                                                    \
    assert(bits == 0);                                                                   \
                                                                                         \
    return res;                                                                          \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_arbitrary_tnum_int##sm##_t(bennet_domain_tnum(int##sm##_t) * d) {   \
    if (d->bottom) {                                                                     \
      assert(false && "Cannot generate value from bottom tnum");                         \
      return 0;                                                                          \
    }                                                                                    \
                                                                                         \
    assert((d->value & d->mask) == 0);                                                   \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
                                                                                         \
    uint##sm##_t end = 0;                                                                \
    for (int i = 0; i < count_ones_##sm(d->mask); i++) {                                 \
      end <<= 1;                                                                         \
      end |= 1;                                                                          \
                                                                                         \
      if (end > sz) {                                                                    \
        break;                                                                           \
      }                                                                                  \
    }                                                                                    \
    if (end > sz) {                                                                      \
      end = sz;                                                                          \
    }                                                                                    \
                                                                                         \
    uint##sm##_t sign = UINT##sm##_C(1) << (sm - 1);                                     \
    if ((uint##sm##_t)d->mask & sign) {                                                  \
      end <<= 1;                                                                         \
      end |= 1;                                                                          \
    }                                                                                    \
                                                                                         \
    uint##sm##_t bits = bennet_range_uint##sm##_t(0, end);                               \
    uint##sm##_t res = d->value;                                                         \
                                                                                         \
    if ((uint##sm##_t)d->mask & sign) {                                                  \
      if (bits & 1) {                                                                    \
        res |= sign;                                                                     \
      }                                                                                  \
      bits >>= 1;                                                                        \
    }                                                                                    \
                                                                                         \
    for (uint##sm##_t i = 0; i < sm && bits != 0; i++) {                                 \
      uint##sm##_t shift = ((uint##sm##_t)d->value & sign) ? (sm - 1 - i) : i;           \
      if ((uint##sm##_t)d->mask & ((uint##sm##_t)1 << shift)) {                          \
        res |= (bits & 1) << shift;                                                      \
        bits >>= 1;                                                                      \
      }                                                                                  \
    }                                                                                    \
    assert(bits == 0);                                                                   \
                                                                                         \
    return res;                                                                          \
  }

TNUM_GEN(8)
TNUM_GEN(16)
TNUM_GEN(32)
TNUM_GEN(64)

// uintptr_t uses uint64_t implementation
uintptr_t bennet_arbitrary_tnum_uintptr_t(bennet_domain_tnum(uintptr_t) * d) {
  assert(sizeof(uintptr_t) == sizeof(uint64_t));
  return (uintptr_t)bennet_arbitrary_tnum_uint64_t((bennet_domain_tnum(uint64_t)*)d);
}

// Generate complete abstract domain interface for each type
#define TNUM_DOMAIN_IMPL(cty, FULL_MASK)                                                 \
  bennet_domain_tnum(cty) * bennet_domain_tnum_top_##cty(void) {                         \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));       \
    assert(result);                                                                      \
    result->top = true;                                                                  \
    result->bottom = false;                                                              \
    result->value = 0;                                                                   \
    result->mask = (cty)(FULL_MASK);                                                     \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bennet_domain_tnum(cty) * bennet_domain_tnum_bottom_##cty(void) {                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));       \
    assert(result);                                                                      \
    result->top = false;                                                                 \
    result->bottom = true;                                                               \
    result->value = 0;                                                                   \
    result->mask = 0;                                                                    \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bennet_domain_tnum(cty) * bennet_domain_tnum_of_##cty(cty value, cty mask) {           \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));       \
    assert(result);                                                                      \
    /* Ensure well-formedness: value & mask == 0 */                                      \
    result->value = value & ~mask;                                                       \
    result->mask = mask;                                                                 \
    result->top = (result->value == 0 && result->mask == (cty)(FULL_MASK));              \
    result->bottom = false;                                                              \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_tnum_is_top_##cty(bennet_domain_tnum(cty) * d) {                    \
    return d->top;                                                                       \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_tnum_is_bottom_##cty(bennet_domain_tnum(cty) * d) {                 \
    return d->bottom;                                                                    \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_tnum_equal_##cty(                                                   \
      bennet_domain_tnum(cty) * d1, bennet_domain_tnum(cty) * d2) {                      \
    if (d1->top && d2->top)                                                              \
      return true;                                                                       \
    if (d1->bottom && d2->bottom)                                                        \
      return true;                                                                       \
    if (d1->top || d1->bottom || d2->top || d2->bottom)                                  \
      return false;                                                                      \
    return d1->value == d2->value && d1->mask == d2->mask;                               \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_tnum_leq_##cty(                                                     \
      bennet_domain_tnum(cty) * d1, bennet_domain_tnum(cty) * d2) {                      \
    if (d1->bottom)                                                                      \
      return true;                                                                       \
    if (d2->top)                                                                         \
      return true;                                                                       \
    if (d1->top && !d2->top)                                                             \
      return false;                                                                      \
    if (d2->bottom)                                                                      \
      return false;                                                                      \
                                                                                         \
    /* d1 <= d2 iff d1 has more known bits that agree with d2's constraints */           \
    /* d1 must know at least all bits d2 knows */                                        \
    cty known_in_d2 = ~d2->mask;                                                         \
    cty known_in_d1 = ~d1->mask;                                                         \
    bool knows_enough = ((known_in_d2 & known_in_d1) == known_in_d2);                    \
    /* For known bits in d2, values must match */                                        \
    bool values_match = ((d1->value & known_in_d2) == d2->value);                        \
    return knows_enough && values_match;                                                 \
  }                                                                                      \
                                                                                         \
  bennet_domain_tnum(cty) * bennet_domain_tnum_join_##cty(bennet_domain_tnum(cty) * d1,  \
                                bennet_domain_tnum(cty) * d2) {                          \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));       \
    assert(result);                                                                      \
                                                                                         \
    if (d1->top || d2->top) {                                                            \
      result->top = true;                                                                \
      result->bottom = false;                                                            \
      result->value = 0;                                                                 \
      result->mask = (cty)(FULL_MASK);                                                   \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    if (d1->bottom) {                                                                    \
      *result = *d2;                                                                     \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    if (d2->bottom) {                                                                    \
      *result = *d1;                                                                     \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    /* Join: bits known in both with same value stay known, others become unknown */     \
    cty known_both = (~d1->mask) & (~d2->mask);                                          \
    cty diff = d1->value ^ d2->value;                                                    \
    cty conflict = known_both & diff;                                                    \
    /* New mask: unknown in either operand, or conflicting values */                     \
    result->mask = d1->mask | d2->mask | conflict;                                       \
    /* New value: keep only bits that are known and agree */                             \
    result->value = (d1->value & d2->value) & (~result->mask);                           \
    result->top = (result->value == 0 && result->mask == (cty)(FULL_MASK));              \
    result->bottom = false;                                                              \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bennet_domain_tnum(cty) * bennet_domain_tnum_meet_##cty(bennet_domain_tnum(cty) * d1,  \
                                bennet_domain_tnum(cty) * d2) {                          \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));       \
    assert(result);                                                                      \
                                                                                         \
    if (d1->bottom || d2->bottom) {                                                      \
      result->top = false;                                                               \
      result->bottom = true;                                                             \
      result->value = 0;                                                                 \
      result->mask = 0;                                                                  \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    if (d1->top) {                                                                       \
      *result = *d2;                                                                     \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    if (d2->top) {                                                                       \
      *result = *d1;                                                                     \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    /* Check for conflict: both known but different values */                            \
    cty known_in_d1 = ~d1->mask;                                                         \
    cty known_in_d2 = ~d2->mask;                                                         \
    cty conflict = (known_in_d1 & known_in_d2) & (d1->value ^ d2->value);                \
    if (conflict != 0) {                                                                 \
      result->top = false;                                                               \
      result->bottom = true;                                                             \
      result->value = 0;                                                                 \
      result->mask = 0;                                                                  \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    /* Meet: combine known bits from both */                                             \
    result->value = d1->value | d2->value;                                               \
    /* Unknown only where both are unknown */                                            \
    result->mask = d1->mask & d2->mask;                                                  \
    result->top = (result->value == 0 && result->mask == (cty)(FULL_MASK));              \
    result->bottom = false;                                                              \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bennet_domain_tnum(cty) * bennet_domain_tnum_copy_##cty(bennet_domain_tnum(cty) * d) { \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));       \
    assert(result);                                                                      \
    *result = *d;                                                                        \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  cty bennet_domain_tnum_arbitrary_##cty(bennet_domain_tnum(cty) * d) {                  \
    return bennet_arbitrary_tnum_##cty(d);                                               \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_tnum_check_##cty(cty v, bennet_domain_tnum(cty) * d) {              \
    if (d->bottom) {                                                                     \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    if (d->top) {                                                                        \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    /* Check that v satisfies the tnum constraint: (v & ~mask) == value */               \
    return (v & ~d->mask) == d->value;                                                   \
  }                                                                                      \
                                                                                         \
  bennet_domain_tnum(cty) * bennet_domain_tnum_from_assignment_##cty(                    \
                                void* base_ptr, void* addr, size_t bytes) {              \
    /* Conservative: return top for any assignment */                                    \
    return bennet_domain_tnum_top_##cty();                                               \
  }

TNUM_DOMAIN_IMPL(uint8_t, UINT8_MAX)
TNUM_DOMAIN_IMPL(uint16_t, UINT16_MAX)
TNUM_DOMAIN_IMPL(uint32_t, UINT32_MAX)
TNUM_DOMAIN_IMPL(uint64_t, UINT64_MAX)
TNUM_DOMAIN_IMPL(uintptr_t, UINTPTR_MAX)
TNUM_DOMAIN_IMPL(int8_t, UINT8_MAX)
TNUM_DOMAIN_IMPL(int16_t, UINT16_MAX)
TNUM_DOMAIN_IMPL(int32_t, UINT32_MAX)
TNUM_DOMAIN_IMPL(int64_t, UINT64_MAX)
