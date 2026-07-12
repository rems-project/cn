#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
#define TNUM_DOMAIN_IMPL(cty, FULL_MASK)                                                   \
  bennet_domain_tnum(cty) * bennet_domain_tnum_top_##cty(void) {                           \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    result->top = true;                                                                    \
    result->bottom = false;                                                                \
    result->value = 0;                                                                     \
    result->mask = (cty)(FULL_MASK);                                                       \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  bennet_domain_tnum(cty) * bennet_domain_tnum_bottom_##cty(void) {                        \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    result->top = false;                                                                   \
    result->bottom = true;                                                                 \
    result->value = 0;                                                                     \
    result->mask = 0;                                                                      \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  bennet_domain_tnum(cty) * bennet_domain_tnum_of_##cty(cty value, cty mask) {             \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    /* Ensure well-formedness: value & mask == 0 */                                        \
    result->value = value & ~mask;                                                         \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == (cty)(FULL_MASK));                \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  bool bennet_domain_tnum_is_top_##cty(bennet_domain_tnum(cty) * d) {                      \
    return d->top;                                                                         \
  }                                                                                        \
                                                                                           \
  bool bennet_domain_tnum_is_bottom_##cty(bennet_domain_tnum(cty) * d) {                   \
    return d->bottom;                                                                      \
  }                                                                                        \
                                                                                           \
  bool bennet_domain_tnum_equal_##cty(                                                     \
      bennet_domain_tnum(cty) * d1, bennet_domain_tnum(cty) * d2) {                        \
    if (d1->top && d2->top)                                                                \
      return true;                                                                         \
    if (d1->bottom && d2->bottom)                                                          \
      return true;                                                                         \
    if (d1->top || d1->bottom || d2->top || d2->bottom)                                    \
      return false;                                                                        \
    return d1->value == d2->value && d1->mask == d2->mask;                                 \
  }                                                                                        \
                                                                                           \
  bool bennet_domain_tnum_leq_##cty(                                                       \
      bennet_domain_tnum(cty) * d1, bennet_domain_tnum(cty) * d2) {                        \
    if (d1->bottom)                                                                        \
      return true;                                                                         \
    if (d2->top)                                                                           \
      return true;                                                                         \
    if (d1->top && !d2->top)                                                               \
      return false;                                                                        \
    if (d2->bottom)                                                                        \
      return false;                                                                        \
                                                                                           \
    /* d1 <= d2 iff d1 has more known bits that agree with d2's constraints */             \
    /* d1 must know at least all bits d2 knows */                                          \
    cty known_in_d2 = ~d2->mask;                                                           \
    cty known_in_d1 = ~d1->mask;                                                           \
    bool knows_enough = ((known_in_d2 & known_in_d1) == known_in_d2);                      \
    /* For known bits in d2, values must match */                                          \
    bool values_match = ((d1->value & known_in_d2) == d2->value);                          \
    return knows_enough && values_match;                                                   \
  }                                                                                        \
                                                                                           \
  bennet_domain_tnum(cty) * bennet_domain_tnum_join_##cty(bennet_domain_tnum(cty) * d1,    \
                                bennet_domain_tnum(cty) * d2) {                            \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
                                                                                           \
    if (d1->top || d2->top) {                                                              \
      result->top = true;                                                                  \
      result->bottom = false;                                                              \
      result->value = 0;                                                                   \
      result->mask = (cty)(FULL_MASK);                                                     \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    if (d1->bottom) {                                                                      \
      *result = *d2;                                                                       \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    if (d2->bottom) {                                                                      \
      *result = *d1;                                                                       \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    /* Join: bits known in both with same value stay known, others become unknown */       \
    cty known_both = (~d1->mask) & (~d2->mask);                                            \
    cty diff = d1->value ^ d2->value;                                                      \
    cty conflict = known_both & diff;                                                      \
    /* New mask: unknown in either operand, or conflicting values */                       \
    result->mask = d1->mask | d2->mask | conflict;                                         \
    /* New value: keep only bits that are known and agree */                               \
    result->value = (d1->value & d2->value) & (~result->mask);                             \
    result->top = (result->value == 0 && result->mask == (cty)(FULL_MASK));                \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  bennet_domain_tnum(cty) * bennet_domain_tnum_meet_##cty(bennet_domain_tnum(cty) * d1,    \
                                bennet_domain_tnum(cty) * d2) {                            \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
                                                                                           \
    if (d1->bottom || d2->bottom) {                                                        \
      result->top = false;                                                                 \
      result->bottom = true;                                                               \
      result->value = 0;                                                                   \
      result->mask = 0;                                                                    \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    if (d1->top) {                                                                         \
      *result = *d2;                                                                       \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    if (d2->top) {                                                                         \
      *result = *d1;                                                                       \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    /* Check for conflict: both known but different values */                              \
    cty known_in_d1 = ~d1->mask;                                                           \
    cty known_in_d2 = ~d2->mask;                                                           \
    cty conflict = (known_in_d1 & known_in_d2) & (d1->value ^ d2->value);                  \
    if (conflict != 0) {                                                                   \
      result->top = false;                                                                 \
      result->bottom = true;                                                               \
      result->value = 0;                                                                   \
      result->mask = 0;                                                                    \
      return result;                                                                       \
    }                                                                                      \
                                                                                           \
    /* Meet: combine known bits from both */                                               \
    result->value = d1->value | d2->value;                                                 \
    /* Unknown only where both are unknown */                                              \
    result->mask = d1->mask & d2->mask;                                                    \
    result->top = (result->value == 0 && result->mask == (cty)(FULL_MASK));                \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  bennet_domain_tnum(cty) * bennet_domain_tnum_copy_##cty(bennet_domain_tnum(cty) * d) {   \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    *result = *d;                                                                          \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  cty bennet_domain_tnum_arbitrary_##cty(bennet_domain_tnum(cty) * d) {                    \
    return bennet_arbitrary_tnum_##cty(d);                                                 \
  }                                                                                        \
                                                                                           \
  bool bennet_domain_tnum_check_##cty(cty v, bennet_domain_tnum(cty) * d) {                \
    if (d->bottom) {                                                                       \
      return false;                                                                        \
    }                                                                                      \
                                                                                           \
    if (d->top) {                                                                          \
      return true;                                                                         \
    }                                                                                      \
                                                                                           \
    /* Check that v satisfies the tnum constraint: (v & ~mask) == value */                 \
    return (v & ~d->mask) == d->value;                                                     \
  }                                                                                        \
                                                                                           \
  bennet_domain_tnum(cty) * bennet_domain_tnum_from_assignment_##cty(                      \
                                void* base_ptr, void* addr, size_t bytes) {                \
    if (sizeof(cty) == sizeof(uintptr_t) && bytes > 0) {                                   \
      uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();                          \
      uintptr_t max_ptr = (uintptr_t)bennet_rand_alloc_max_ptr();                          \
      uintptr_t offset = (uintptr_t)addr - (uintptr_t)base_ptr;                            \
      if (offset > min_ptr) {                                                              \
        return bennet_domain_tnum_top_##cty();                                             \
      }                                                                                    \
      uintptr_t lo = min_ptr - offset;                                                     \
      if (offset + bytes - 1 > max_ptr) {                                                  \
        return bennet_domain_tnum_top_##cty();                                             \
      }                                                                                    \
      uintptr_t hi = max_ptr - offset - bytes + 1;                                         \
      if (hi < lo) {                                                                       \
        return bennet_domain_tnum_top_##cty();                                             \
      }                                                                                    \
      return bennet_domain_tnum_of_interval_##cty((cty)lo, (cty)hi);                       \
    }                                                                                      \
    return bennet_domain_tnum_top_##cty();                                                 \
  }                                                                                        \
                                                                                           \
  /* Bitwise AND: optimal algorithm                                                      \
     - If either input is known 0 -> output is known 0                                   \
     - If both inputs are known 1 -> output is known 1                                   \
     - Otherwise -> output is unknown */ \
  bennet_domain_tnum(cty) * bennet_domain_tnum_and_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* Output value: bits that are known 1 in both inputs */                               \
    cty value = d1->value & d2->value;                                                     \
    /* Known 0 bits in each operand */                                                     \
    cty k0_d1 = (~d1->value) & (~d1->mask);                                                \
    cty k0_d2 = (~d2->value) & (~d2->mask);                                                \
    /* Output bits that are known 0 */                                                     \
    cty out_k0 = k0_d1 | k0_d2;                                                            \
    /* Known bits = k0 | value */                                                          \
    cty known = out_k0 | value;                                                            \
    /* Mask = unknown bits */                                                              \
    cty mask = fm & (~known);                                                              \
    result->value = value & (~mask);                                                       \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Bitwise OR: optimal algorithm                                                       \
     - If either input is known 1 -> output is known 1                                   \
     - If both inputs are known 0 -> output is known 0                                   \
     - Otherwise -> output is unknown */ \
  bennet_domain_tnum(cty) * bennet_domain_tnum_or_##cty(bennet_domain_tnum(cty) * d1,      \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* Output value: bits that are known 1 in either input */                              \
    cty value = d1->value | d2->value;                                                     \
    /* Known 0 bits in each operand */                                                     \
    cty k0_d1 = (~d1->value) & (~d1->mask);                                                \
    cty k0_d2 = (~d2->value) & (~d2->mask);                                                \
    /* Output bits that are known 0: both inputs must be known 0 */                        \
    cty out_k0 = k0_d1 & k0_d2;                                                            \
    /* Known bits = k0 | value */                                                          \
    cty known = out_k0 | value;                                                            \
    /* Mask = unknown bits */                                                              \
    cty mask = fm & (~known);                                                              \
    result->value = value & (~mask);                                                       \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Bitwise XOR:                                                                        \
     - If both inputs are known -> output is known (xor of values)                       \
     - Otherwise -> output is unknown */ \
  bennet_domain_tnum(cty) * bennet_domain_tnum_xor_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* XOR of known values */                                                              \
    cty value = d1->value ^ d2->value;                                                     \
    /* Output is known only where both inputs are known */                                 \
    cty mask = d1->mask | d2->mask;                                                        \
    result->value = value & (~mask);                                                       \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Bitwise NOT: flip all known bits, mask stays the same */                              \
  bennet_domain_tnum(cty) * bennet_domain_tnum_not_##cty(bennet_domain_tnum(cty) * d) {    \
    if (d->bottom) {                                                                       \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* Known 0 in input = ~value & ~mask, after NOT these become known 1 */                \
    cty k0 = fm & ((~d->value) & (~d->mask));                                              \
    cty value = k0;                                                                        \
    cty mask = d->mask;                                                                    \
    result->value = value & (~mask);                                                       \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Left shift: shift value and mask left, clear low bits */                              \
  bennet_domain_tnum(cty) * bennet_domain_tnum_shl_##cty(bennet_domain_tnum(cty) * d,      \
                                bennet_domain_tnum(cty) * shift_amt) {                     \
    if (d->bottom || shift_amt->bottom) {                                                  \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Non-constant shift: return top */                                                   \
    if (shift_amt->mask != 0) {                                                            \
      return bennet_domain_tnum_top_##cty();                                               \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    int width = sizeof(cty) * 8;                                                           \
    int shift = (int)shift_amt->value;                                                     \
    if (shift < 0 || shift >= width) {                                                     \
      result->top = true;                                                                  \
      result->bottom = false;                                                              \
      result->value = 0;                                                                   \
      result->mask = fm;                                                                   \
      return result;                                                                       \
    }                                                                                      \
    cty value = (d->value << shift) & fm;                                                  \
    cty mask = (d->mask << shift) & fm;                                                    \
    result->value = value & (~mask);                                                       \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Logical right shift: shift value and mask right, clear high bits */                   \
  bennet_domain_tnum(cty) * bennet_domain_tnum_lshr_##cty(bennet_domain_tnum(cty) * d,     \
                                bennet_domain_tnum(cty) * shift_amt) {                     \
    if (d->bottom || shift_amt->bottom) {                                                  \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Non-constant shift: return top */                                                   \
    if (shift_amt->mask != 0) {                                                            \
      return bennet_domain_tnum_top_##cty();                                               \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    int width = sizeof(cty) * 8;                                                           \
    int shift = (int)shift_amt->value;                                                     \
    if (shift < 0 || shift >= width) {                                                     \
      result->top = true;                                                                  \
      result->bottom = false;                                                              \
      result->value = 0;                                                                   \
      result->mask = fm;                                                                   \
      return result;                                                                       \
    }                                                                                      \
    /* Cast to unsigned type for logical shift */                                          \
    cty value = ((cty)d->value) >> shift;                                                  \
    cty mask = ((cty)d->mask) >> shift;                                                    \
    result->value = value & (~mask);                                                       \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Addition: Linux kernel tnum_add algorithm                                           \
     sv = sum of known values                                                            \
     sm = sum of masks                                                                   \
     sigma = sv + sm (carry propagation)                                                 \
     chi = sigma XOR sv (identifies changed bits)                                        \
     Output mask = chi | masks (any bit that could change is unknown) */ \
  bennet_domain_tnum(cty) * bennet_domain_tnum_add_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* Sum of known values */                                                              \
    cty sv = (d1->value + d2->value) & fm;                                                 \
    /* Sum of masks */                                                                     \
    cty sm = (d1->mask + d2->mask) & fm;                                                   \
    /* sigma = sv + sm (propagates carry through unknown bits) */                          \
    cty sigma = (sv + sm) & fm;                                                            \
    /* chi = sigma XOR sv (identifies changed bits) */                                     \
    cty chi = sigma ^ sv;                                                                  \
    /* Output mask: any bit that could change is unknown */                                \
    cty mask = (chi | d1->mask | d2->mask) & fm;                                           \
    /* Output value: known bits that don't change */                                       \
    cty value = sv & (~mask);                                                              \
    result->value = value;                                                                 \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Subtraction: kernel tnum_sub. Negate-then-add is unsound here because   \
   * negating a tnum bitwise ignores borrows rippling through unknown bits.  \
   * alpha/beta are the extremes of the borrow range. */             \
  bennet_domain_tnum(cty) * bennet_domain_tnum_sub_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    cty fm = (cty)(FULL_MASK);                                                             \
    cty dv = (d1->value - d2->value) & fm;                                                 \
    cty alpha = (dv + d1->mask) & fm;                                                      \
    cty beta = (dv - d2->mask) & fm;                                                       \
    cty chi = alpha ^ beta;                                                                \
    cty mask = (chi | d1->mask | d2->mask) & fm;                                           \
    cty value = dv & (~mask);                                                              \
    result->value = value;                                                                 \
    result->mask = mask;                                                                   \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
  }                                                                                        \
                                                                                           \
  /* Multiplication: conservative - precise for constants, top otherwise */                \
  bennet_domain_tnum(cty) * bennet_domain_tnum_mul_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* Both constants */                                                                   \
    if (d1->mask == 0 && d2->mask == 0) {                                                  \
      return bennet_domain_tnum_of_##cty((d1->value * d2->value) & fm, 0);                 \
    }                                                                                      \
    /* Either is zero */                                                                   \
    if (d1->mask == 0 && d1->value == 0) {                                                 \
      return bennet_domain_tnum_of_##cty(0, 0);                                            \
    }                                                                                      \
    if (d2->mask == 0 && d2->value == 0) {                                                 \
      return bennet_domain_tnum_of_##cty(0, 0);                                            \
    }                                                                                      \
    /* Conservative: return top */                                                         \
    return bennet_domain_tnum_top_##cty();                                                 \
  }                                                                                        \
                                                                                           \
  /* Division: precise for constants, bottom for div by zero, top otherwise */             \
  bennet_domain_tnum(cty) * bennet_domain_tnum_div_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Division by zero */                                                                 \
    if (d2->mask == 0 && d2->value == 0) {                                                 \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Both constants */                                                                   \
    if (d1->mask == 0 && d2->mask == 0) {                                                  \
      return bennet_domain_tnum_of_##cty(d1->value / d2->value, 0);                        \
    }                                                                                      \
    /* Conservative: return top */                                                         \
    return bennet_domain_tnum_top_##cty();                                                 \
  }                                                                                        \
                                                                                           \
  /* Modulo: precise for constants, bottom for mod by zero, top otherwise */               \
  bennet_domain_tnum(cty) * bennet_domain_tnum_mod_##cty(bennet_domain_tnum(cty) * d1,     \
                                bennet_domain_tnum(cty) * d2) {                            \
    if (d1->bottom || d2->bottom) {                                                        \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Modulo by zero */                                                                   \
    if (d2->mask == 0 && d2->value == 0) {                                                 \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Both constants */                                                                   \
    if (d1->mask == 0 && d2->mask == 0) {                                                  \
      return bennet_domain_tnum_of_##cty(d1->value % d2->value, 0);                        \
    }                                                                                      \
    /* Conservative: return top */                                                         \
    return bennet_domain_tnum_top_##cty();                                                 \
  }                                                                                        \
                                                                                           \
  bool bennet_domain_tnum_to_interval_##cty(                                               \
      bennet_domain_tnum(cty) * d, cty * lo_out, cty * hi_out) {                           \
    if (d->top || d->bottom) {                                                             \
      return false;                                                                        \
    }                                                                                      \
    *lo_out = d->value;                                                                    \
    *hi_out = (cty)(d->value | d->mask);                                                   \
    return true;                                                                           \
  }                                                                                        \
                                                                                           \
  /* of_interval: create tnum from interval [lo, hi]                                     \
     Find common prefix bits, mask the rest as unknown */ \
  bennet_domain_tnum(cty) * bennet_domain_tnum_of_interval_##cty(cty lo, cty hi) {         \
    cty fm = (cty)(FULL_MASK);                                                             \
    /* Empty interval */                                                                   \
    if (lo > hi) {                                                                         \
      return bennet_domain_tnum_bottom_##cty();                                            \
    }                                                                                      \
    /* Single value */                                                                     \
    if (lo == hi) {                                                                        \
      return bennet_domain_tnum_of_##cty(lo & fm, 0);                                      \
    }                                                                                      \
    bennet_domain_tnum(cty)* result = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(result);                                                                        \
    /* XOR gives us bits that differ between lo and hi */                                  \
    cty diff = lo ^ hi;                                                                    \
    /* Find the highest differing bit position */                                          \
    int highest = 0;                                                                       \
    cty tmp = diff;                                                                        \
    while (tmp != 0) {                                                                     \
      tmp >>= 1;                                                                           \
      highest++;                                                                           \
    }                                                                                      \
    /* Mask from highest differing bit down */                                             \
    cty uncertain_mask =                                                                   \
        (highest >= (int)(sizeof(cty) * 8)) ? fm : (((cty)1 << highest) - 1);              \
    /* Value is the common prefix (bits above the highest differing bit) */                \
    cty value = lo & (~uncertain_mask);                                                    \
    /* Ensure well-formedness */                                                           \
    result->value = value & (~uncertain_mask);                                             \
    result->mask = uncertain_mask & fm;                                                    \
    result->top = (result->value == 0 && result->mask == fm);                              \
    result->bottom = false;                                                                \
    return result;                                                                         \
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

/*=============================================================================
 * Tagged Domain Transformer Infrastructure for Tnum
 *===========================================================================*/

#include <bennet/internals/domain.h>
#include <cn-smt/terms.h>

/*-----------------------------------------------------------------------------
 * tnum_generic: type-erased tnum for transformer dispatch
 *---------------------------------------------------------------------------*/

typedef struct {
  bool is_top;
  bool is_bottom;
  bool is_signed;
  int width;
  uint64_t value; /* Known 1 bits */
  uint64_t mask;  /* Unknown bits */
} tnum_generic;

/**
 * Full mask for a given width.
 */
static inline uint64_t tnum_full_mask(int width) {
  if (width >= 64)
    return UINT64_MAX;
  return ((uint64_t)1 << width) - 1;
}

/**
 * Create a generic tnum representing top.
 */
static tnum_generic tnum_generic_top(int width, bool is_signed) {
  return (tnum_generic){
      .is_top = true,
      .is_bottom = false,
      .is_signed = is_signed,
      .width = width,
      .value = 0,
      .mask = tnum_full_mask(width),
  };
}

/**
 * Create a generic tnum representing bottom.
 */
static tnum_generic tnum_generic_bottom(int width, bool is_signed) {
  return (tnum_generic){
      .is_top = false,
      .is_bottom = true,
      .is_signed = is_signed,
      .width = width,
      .value = 0,
      .mask = 0,
  };
}

/**
 * Create a singleton tnum (constant).
 */
static tnum_generic tnum_generic_const(int width, bool is_signed, uint64_t val) {
  uint64_t fm = tnum_full_mask(width);
  val &= fm;
  return (tnum_generic){
      .is_top = false,
      .is_bottom = false,
      .is_signed = is_signed,
      .width = width,
      .value = val,
      .mask = 0,
  };
}

/**
 * Create a tnum from an interval [lo, hi].
 */
/* Known-bits abstraction of an unsigned-contiguous pattern range [lo, hi]:
 * the common bit prefix is known, everything below the highest differing bit
 * is unknown. Requires lo <= hi as raw width-masked patterns; signed ranges
 * that straddle zero are NOT pattern-contiguous and must go through
 * tnum_generic_of_interval. */
static tnum_generic tnum_of_pattern_range(
    int width, bool is_signed, uint64_t lo, uint64_t hi) {
  uint64_t fm = tnum_full_mask(width);
  lo &= fm;
  hi &= fm;
  if (lo > hi)
    return tnum_generic_bottom(width, is_signed);
  if (lo == hi)
    return tnum_generic_const(width, is_signed, lo);
  uint64_t diff = lo ^ hi;
  int highest = 0;
  uint64_t tmp = diff;
  while (tmp != 0) {
    tmp >>= 1;
    highest++;
  }
  uint64_t uncertain_mask = (highest >= 64) ? fm : (((uint64_t)1 << highest) - 1);
  uint64_t value = lo & (~uncertain_mask) & fm;
  uint64_t mask = uncertain_mask & fm;
  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = is_signed,
      .width = width,
      .value = value & (~mask),
      .mask = mask,
  };
}

/**
 * Sign-extend a value from a given width to int64_t.
 */
static int64_t tnum_to_signed_value(int width, uint64_t value) {
  if (width >= 64)
    return (int64_t)value;
  uint64_t sign_bit = (uint64_t)1 << (width - 1);
  if (value & sign_bit)
    return (int64_t)(value | ~tnum_full_mask(width));
  return (int64_t)value;
}

/**
 * Get the minimum and maximum representable values for a given width and signedness.
 */
static void tnum_get_extrema(
    int width, bool is_signed, int64_t* min_out, int64_t* max_out) {
  if (is_signed) {
    if (width >= 64) {
      *min_out = INT64_MIN;
      *max_out = INT64_MAX;
    } else {
      *min_out = -((int64_t)1 << (width - 1));
      *max_out = ((int64_t)1 << (width - 1)) - 1;
    }
  } else {
    *min_out = 0;
    if (width >= 64)
      *max_out = (int64_t)UINT64_MAX;
    else
      *max_out = (int64_t)(((uint64_t)1 << width) - 1);
  }
}

/**
 * Convert between tagged domains and the generic form. Loads zero-extend the
 * raw bit patterns (never sign-extend); stores mask to the type's width
 * before narrowing (identity at width 64).
 */
#define TNUM_TAGGED_LOAD(cty, ucty)                                                      \
  do {                                                                                   \
    bennet_domain_tnum(cty)* dom_ = (bennet_domain_tnum(cty)*)d->domain;                 \
    result.is_top = dom_->top;                                                           \
    result.is_bottom = dom_->bottom;                                                     \
    result.value = (uint64_t)(ucty)dom_->value;                                          \
    result.mask = (uint64_t)(ucty)dom_->mask;                                            \
  } while (0)

#define TNUM_TAGGED_STORE(cty, ucty)                                                     \
  do {                                                                                   \
    bennet_domain_tnum(cty)* dom_ = std_malloc(sizeof(bennet_domain_tnum(cty)));         \
    assert(dom_);                                                                        \
    dom_->top = g->is_top;                                                               \
    dom_->bottom = g->is_bottom;                                                         \
    dom_->value = (cty)(g->value & tnum_full_mask(width));                               \
    dom_->mask = (cty)(g->mask & tnum_full_mask(width));                                 \
    result.domain = dom_;                                                                \
  } while (0)

BENNET_ABSINT_TAGGED_CONVERT_IMPL(tnum, tnum_generic, TNUM_TAGGED_LOAD, TNUM_TAGGED_STORE)

/*-----------------------------------------------------------------------------
 * Generic tnum operations (type-erased, width-parametric)
 *---------------------------------------------------------------------------*/

static tnum_generic tnum_generic_add(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  uint64_t sv = (a->value + b->value) & fm;
  uint64_t sm = (a->mask + b->mask) & fm;
  uint64_t sigma = (sv + sm) & fm;
  uint64_t chi = sigma ^ sv;
  uint64_t mask = (chi | a->mask | b->mask) & fm;
  uint64_t value = sv & (~mask) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_sub(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  /* Kernel tnum_sub: alpha/beta are the extremes of the borrow range, so
   * chi = alpha ^ beta flags every bit a borrow can reach. */
  uint64_t fm = tnum_full_mask(a->width);
  uint64_t dv = (a->value - b->value) & fm;
  uint64_t alpha = (dv + a->mask) & fm;
  uint64_t beta = (dv - b->mask) & fm;
  uint64_t chi = alpha ^ beta;
  uint64_t mask = (chi | a->mask | b->mask) & fm;
  uint64_t value = dv & (~mask) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_and(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  /* Known 1s: both definitely 1 */
  uint64_t value = (a->value & b->value) & fm;
  /* Unknown: where either input is unknown AND result could be 1 */
  uint64_t mask =
      ((a->mask & (b->value | b->mask)) | (b->mask & (a->value | a->mask))) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_or(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  /* Known 1s: either definitely 1 */
  uint64_t value = (a->value | b->value) & fm;
  /* Unknown: where either input is unknown AND result could be 0 */
  uint64_t mask = ((a->mask & ~b->value) | (b->mask & ~a->value)) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_xor(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  /* XOR of known bits */
  uint64_t value = (a->value ^ b->value) & fm;
  /* Unknown where either is unknown */
  uint64_t mask = (a->mask | b->mask) & fm;
  value &= ~mask;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_not(tnum_generic* a) {
  if (a->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  /* NOT flips known bits, mask stays the same */
  uint64_t value = (~a->value & ~a->mask) & fm;
  uint64_t mask = a->mask & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_shl(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  /* Shift amount must be a constant for precision */
  if (b->mask != 0)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  uint64_t shift = b->value;
  if (shift >= (uint64_t)a->width)
    return tnum_generic_const(a->width, a->is_signed, 0);

  uint64_t value = (a->value << shift) & fm;
  uint64_t mask = (a->mask << shift) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_lshr(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  /* Shift amount must be a constant for precision */
  if (b->mask != 0)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t shift = b->value;
  if (shift >= (uint64_t)a->width)
    return tnum_generic_const(a->width, a->is_signed, 0);

  uint64_t value = a->value >> shift;
  uint64_t mask = a->mask >> shift;

  return (tnum_generic){
      .is_top = (value == 0 && mask == tnum_full_mask(a->width)),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

static tnum_generic tnum_generic_mul(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  /* If both are constants, result is constant */
  if (a->mask == 0 && b->mask == 0) {
    uint64_t fm = tnum_full_mask(a->width);
    uint64_t value = (a->value * b->value) & fm;
    return tnum_generic_const(a->width, a->is_signed, value);
  }

  /* Multiplication by 0 */
  if (a->mask == 0 && a->value == 0)
    return tnum_generic_const(a->width, a->is_signed, 0);
  if (b->mask == 0 && b->value == 0)
    return tnum_generic_const(a->width, a->is_signed, 0);

  /* Conservative: return top for non-constant multiplication */
  return tnum_generic_top(a->width, a->is_signed);
}

static tnum_generic tnum_generic_div(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  /* Division by zero */
  if (b->mask == 0 && b->value == 0)
    return tnum_generic_bottom(a->width, a->is_signed);

  /* Both constants */
  if (a->mask == 0 && b->mask == 0) {
    uint64_t fm = tnum_full_mask(a->width);
    uint64_t value = (a->value / b->value) & fm;
    return tnum_generic_const(a->width, a->is_signed, value);
  }

  return tnum_generic_top(a->width, a->is_signed);
}

static tnum_generic tnum_generic_mod(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  /* Modulo by zero */
  if (b->mask == 0 && b->value == 0)
    return tnum_generic_bottom(a->width, a->is_signed);

  /* Both constants */
  if (a->mask == 0 && b->mask == 0) {
    uint64_t fm = tnum_full_mask(a->width);
    uint64_t value = (a->value % b->value) & fm;
    return tnum_generic_const(a->width, a->is_signed, value);
  }

  return tnum_generic_top(a->width, a->is_signed);
}

static tnum_generic tnum_generic_join(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom)
    return *b;
  if (b->is_bottom)
    return *a;
  if (a->is_top || b->is_top)
    return tnum_generic_top(a->width, a->is_signed);

  uint64_t fm = tnum_full_mask(a->width);
  /* Bits that differ between the two known parts */
  uint64_t diff = (a->value ^ b->value) & fm;
  uint64_t mask = (a->mask | b->mask | diff) & fm;
  uint64_t value = (a->value & b->value & ~mask) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

/* Known-bits abstraction of the value interval [lo, hi], interpreted per
 * signedness. A signed interval straddling zero (lo < 0 <= hi) is not
 * contiguous in raw patterns ([lo..fm] then [0..hi]), so it is the join of
 * its sign-homogeneous halves; comparing the raw patterns unsigned instead
 * used to bottom every such range (assume x:i8 <= 1 claimed unsat). */
static tnum_generic tnum_generic_of_interval(
    int width, bool is_signed, uint64_t lo, uint64_t hi) {
  uint64_t fm = tnum_full_mask(width);
  lo &= fm;
  hi &= fm;
  if (is_signed) {
    int64_t slo = tnum_to_signed_value(width, lo);
    int64_t shi = tnum_to_signed_value(width, hi);
    if (slo > shi)
      return tnum_generic_bottom(width, is_signed);
    if (slo < 0 && shi >= 0) {
      tnum_generic neg = tnum_of_pattern_range(width, is_signed, lo, fm);
      tnum_generic nonneg = tnum_of_pattern_range(width, is_signed, 0, hi);
      return tnum_generic_join(&neg, &nonneg);
    }
  }
  return tnum_of_pattern_range(width, is_signed, lo, hi);
}

static tnum_generic tnum_generic_meet(tnum_generic* a, tnum_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return tnum_generic_bottom(a->width, a->is_signed);
  if (a->is_top)
    return *b;
  if (b->is_top)
    return *a;

  uint64_t fm = tnum_full_mask(a->width);
  /* Check for conflict: both known but different */
  uint64_t both_known = ~(a->mask | b->mask) & fm;
  if ((a->value & both_known) != (b->value & both_known))
    return tnum_generic_bottom(a->width, a->is_signed);

  uint64_t mask = (a->mask & b->mask) & fm;
  uint64_t value = ((a->value | b->value) & ~mask) & fm;

  return (tnum_generic){
      .is_top = (value == 0 && mask == fm),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .value = value,
      .mask = mask,
  };
}

/*-----------------------------------------------------------------------------
 * Tagged Domain Functions (tnum-specific)
 *---------------------------------------------------------------------------*/

bool bennet_tagged_domain_is_bottom_tnum(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return false;
  tnum_generic g = tnum_from_tagged(d);
  return g.is_bottom;
}

bool bennet_tagged_domain_is_top_tnum(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return true;
  tnum_generic g = tnum_from_tagged(d);
  return g.is_top;
}

bennet_tagged_domain bennet_tagged_domain_copy_tnum(bennet_tagged_domain* d) {
  if (!d || !d->domain) {
    return bennet_tagged_domain_top_tnum(d ? d->type : NULL);
  }
  tnum_generic g = tnum_from_tagged(d);
  return tnum_to_tagged(&g, d->type);
}

bennet_tagged_domain bennet_tagged_domain_top_tnum(cn_base_type* type) {
  int width = 64;
  bool is_signed = false;
  if (type)
    bennet_absint_type_info(type, &width, &is_signed);
  tnum_generic g = tnum_generic_top(width, is_signed);
  return tnum_to_tagged(&g, type);
}

bennet_tagged_domain bennet_tagged_domain_bottom_tnum(cn_base_type* type) {
  int width = 64;
  bool is_signed = false;
  if (type)
    bennet_absint_type_info(type, &width, &is_signed);
  tnum_generic g = tnum_generic_bottom(width, is_signed);
  return tnum_to_tagged(&g, type);
}

bennet_tagged_domain bennet_tagged_domain_meet_tnum(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2 && d1->type && d2->type);
  tnum_generic g1 = tnum_from_tagged(d1);
  tnum_generic g2 = tnum_from_tagged(d2);
  tnum_generic result = tnum_generic_meet(&g1, &g2);
  return tnum_to_tagged(&result, d1->type);
}

bennet_tagged_domain bennet_tagged_domain_join_tnum(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2 && d1->type && d2->type);
  tnum_generic g1 = tnum_from_tagged(d1);
  tnum_generic g2 = tnum_from_tagged(d2);
  tnum_generic result = tnum_generic_join(&g1, &g2);
  return tnum_to_tagged(&result, d1->type);
}

/*-----------------------------------------------------------------------------
 * Abstract State Implementation (tnum)
 *---------------------------------------------------------------------------*/

BENNET_ABSINT_STATE_IMPL(tnum)

/*-----------------------------------------------------------------------------
 * Forward Transformer Implementation (Tnum)
 *---------------------------------------------------------------------------*/

static bennet_tagged_domain tnum_forward_const(cn_term* term) {
  assert(term && term->type == CN_TERM_CONST);

  int width;
  bool is_signed;
  bennet_absint_type_info(&term->base_type, &width, &is_signed);

  tnum_generic g;
  cn_const* c = &term->data.const_val;
  switch (c->type) {
    case CN_CONST_BITS:
      g = tnum_generic_const(width, is_signed, (uint64_t)c->data.bits.value);
      break;
    case CN_CONST_Z:
      g = tnum_generic_const(width, is_signed, (uint64_t)c->data.z);
      break;
    case CN_CONST_BOOL:
      g = tnum_generic_const(1, false, c->data.boolean ? 1 : 0);
      break;
    case CN_CONST_POINTER:
      g = tnum_generic_const(width, is_signed, (uint64_t)c->data.pointer);
      break;
    case CN_CONST_NULL:
      g = tnum_generic_const(width, is_signed, 0);
      break;
    default:
      g = tnum_generic_top(width, is_signed);
      break;
  }

  return tnum_to_tagged(&g, &term->base_type);
}

static bennet_tagged_domain tnum_forward_sym(cn_term* term, bennet_absint_state* state) {
  assert(term && term->type == CN_TERM_SYM);
  bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
  return bennet_absint_state_get_tnum(state, sym, &term->base_type);
}

static bennet_tagged_domain tnum_forward_binop(cn_binop op,
    bennet_tagged_domain* left,
    bennet_tagged_domain* right,
    cn_base_type* result_type) {
  tnum_generic g1 = tnum_from_tagged(left);
  tnum_generic g2 = tnum_from_tagged(right);
  tnum_generic result;

  int width;
  bool is_signed;
  bennet_absint_type_info(result_type, &width, &is_signed);

  /* Adjust widths to result type */
  g1.width = width;
  g1.is_signed = is_signed;
  g2.width = width;
  g2.is_signed = is_signed;

  if (g1.is_bottom || g2.is_bottom) {
    result = tnum_generic_bottom(width, is_signed);
    return tnum_to_tagged(&result, result_type);
  }

  if (g1.is_top || g2.is_top) {
    switch (op) {
      case CN_BINOP_LT:
      case CN_BINOP_LE:
      case CN_BINOP_EQ:
      case CN_BINOP_LT_POINTER:
      case CN_BINOP_LE_POINTER:
        /* Boolean result: could be true or false */
        result = tnum_generic_top(1, false);
        return tnum_to_tagged(&result, result_type);
      default:
        result = tnum_generic_top(width, is_signed);
        return tnum_to_tagged(&result, result_type);
    }
  }

  switch (op) {
    case CN_BINOP_ADD:
      result = tnum_generic_add(&g1, &g2);
      break;
    case CN_BINOP_SUB:
      result = tnum_generic_sub(&g1, &g2);
      break;
    case CN_BINOP_MUL:
    case CN_BINOP_MULNOSMT:
      result = tnum_generic_mul(&g1, &g2);
      break;
    case CN_BINOP_DIV:
    case CN_BINOP_DIVNOSMT:
      result = tnum_generic_div(&g1, &g2);
      break;
    case CN_BINOP_MOD:
    case CN_BINOP_MODNOSMT:
    case CN_BINOP_REM:
    case CN_BINOP_REMNOSMT:
      result = tnum_generic_mod(&g1, &g2);
      break;
    case CN_BINOP_BW_AND:
      result = tnum_generic_and(&g1, &g2);
      break;
    case CN_BINOP_BW_OR:
      result = tnum_generic_or(&g1, &g2);
      break;
    case CN_BINOP_BW_XOR:
      result = tnum_generic_xor(&g1, &g2);
      break;
    case CN_BINOP_SHIFT_LEFT:
      result = tnum_generic_shl(&g1, &g2);
      break;
    case CN_BINOP_SHIFT_RIGHT:
      result = tnum_generic_lshr(&g1, &g2);
      break;
    case CN_BINOP_LT:
    case CN_BINOP_LT_POINTER:
    case CN_BINOP_LE:
    case CN_BINOP_LE_POINTER:
    case CN_BINOP_EQ:
      /* Boolean result: conservative - return top(1-bit) */
      result = tnum_generic_top(1, false);
      break;
    default:
      result = tnum_generic_top(width, is_signed);
      break;
  }

  return tnum_to_tagged(&result, result_type);
}

static bennet_tagged_domain tnum_forward_unop(
    cn_unop op, bennet_tagged_domain* operand, cn_base_type* result_type) {
  tnum_generic g = tnum_from_tagged(operand);
  tnum_generic result;

  int width;
  bool is_signed;
  bennet_absint_type_info(result_type, &width, &is_signed);
  g.width = width;
  g.is_signed = is_signed;

  if (g.is_bottom) {
    result = tnum_generic_bottom(width, is_signed);
    return tnum_to_tagged(&result, result_type);
  }

  if (g.is_top) {
    result = tnum_generic_top(width, is_signed);
    return tnum_to_tagged(&result, result_type);
  }

  switch (op) {
    case CN_UNOP_NOT: {
      /* Logical NOT for boolean */
      if (g.mask == 0) {
        if (g.value == 0)
          result = tnum_generic_const(1, false, 1);
        else
          result = tnum_generic_const(1, false, 0);
      } else {
        result = tnum_generic_top(1, false);
      }
      break;
    }
    case CN_UNOP_NEGATE: {
      /* -x = 0 - x */
      tnum_generic zero = tnum_generic_const(width, is_signed, 0);
      result = tnum_generic_sub(&zero, &g);
      break;
    }
    case CN_UNOP_BW_COMPL:
      result = tnum_generic_not(&g);
      break;
    default:
      result = tnum_generic_top(width, is_signed);
      break;
  }

  return tnum_to_tagged(&result, result_type);
}

bennet_tagged_domain bennet_tnum_transform_forward(
    cn_term* term, bennet_absint_state* state) {
  if (!term) {
    cn_base_type bt = cn_base_type_bits(false, 64);
    return bennet_tagged_domain_top_tnum(&bt);
  }

  switch (term->type) {
    case CN_TERM_CONST:
      return tnum_forward_const(term);

    case CN_TERM_SYM:
      return tnum_forward_sym(term, state);

    case CN_TERM_UNOP: {
      bennet_tagged_domain operand_dom =
          bennet_tnum_transform_forward(term->data.unop.operand, state);
      return tnum_forward_unop(term->data.unop.op, &operand_dom, &term->base_type);
    }

    case CN_TERM_BINOP: {
      bennet_tagged_domain left_dom =
          bennet_tnum_transform_forward(term->data.binop.left, state);
      bennet_tagged_domain right_dom =
          bennet_tnum_transform_forward(term->data.binop.right, state);
      return tnum_forward_binop(
          term->data.binop.op, &left_dom, &right_dom, &term->base_type);
    }

    case CN_TERM_ITE: {
      bennet_tagged_domain then_dom =
          bennet_tnum_transform_forward(term->data.ite.then_term, state);
      bennet_tagged_domain else_dom =
          bennet_tnum_transform_forward(term->data.ite.else_term, state);
      /* Use tnum join instead of tagged domain join for tnum precision */
      tnum_generic tg = tnum_from_tagged(&then_dom);
      tnum_generic eg = tnum_from_tagged(&else_dom);
      tnum_generic joined = tnum_generic_join(&tg, &eg);
      return tnum_to_tagged(&joined, &term->base_type);
    }

    case CN_TERM_CAST: {
      bennet_tagged_domain src_dom =
          bennet_tnum_transform_forward(term->data.cast.value, state);

      int src_width, dst_width;
      bool src_signed, dst_signed;
      bennet_absint_type_info(src_dom.type, &src_width, &src_signed);
      bennet_absint_type_info(&term->base_type, &dst_width, &dst_signed);

      tnum_generic src = tnum_from_tagged(&src_dom);

      if (src.is_bottom) {
        tnum_generic bot = tnum_generic_bottom(dst_width, dst_signed);
        return tnum_to_tagged(&bot, &term->base_type);
      }

      if (src.is_top) {
        tnum_generic top = tnum_generic_top(dst_width, dst_signed);
        return tnum_to_tagged(&top, &term->base_type);
      }

      if (src_width == dst_width) {
        /* Same width: just change type metadata */
        src.is_signed = dst_signed;
        src.width = dst_width;
        return tnum_to_tagged(&src, &term->base_type);
      } else if (src_width > dst_width) {
        /* Truncation: mask to lower bits */
        uint64_t fm = tnum_full_mask(dst_width);
        tnum_generic result = {
            .is_top = false,
            .is_bottom = false,
            .is_signed = dst_signed,
            .width = dst_width,
            .value = src.value & fm,
            .mask = src.mask & fm,
        };
        result.is_top = (result.value == 0 && result.mask == fm);
        return tnum_to_tagged(&result, &term->base_type);
      } else {
        /* Extension: upper bits become 0 (zero-ext) or preserve sign (sign-ext) */
        uint64_t dst_fm = tnum_full_mask(dst_width);
        uint64_t src_fm = tnum_full_mask(src_width);
        tnum_generic result = {
            .is_top = false,
            .is_bottom = false,
            .is_signed = dst_signed,
            .width = dst_width,
            .value = src.value & src_fm,
            .mask = src.mask & src_fm,
        };
        if (src_signed && src_width > 0) {
          /* Sign extension: if sign bit is unknown, upper bits are unknown */
          uint64_t sign_bit = (uint64_t)1 << (src_width - 1);
          uint64_t upper = dst_fm & ~src_fm;
          if (src.mask & sign_bit) {
            /* Sign bit is unknown, upper bits are unknown */
            result.mask |= upper;
          } else if (src.value & sign_bit) {
            /* Sign bit is known 1, upper bits are 1 */
            result.value |= upper;
          }
          /* If sign bit is known 0, upper bits stay 0 */
        }
        result.is_top = (result.value == 0 && result.mask == dst_fm);
        return tnum_to_tagged(&result, &term->base_type);
      }
    }

    case CN_TERM_ARRAY_SHIFT: {
      bennet_tagged_domain base_dom =
          bennet_tnum_transform_forward(term->data.array_shift.base, state);
      bennet_tagged_domain index_dom =
          bennet_tnum_transform_forward(term->data.array_shift.index, state);

      /* Create constant tnum for element_size */
      int idx_width;
      bool idx_signed;
      bennet_absint_type_info(index_dom.type, &idx_width, &idx_signed);
      tnum_generic elem_size_g =
          tnum_generic_const(idx_width, idx_signed, term->data.array_shift.element_size);
      bennet_tagged_domain elem_size_dom = tnum_to_tagged(&elem_size_g, index_dom.type);

      /* index * element_size */
      bennet_tagged_domain offset_dom =
          tnum_forward_binop(CN_BINOP_MUL, &index_dom, &elem_size_dom, index_dom.type);

      /* base + offset */
      return tnum_forward_binop(CN_BINOP_ADD, &base_dom, &offset_dom, &term->base_type);
    }

    case CN_TERM_MEMBER_SHIFT: {
      bennet_tagged_domain base_dom =
          bennet_tnum_transform_forward(term->data.member_shift.base, state);

      /* Create constant tnum for offset */
      int base_width;
      bool base_signed;
      bennet_absint_type_info(base_dom.type, &base_width, &base_signed);
      tnum_generic offset_g =
          tnum_generic_const(base_width, base_signed, term->data.member_shift.offset);
      bennet_tagged_domain offset_dom = tnum_to_tagged(&offset_g, base_dom.type);

      /* base + offset */
      return tnum_forward_binop(CN_BINOP_ADD, &base_dom, &offset_dom, &term->base_type);
    }

    default:
      return bennet_tagged_domain_top_tnum(&term->base_type);
  }
}

/*-----------------------------------------------------------------------------
 * Backward Transformer Implementation (Tnum)
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_tnum_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state) {
  if (!term || !state)
    return state;

  bennet_absint_sym sym = {.name = target_sym.name, .id = target_sym.id};

  if (bennet_tagged_domain_is_bottom_tnum(&output_domain)) {
    bennet_absint_state* result = bennet_absint_state_copy_tnum(state);
    result = bennet_absint_state_set_tnum(
        result, sym, bennet_tagged_domain_bottom_tnum(&term->base_type));
    return result;
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      if (term->data.sym.id == target_sym.id) {
        return bennet_absint_state_meet_tnum(state, sym, output_domain);
      }
      return bennet_absint_state_copy_tnum(state);
    }

    case CN_TERM_BINOP: {
      cn_term* left = term->data.binop.left;
      cn_term* right = term->data.binop.right;
      bool left_has_target = term_contains_sym(left, target_sym.id);
      bool right_has_target = term_contains_sym(right, target_sym.id);

      if (!left_has_target && !right_has_target)
        return bennet_absint_state_copy_tnum(state);

      /* For comparisons, handled by backward_assume */
      switch (term->data.binop.op) {
        case CN_BINOP_EQ:
        case CN_BINOP_LT:
        case CN_BINOP_LE:
        case CN_BINOP_LT_POINTER:
        case CN_BINOP_LE_POINTER:
          return bennet_absint_state_copy_tnum(state);
        default:
          break;
      }

      /* For bitwise operations, apply backward refinement */
      cn_term* target_side = left_has_target ? left : right;
      cn_term* other_side = left_has_target ? right : left;

      switch (term->data.binop.op) {
        case CN_BINOP_BW_AND: {
          /* Backward AND: if result = target & other, refine target */
          bennet_tagged_domain other_dom =
              bennet_tnum_transform_forward(other_side, state);
          tnum_generic other_g = tnum_from_tagged(&other_dom);
          tnum_generic result_g = tnum_from_tagged(&output_domain);
          uint64_t fm = tnum_full_mask(result_g.width);

          if (!result_g.is_top && !result_g.is_bottom && !other_g.is_top &&
              !other_g.is_bottom) {
            if (other_g.mask == 0) {
              /* Other is constant mask */
              uint64_t mask_val = other_g.value;
              uint64_t new_value = result_g.value & mask_val;
              uint64_t new_mask = fm & (~mask_val | result_g.mask);
              tnum_generic refined = {
                  .is_top = false,
                  .is_bottom = false,
                  .is_signed = result_g.is_signed,
                  .width = result_g.width,
                  .value = new_value & ~new_mask,
                  .mask = new_mask,
              };
              bennet_tagged_domain target_dom =
                  bennet_tnum_transform_forward(target_side, state);
              tnum_generic orig_target = tnum_from_tagged(&target_dom);
              tnum_generic met = tnum_generic_meet(&orig_target, &refined);
              bennet_tagged_domain met_td = tnum_to_tagged(&met, &target_side->base_type);
              return bennet_tnum_transform_backward(
                  target_side, target_sym, met_td, state);
            }
          }
          return bennet_tnum_transform_backward(
              target_side, target_sym, output_domain, state);
        }

        case CN_BINOP_BW_OR: {
          /* Backward OR: R = target | other */
          bennet_tagged_domain other_dom =
              bennet_tnum_transform_forward(other_side, state);
          tnum_generic other_g = tnum_from_tagged(&other_dom);
          tnum_generic result_g = tnum_from_tagged(&output_domain);
          uint64_t fm = tnum_full_mask(result_g.width);

          if (!result_g.is_top && !result_g.is_bottom && !other_g.is_top &&
              !other_g.is_bottom) {
            uint64_t result_k0 = fm & (~result_g.value & ~result_g.mask);
            uint64_t result_k1 = fm & (result_g.value & ~result_g.mask);
            uint64_t other_k0 = fm & (~other_g.value & ~other_g.mask);
            uint64_t forced_0 = result_k0;
            uint64_t forced_1 = result_k1 & other_k0;

            bennet_tagged_domain target_dom =
                bennet_tnum_transform_forward(target_side, state);
            tnum_generic target_g = tnum_from_tagged(&target_dom);

            uint64_t new_value = (target_g.value | forced_1) & ~forced_0;
            uint64_t new_mask = target_g.mask & ~(forced_0 | forced_1);
            tnum_generic refined = {
                .is_top = false,
                .is_bottom = false,
                .is_signed = target_g.is_signed,
                .width = target_g.width,
                .value = new_value & ~new_mask & fm,
                .mask = new_mask & fm,
            };
            tnum_generic met = tnum_generic_meet(&target_g, &refined);
            bennet_tagged_domain met_td = tnum_to_tagged(&met, &target_side->base_type);
            return bennet_tnum_transform_backward(target_side, target_sym, met_td, state);
          }
          return bennet_tnum_transform_backward(
              target_side, target_sym, output_domain, state);
        }

        case CN_BINOP_BW_XOR: {
          /* Backward XOR: R = target ^ other */
          bennet_tagged_domain other_dom =
              bennet_tnum_transform_forward(other_side, state);
          tnum_generic other_g = tnum_from_tagged(&other_dom);
          tnum_generic result_g = tnum_from_tagged(&output_domain);
          uint64_t fm = tnum_full_mask(result_g.width);

          if (!result_g.is_top && !result_g.is_bottom && !other_g.is_top &&
              !other_g.is_bottom) {
            uint64_t both_known = (~result_g.mask) & (~other_g.mask) & fm;
            uint64_t derived_value = (result_g.value ^ other_g.value) & both_known;

            bennet_tagged_domain target_dom =
                bennet_tnum_transform_forward(target_side, state);
            tnum_generic target_g = tnum_from_tagged(&target_dom);
            uint64_t new_mask = target_g.mask & ~both_known;
            uint64_t new_value =
                ((target_g.value & ~target_g.mask) | derived_value) & ~new_mask & fm;
            tnum_generic refined = {
                .is_top = false,
                .is_bottom = false,
                .is_signed = target_g.is_signed,
                .width = target_g.width,
                .value = new_value,
                .mask = new_mask & fm,
            };
            tnum_generic met = tnum_generic_meet(&target_g, &refined);
            bennet_tagged_domain met_td = tnum_to_tagged(&met, &target_side->base_type);
            return bennet_tnum_transform_backward(target_side, target_sym, met_td, state);
          }
          return bennet_tnum_transform_backward(
              target_side, target_sym, output_domain, state);
        }

        case CN_BINOP_SHIFT_LEFT: {
          /* Backward SHL: R = target << amount */
          bennet_tagged_domain other_dom =
              bennet_tnum_transform_forward(other_side, state);
          tnum_generic other_g = tnum_from_tagged(&other_dom);
          tnum_generic result_g = tnum_from_tagged(&output_domain);

          if (!result_g.is_top && !result_g.is_bottom && other_g.mask == 0) {
            uint64_t shift = other_g.value;
            if (shift < (uint64_t)result_g.width && left_has_target) {
              uint64_t derived_value = result_g.value >> shift;
              uint64_t derived_mask = result_g.mask >> shift;
              tnum_generic derived = {
                  .is_top = false,
                  .is_bottom = false,
                  .is_signed = result_g.is_signed,
                  .width = result_g.width,
                  .value = derived_value & ~derived_mask,
                  .mask = derived_mask,
              };
              bennet_tagged_domain target_dom =
                  bennet_tnum_transform_forward(target_side, state);
              tnum_generic target_g = tnum_from_tagged(&target_dom);
              tnum_generic met = tnum_generic_meet(&target_g, &derived);
              bennet_tagged_domain met_td = tnum_to_tagged(&met, &target_side->base_type);
              return bennet_tnum_transform_backward(
                  target_side, target_sym, met_td, state);
            }
          }
          return bennet_tnum_transform_backward(
              target_side, target_sym, output_domain, state);
        }

        case CN_BINOP_SHIFT_RIGHT: {
          /* Backward SHR: R = target >> amount */
          bennet_tagged_domain other_dom =
              bennet_tnum_transform_forward(other_side, state);
          tnum_generic other_g = tnum_from_tagged(&other_dom);
          tnum_generic result_g = tnum_from_tagged(&output_domain);
          uint64_t fm = tnum_full_mask(result_g.width);

          if (!result_g.is_top && !result_g.is_bottom && other_g.mask == 0) {
            uint64_t shift = other_g.value;
            if (shift < (uint64_t)result_g.width && left_has_target) {
              uint64_t derived_value = fm & (result_g.value << shift);
              uint64_t low_k = ((uint64_t)1 << shift) - 1;
              uint64_t derived_mask = fm & ((result_g.mask << shift) | low_k);
              tnum_generic derived = {
                  .is_top = false,
                  .is_bottom = false,
                  .is_signed = result_g.is_signed,
                  .width = result_g.width,
                  .value = derived_value & ~derived_mask & fm,
                  .mask = derived_mask,
              };
              bennet_tagged_domain target_dom =
                  bennet_tnum_transform_forward(target_side, state);
              tnum_generic target_g = tnum_from_tagged(&target_dom);
              tnum_generic met = tnum_generic_meet(&target_g, &derived);
              bennet_tagged_domain met_td = tnum_to_tagged(&met, &target_side->base_type);
              return bennet_tnum_transform_backward(
                  target_side, target_sym, met_td, state);
            }
          }
          return bennet_tnum_transform_backward(
              target_side, target_sym, output_domain, state);
        }

        default:
          /* For other operations (ADD, SUB, MUL, etc.), propagate unchanged */
          return bennet_tnum_transform_backward(
              target_side, target_sym, output_domain, state);
      }
    }

    case CN_TERM_UNOP: {
      if (term->data.unop.op == CN_UNOP_BW_COMPL) {
        /* Backward NOT: R = ~operand => operand bits are flipped result bits */
        tnum_generic result_g = tnum_from_tagged(&output_domain);
        uint64_t fm = tnum_full_mask(result_g.width);

        if (!result_g.is_top && !result_g.is_bottom) {
          uint64_t result_k0 = fm & (~result_g.value & ~result_g.mask);
          uint64_t result_k1 = fm & (result_g.value & ~result_g.mask);
          uint64_t derived_value =
              result_k0; /* known-0 in result -> known-1 in operand */
          uint64_t derived_known = result_k0 | result_k1;

          bennet_tagged_domain op_dom =
              bennet_tnum_transform_forward(term->data.unop.operand, state);
          tnum_generic op_g = tnum_from_tagged(&op_dom);
          uint64_t new_mask = op_g.mask & ~derived_known;
          tnum_generic refined = {
              .is_top = false,
              .is_bottom = false,
              .is_signed = op_g.is_signed,
              .width = op_g.width,
              .value = derived_value & ~new_mask & fm,
              .mask = new_mask & fm,
          };
          tnum_generic met = tnum_generic_meet(&op_g, &refined);
          bennet_tagged_domain met_td =
              tnum_to_tagged(&met, &term->data.unop.operand->base_type);
          return bennet_tnum_transform_backward(
              term->data.unop.operand, target_sym, met_td, state);
        }
      }
      return bennet_tnum_transform_backward(
          term->data.unop.operand, target_sym, output_domain, state);
    }

    case CN_TERM_ITE: {
      bennet_absint_state* then_state = bennet_tnum_transform_backward(
          term->data.ite.then_term, target_sym, output_domain, state);
      bennet_absint_state* else_state = bennet_tnum_transform_backward(
          term->data.ite.else_term, target_sym, output_domain, state);

      if (bennet_absint_state_is_bottom_tnum(then_state))
        return else_state;
      if (bennet_absint_state_is_bottom_tnum(else_state))
        return then_state;

      bennet_tagged_domain then_dom =
          bennet_absint_state_get_tnum(then_state, sym, &term->base_type);
      bennet_tagged_domain else_dom =
          bennet_absint_state_get_tnum(else_state, sym, &term->base_type);
      /* Use tnum join for the two branches */
      tnum_generic tg = tnum_from_tagged(&then_dom);
      tnum_generic eg = tnum_from_tagged(&else_dom);
      tnum_generic joined = tnum_generic_join(&tg, &eg);
      bennet_tagged_domain joined_td = tnum_to_tagged(&joined, &term->base_type);
      return bennet_absint_state_set_tnum(state, sym, joined_td);
    }

    case CN_TERM_ARRAY_SHIFT: {
      cn_term* base = term->data.array_shift.base;
      cn_term* index = term->data.array_shift.index;
      bool base_has_target = term_contains_sym(base, target_sym.id);
      bool index_has_target = term_contains_sym(index, target_sym.id);

      if (!base_has_target && !index_has_target)
        return bennet_absint_state_copy_tnum(state);

      tnum_generic result_g = tnum_from_tagged(&output_domain);

      if (base_has_target && !result_g.is_top && !result_g.is_bottom) {
        /* refined_base = result - (index * elem_size) */
        bennet_tagged_domain index_dom = bennet_tnum_transform_forward(index, state);
        tnum_generic index_g = tnum_from_tagged(&index_dom);
        int64_t elem_size = term->data.array_shift.element_size;
        tnum_generic elem_tnum =
            tnum_generic_const(index_g.width, index_g.is_signed, (uint64_t)elem_size);
        tnum_generic offset = tnum_generic_mul(&index_g, &elem_tnum);
        if (!offset.is_top && !offset.is_bottom) {
          offset.width = result_g.width;
          offset.is_signed = result_g.is_signed;
          tnum_generic refined = tnum_generic_sub(&result_g, &offset);
          if (!refined.is_top && !refined.is_bottom) {
            bennet_tagged_domain refined_td = tnum_to_tagged(&refined, &base->base_type);
            return bennet_tnum_transform_backward(base, target_sym, refined_td, state);
          }
        }
        return bennet_tnum_transform_backward(base, target_sym, output_domain, state);
      }

      if (index_has_target && !result_g.is_top && !result_g.is_bottom) {
        /* refined_index = (result - base) / elem_size */
        bennet_tagged_domain base_dom = bennet_tnum_transform_forward(base, state);
        tnum_generic base_g = tnum_from_tagged(&base_dom);
        /* Reinterpret types for subtraction */
        base_g.width = result_g.width;
        base_g.is_signed = result_g.is_signed;
        tnum_generic diff = tnum_generic_sub(&result_g, &base_g);
        if (!diff.is_top && !diff.is_bottom) {
          int width_idx;
          bool signed_idx;
          bennet_absint_type_info(&index->base_type, &width_idx, &signed_idx);
          diff.width = width_idx;
          diff.is_signed = signed_idx;
          int64_t elem_size = term->data.array_shift.element_size;
          tnum_generic elem_tnum =
              tnum_generic_const(width_idx, signed_idx, (uint64_t)elem_size);
          tnum_generic refined = tnum_generic_div(&diff, &elem_tnum);
          if (!refined.is_top && !refined.is_bottom) {
            bennet_tagged_domain refined_td = tnum_to_tagged(&refined, &index->base_type);
            return bennet_tnum_transform_backward(index, target_sym, refined_td, state);
          }
        }
        return bennet_tnum_transform_backward(index, target_sym, output_domain, state);
      }

      /* Fallback */
      if (base_has_target)
        return bennet_tnum_transform_backward(base, target_sym, output_domain, state);
      return bennet_tnum_transform_backward(index, target_sym, output_domain, state);
    }

    case CN_TERM_MEMBER_SHIFT: {
      cn_term* base = term->data.member_shift.base;
      if (!term_contains_sym(base, target_sym.id))
        return bennet_absint_state_copy_tnum(state);

      tnum_generic result_g = tnum_from_tagged(&output_domain);
      if (!result_g.is_top && !result_g.is_bottom) {
        int64_t offset = term->data.member_shift.offset;
        tnum_generic offset_tnum =
            tnum_generic_const(result_g.width, result_g.is_signed, (uint64_t)offset);
        tnum_generic refined = tnum_generic_sub(&result_g, &offset_tnum);
        if (!refined.is_top && !refined.is_bottom) {
          bennet_tagged_domain refined_td = tnum_to_tagged(&refined, &base->base_type);
          return bennet_tnum_transform_backward(base, target_sym, refined_td, state);
        }
      }
      return bennet_tnum_transform_backward(base, target_sym, output_domain, state);
    }

    default:
      return bennet_absint_state_copy_tnum(state);
  }
}

bennet_absint_state* bennet_tnum_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state) {
  if (!term || !state)
    return state;

  /* Handle NOT(expr) by recursing with flipped value */
  if (term->type == CN_TERM_UNOP && term->data.unop.op == CN_UNOP_NOT) {
    return bennet_tnum_transform_backward_assume(term->data.unop.operand, !value, state);
  }

  /* Handle comparison operators */
  if (term->type == CN_TERM_BINOP) {
    cn_term* left = term->data.binop.left;
    cn_term* right = term->data.binop.right;
    cn_binop op = term->data.binop.op;

    bennet_tagged_domain left_dom = bennet_tnum_transform_forward(left, state);
    bennet_tagged_domain right_dom = bennet_tnum_transform_forward(right, state);

    tnum_generic lg = tnum_from_tagged(&left_dom);
    tnum_generic rg = tnum_from_tagged(&right_dom);
    tnum_generic lg_refined = lg;
    tnum_generic rg_refined = rg;

    switch (op) {
      case CN_BINOP_EQ: {
        if (value) {
          /* a == b must be true: meet both domains */
          tnum_generic meet = tnum_generic_meet(&lg, &rg);
          lg_refined = meet;
          rg_refined = meet;
        } else {
          /* a != b: power-of-two mask elimination */
          bool lg_const = (lg.mask == 0);
          bool rg_const = (rg.mask == 0);

          if (rg_const && lg_const && lg.value == rg.value) {
            /* Both same constant - contradiction */
            lg_refined = tnum_generic_bottom(lg.width, lg.is_signed);
            rg_refined = tnum_generic_bottom(rg.width, rg.is_signed);
          } else if (rg_const) {
            /* Right is constant, try to refine left */
            uint64_t c = rg.value;
            if (!lg_const && lg.mask != 0 && (lg.mask & (lg.mask - 1)) == 0) {
              /* Left has power-of-two mask: represents exactly 2 values */
              uint64_t v1 = lg.value;
              uint64_t v2 = lg.value | lg.mask;
              if (v1 == c)
                lg_refined = tnum_generic_const(lg.width, lg.is_signed, v2);
              else if (v2 == c)
                lg_refined = tnum_generic_const(lg.width, lg.is_signed, v1);
            }
          } else if (lg_const) {
            /* Left is constant, try to refine right */
            uint64_t c = lg.value;
            if (!rg_const && rg.mask != 0 && (rg.mask & (rg.mask - 1)) == 0) {
              uint64_t v1 = rg.value;
              uint64_t v2 = rg.value | rg.mask;
              if (v1 == c)
                rg_refined = tnum_generic_const(rg.width, rg.is_signed, v2);
              else if (v2 == c)
                rg_refined = tnum_generic_const(rg.width, rg.is_signed, v1);
            }
          }
        }
        break;
      }

      case CN_BINOP_LE:
      case CN_BINOP_LE_POINTER: {
        int64_t min_val, max_val;
        tnum_get_extrema(lg.width, lg.is_signed, &min_val, &max_val);
        int64_t t1_val =
            lg.is_signed ? tnum_to_signed_value(lg.width, lg.value) : (int64_t)lg.value;
        int64_t t2_val =
            rg.is_signed ? tnum_to_signed_value(rg.width, rg.value) : (int64_t)rg.value;
        if (value) {
          /* a <= b must be true */
          if (rg.mask == 0) {
            /* Right is constant: left <= t2_val */
            tnum_generic t1_interval = tnum_generic_of_interval(
                lg.width, lg.is_signed, (uint64_t)min_val, (uint64_t)t2_val);
            lg_refined = tnum_generic_meet(&lg, &t1_interval);
          }
          if (lg.mask == 0) {
            /* Left is constant: t1_val <= right */
            tnum_generic t2_interval = tnum_generic_of_interval(
                rg.width, rg.is_signed, (uint64_t)t1_val, (uint64_t)max_val);
            rg_refined = tnum_generic_meet(&rg, &t2_interval);
          }
        } else {
          /* a <= b is false means a > b */
          if (lg.mask == 0) {
            /* Left is constant: right < t1_val => right <= t1_val - 1 */
            tnum_generic t2_interval = tnum_generic_of_interval(
                rg.width, rg.is_signed, (uint64_t)min_val, (uint64_t)(t1_val - 1));
            rg_refined = tnum_generic_meet(&rg, &t2_interval);
          }
          if (rg.mask == 0) {
            /* Right is constant: left > t2_val => left >= t2_val + 1 */
            tnum_generic t1_interval = tnum_generic_of_interval(
                lg.width, lg.is_signed, (uint64_t)(t2_val + 1), (uint64_t)max_val);
            lg_refined = tnum_generic_meet(&lg, &t1_interval);
          }
        }
        break;
      }

      case CN_BINOP_LT:
      case CN_BINOP_LT_POINTER: {
        int64_t min_val, max_val;
        tnum_get_extrema(lg.width, lg.is_signed, &min_val, &max_val);
        int64_t t1_val =
            lg.is_signed ? tnum_to_signed_value(lg.width, lg.value) : (int64_t)lg.value;
        int64_t t2_val =
            rg.is_signed ? tnum_to_signed_value(rg.width, rg.value) : (int64_t)rg.value;
        if (value) {
          /* a < b must be true */
          if (rg.mask == 0) {
            /* Right is constant: left < t2_val => left <= t2_val - 1 */
            tnum_generic t1_interval = tnum_generic_of_interval(
                lg.width, lg.is_signed, (uint64_t)min_val, (uint64_t)(t2_val - 1));
            lg_refined = tnum_generic_meet(&lg, &t1_interval);
          }
          if (lg.mask == 0) {
            /* Left is constant: t1_val < right => right >= t1_val + 1 */
            tnum_generic t2_interval = tnum_generic_of_interval(
                rg.width, rg.is_signed, (uint64_t)(t1_val + 1), (uint64_t)max_val);
            rg_refined = tnum_generic_meet(&rg, &t2_interval);
          }
        } else {
          /* a < b is false means b <= a */
          if (lg.mask == 0) {
            /* Left is constant: right <= t1_val */
            tnum_generic t2_interval = tnum_generic_of_interval(
                rg.width, rg.is_signed, (uint64_t)min_val, (uint64_t)t1_val);
            rg_refined = tnum_generic_meet(&rg, &t2_interval);
          }
          if (rg.mask == 0) {
            /* Right is constant: left >= t2_val */
            tnum_generic t1_interval = tnum_generic_of_interval(
                lg.width, lg.is_signed, (uint64_t)t2_val, (uint64_t)max_val);
            lg_refined = tnum_generic_meet(&lg, &t1_interval);
          }
        }
        break;
      }

      case CN_BINOP_AND: {
        if (value) {
          /* Both sides must be true */
          bennet_absint_state* result =
              bennet_tnum_transform_backward_assume(left, true, state);
          return bennet_tnum_transform_backward_assume(right, true, result);
        }
        return bennet_absint_state_copy_tnum(state);
      }

      case CN_BINOP_OR: {
        if (!value) {
          /* Both sides must be false */
          bennet_absint_state* result =
              bennet_tnum_transform_backward_assume(left, false, state);
          return bennet_tnum_transform_backward_assume(right, false, result);
        }
        return bennet_absint_state_copy_tnum(state);
      }

      default:
        return bennet_absint_state_copy_tnum(state);
    }

    /* Apply refinements */
    bennet_absint_state* result = bennet_absint_state_copy_tnum(state);

    /* Unwrap a single CAST to reach the underlying symbol */
    cn_term* left_inner = (left->type == CN_TERM_CAST) ? left->data.cast.value : left;
    cn_term* right_inner = (right->type == CN_TERM_CAST) ? right->data.cast.value : right;

    if (left_inner->type == CN_TERM_SYM) {
      bennet_absint_sym sym = {
          .name = left_inner->data.sym.name, .id = left_inner->data.sym.id};
      cn_base_type* inner_type = &left_inner->base_type;
      int inner_width;
      bool inner_signed;
      bennet_absint_type_info(inner_type, &inner_width, &inner_signed);
      if (inner_width == lg_refined.width) {
        bennet_tagged_domain refined = tnum_to_tagged(&lg_refined, inner_type);
        result = bennet_absint_state_meet_tnum(result, sym, refined);
      }
    }

    if (right_inner->type == CN_TERM_SYM) {
      bennet_absint_sym sym = {
          .name = right_inner->data.sym.name, .id = right_inner->data.sym.id};
      cn_base_type* inner_type = &right_inner->base_type;
      int inner_width;
      bool inner_signed;
      bennet_absint_type_info(inner_type, &inner_width, &inner_signed);
      if (inner_width == rg_refined.width) {
        bennet_tagged_domain refined = tnum_to_tagged(&rg_refined, inner_type);
        result = bennet_absint_state_meet_tnum(result, sym, refined);
      }
    }

    return result;
  }

  return bennet_absint_state_copy_tnum(state);
}
