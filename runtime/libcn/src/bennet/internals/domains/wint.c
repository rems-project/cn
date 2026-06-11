#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils.h>
#include <cn-smt/memory/std_alloc.h>

#define WINT_GEN(sm)                                                                     \
  uint##sm##_t bennet_arbitrary_wint_uint##sm##_t(                                       \
      bennet_domain_wint(uint##sm##_t) * d) {                                            \
    uint##sm##_t start = d->start;                                                       \
    uint##sm##_t end = d->end;                                                           \
                                                                                         \
    if (start == end) {                                                                  \
      return start;                                                                      \
    }                                                                                    \
                                                                                         \
    if (start == 0 && end == UINT##sm##_MAX) {                                           \
      return bennet_arbitrary_sized_top(uint##sm##_t);                                   \
    }                                                                                    \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
    size_t width = end - start + 1;                                                      \
    if (width > sz) {                                                                    \
      width = sz;                                                                        \
    }                                                                                    \
                                                                                         \
    uint##sm##_t res = bennet_uniform_uint##sm##_t(width);                               \
    if (start <= end) {                                                                  \
      return res + start;                                                                \
    }                                                                                    \
                                                                                         \
    if (res <= end) {                                                                    \
      return res;                                                                        \
    }                                                                                    \
                                                                                         \
    return (res - end) + start;                                                          \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_arbitrary_wint_int##sm##_t(bennet_domain_wint(int##sm##_t) * d) {   \
    int##sm##_t start = d->start;                                                        \
    int##sm##_t end = d->end;                                                            \
    int##sm##_t orig_end = end;                                                          \
                                                                                         \
    if (start == end) {                                                                  \
      return start;                                                                      \
    }                                                                                    \
                                                                                         \
    if (start == INT##sm##_MIN && end == INT##sm##_MAX) {                                \
      return bennet_arbitrary_sized_top(int##sm##_t);                                    \
    }                                                                                    \
                                                                                         \
    bool offset = false;                                                                 \
    if (start <= end) {                                                                  \
      offset = true;                                                                     \
      start -= (orig_end == INT##sm##_MAX);                                              \
    }                                                                                    \
                                                                                         \
    int64_t sz = (int64_t)bennet_get_size();                                             \
                                                                                         \
    if (end < start) {                                                                   \
      /* -n....n....end....start */                                                      \
      if (sz - 1 <= end) {                                                               \
        start = -sz;                                                                     \
        end = sz;                                                                        \
      }                                                                                  \
                                                                                         \
      /* end....start....-n....n */                                                      \
      else if (start <= -sz + 1) {                                                       \
        start = -sz;                                                                     \
        end = sz;                                                                        \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    if (start <= end) {                                                                  \
      /* Shifts the range bounds to be centered around zero, */                          \
      /* while ensuring `end - start` < `2 * sz` */                                      \
      if (start <= -sz + 1) {                                                            \
        if (end >= sz) {                                                                 \
          start = -sz + 1;                                                               \
          end = sz - 1;                                                                  \
        } else {                                                                         \
          int64_t excess = (sz - end);                                                   \
          if (start < -sz + 1 - excess) {                                                \
            start = -sz + 1 - excess;                                                    \
          }                                                                              \
        }                                                                                \
      } else {                                                                           \
        int64_t excess = start - (-sz + 1);                                              \
        if (end > sz - 1 + excess) {                                                     \
          end = sz - 1 + excess;                                                         \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      assert(end - start < 2 * sz);                                                      \
    }                                                                                    \
                                                                                         \
    size_t width = end - start + 1;                                                      \
    if (width >= (size_t)(2 * sz)) {                                                     \
      width = 2 * sz - 1;                                                                \
    }                                                                                    \
                                                                                         \
    uint##sm##_t res = bennet_uniform_uint##sm##_t(width);                               \
                                                                                         \
    if (offset) {                                                                        \
      res += (orig_end == INT##sm##_MAX);                                                \
    }                                                                                    \
                                                                                         \
    if (start <= end) {                                                                  \
      return res + start;                                                                \
    }                                                                                    \
                                                                                         \
    /* Disjoint wrapping interval: (INT_MIN, end] ∪ [start, INT_MAX) */                  \
    if (end >= 0) {                                                                      \
      /* If values >= start will ever be closer to 0 */                                  \
      if (start <= width - end) {                                                        \
        /* (-start, end] */                                                              \
        int##sm##_t below_end = end + start;                                             \
        int##sm##_t excess = width - below_end;                                          \
                                                                                         \
        /* (-start - excess / 2, end] ∪ [start, start + excess / 2] */                   \
        below_end += excess / 2;                                                         \
        excess = (excess + 1) / 2;                                                       \
                                                                                         \
        if (res < below_end) {                                                           \
          return end - res;                                                              \
        } else {                                                                         \
          return (res - below_end) + start;                                              \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      return end - res;                                                                  \
    }                                                                                    \
                                                                                         \
    if (start <= 0) {                                                                    \
      /* If values <= end will ever be closer to 0 */                                    \
      if (end >= -(width + start)) {                                                     \
        /* [start, -end) */                                                              \
        int##sm##_t above_start = -start - end;                                          \
        int##sm##_t excess = width - above_start;                                        \
                                                                                         \
        above_start += (excess + 1) / 2;                                                 \
        excess /= 2;                                                                     \
                                                                                         \
        if (res < above_start) {                                                         \
          return start + res;                                                            \
        } else {                                                                         \
          return end - (res - above_start);                                              \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      return end - res;                                                                  \
    }                                                                                    \
                                                                                         \
    int##sm##_t diff = start + end;                                                      \
    if (diff < 0) {                                                                      \
      diff = -diff;                                                                      \
    }                                                                                    \
                                                                                         \
    if (-start <= end) {                                                                 \
      int##sm##_t below_end = diff;                                                      \
      int##sm##_t excess = width - below_end;                                            \
                                                                                         \
      below_end += excess / 2;                                                           \
      excess = (excess + 1) / 2;                                                         \
                                                                                         \
      if (res < below_end) {                                                             \
        return end - res;                                                                \
      } else {                                                                           \
        return (res - below_end) + start;                                                \
      }                                                                                  \
    } else {                                                                             \
      int##sm##_t above_start = diff;                                                    \
      int##sm##_t excess = width - above_start;                                          \
                                                                                         \
      above_start += (excess + 1) / 2;                                                   \
      excess /= 2;                                                                       \
                                                                                         \
      if (res < above_start) {                                                           \
        return start + res;                                                              \
      } else {                                                                           \
        return end - (res - above_start);                                                \
      }                                                                                  \
    }                                                                                    \
  }

WINT_GEN(8);
WINT_GEN(16);
WINT_GEN(32);
WINT_GEN(64);

// For uintptr_t, use appropriate size based on platform

uintptr_t bennet_arbitrary_wint_uintptr_t(bennet_domain_wint(uintptr_t) * d) {
  assert(sizeof(uintptr_t) == sizeof(uint64_t));
  return (uintptr_t)bennet_arbitrary_wint_uint64_t((bennet_domain_wint(uint64_t)*)d);
}

// Helper functions for wrapped interval membership
#define WINT_MEMBER(cty, val, start, end)                                                \
  ((start) <= (end) ? ((val) >= (start) && (val) <= (end))                               \
                    : ((val) >= (start) || (val) <= (end)))

// Generate complete abstract domain interface for each type
#define WINT_DOMAIN_IMPL(cty)                                                                            \
  bennet_domain_wint(cty) * bennet_domain_wint_top_##cty(void) {                                         \
    bennet_domain_wint(cty)* result = std_malloc(sizeof(bennet_domain_wint(cty)));                       \
    assert(result);                                                                                      \
    result->top = true;                                                                                  \
    result->bottom = false;                                                                              \
    result->start = BV_MIN(cty);                                                                         \
    result->end = BV_MAX(cty);                                                                           \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_bottom_##cty(void) {                                      \
    bennet_domain_wint(cty)* result = std_malloc(sizeof(bennet_domain_wint(cty)));                       \
    assert(result);                                                                                      \
    result->top = false;                                                                                 \
    result->bottom = true;                                                                               \
    result->start = 0;                                                                                   \
    result->end = 0;                                                                                     \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_of_##cty(cty start, cty end) {                            \
    bennet_domain_wint(cty)* result = std_malloc(sizeof(bennet_domain_wint(cty)));                       \
    assert(result);                                                                                      \
    result->top = false;                                                                                 \
    result->bottom = false;                                                                              \
    result->start = start;                                                                               \
    result->end = end;                                                                                   \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bool bennet_domain_wint_is_top_##cty(bennet_domain_wint(cty) * d) {                                    \
    return d->top;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bool bennet_domain_wint_is_bottom_##cty(bennet_domain_wint(cty) * d) {                                 \
    return d->bottom;                                                                                    \
  }                                                                                                      \
                                                                                                         \
  bool bennet_domain_wint_equal_##cty(                                                                   \
      bennet_domain_wint(cty) * d1, bennet_domain_wint(cty) * d2) {                                      \
    if (d1->top && d2->top)                                                                              \
      return true;                                                                                       \
    if (d1->bottom && d2->bottom)                                                                        \
      return true;                                                                                       \
    if (d1->top || d1->bottom || d2->top || d2->bottom)                                                  \
      return false;                                                                                      \
    return d1->start == d2->start && d1->end == d2->end;                                                 \
  }                                                                                                      \
                                                                                                         \
  bool bennet_domain_wint_leq_##cty(                                                                     \
      bennet_domain_wint(cty) * d1, bennet_domain_wint(cty) * d2) {                                      \
    if (d1->bottom)                                                                                      \
      return true;                                                                                       \
    if (d2->top)                                                                                         \
      return true;                                                                                       \
    if (d1->top && !d2->top)                                                                             \
      return false;                                                                                      \
    if (d2->bottom)                                                                                      \
      return false;                                                                                      \
                                                                                                         \
    /* Check if d1 interval is contained in d2 interval */                                               \
    if (d2->start <= d2->end) {                                                                          \
      /* d2 is normal interval [start, end] */                                                           \
      if (d1->start <= d1->end) {                                                                        \
        /* d1 is normal interval - check containment */                                                  \
        return d1->start >= d2->start && d1->end <= d2->end;                                             \
      } else {                                                                                           \
        /* d1 is wrapped interval - never contained in normal interval */                                \
        return false;                                                                                    \
      }                                                                                                  \
    } else {                                                                                             \
      /* d2 is wrapped interval (MIN, end] ∪ [start, MAX) */                                             \
      if (d1->start <= d1->end) {                                                                        \
        /* d1 is normal - check if contained in either part */                                           \
        return (d1->start >= d2->start) || (d1->end <= d2->end);                                         \
      } else {                                                                                           \
        /* d1 is wrapped - check if start >= d2->start and end <= d2->end */                             \
        return d1->start >= d2->start && d1->end <= d2->end;                                             \
      }                                                                                                  \
    }                                                                                                    \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_join_##cty(bennet_domain_wint(cty) * d1,                  \
                                bennet_domain_wint(cty) * d2) {                                          \
    bennet_domain_wint(cty)* result = std_malloc(sizeof(bennet_domain_wint(cty)));                       \
    assert(result);                                                                                      \
                                                                                                         \
    if (d1->top || d2->top) {                                                                            \
      result->top = true;                                                                                \
      result->bottom = false;                                                                            \
      result->start = BV_MIN(cty);                                                                       \
      result->end = BV_MAX(cty);                                                                         \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    if (d1->bottom) {                                                                                    \
      *result = *d2;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    if (d2->bottom) {                                                                                    \
      *result = *d1;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    /* For simplicity, conservative join: if intervals don't trivially contain each other, return top */ \
    if (bennet_domain_wint_leq_##cty(d1, d2)) {                                                          \
      *result = *d2;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    if (bennet_domain_wint_leq_##cty(d2, d1)) {                                                          \
      *result = *d1;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    /* Conservative: return top for complex cases */                                                     \
    result->top = true;                                                                                  \
    result->bottom = false;                                                                              \
    result->start = BV_MIN(cty);                                                                         \
    result->end = BV_MAX(cty);                                                                           \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_meet_##cty(bennet_domain_wint(cty) * d1,                  \
                                bennet_domain_wint(cty) * d2) {                                          \
    bennet_domain_wint(cty)* result = std_malloc(sizeof(bennet_domain_wint(cty)));                       \
    assert(result);                                                                                      \
                                                                                                         \
    if (d1->bottom || d2->bottom) {                                                                      \
      result->top = false;                                                                               \
      result->bottom = true;                                                                             \
      result->start = 0;                                                                                 \
      result->end = 0;                                                                                   \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    if (d1->top) {                                                                                       \
      *result = *d2;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    if (d2->top) {                                                                                       \
      *result = *d1;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    /* Use unsigned arithmetic for wrapped interval meet */                                              \
    uint64_t a = (uint64_t)d1->start;                                                                    \
    uint64_t b = (uint64_t)d1->end;                                                                      \
    uint64_t c = (uint64_t)d2->start;                                                                    \
    uint64_t d = (uint64_t)d2->end;                                                                      \
    int w = (int)(sizeof(cty) * 8);                                                                      \
    uint64_t mask = (w >= 64) ? UINT64_MAX : ((uint64_t)1 << w) - 1;                                     \
    a &= mask;                                                                                           \
    b &= mask;                                                                                           \
    c &= mask;                                                                                           \
    d &= mask;                                                                                           \
                                                                                                         \
    /* Membership check: v in [s,e] iff (v-s) mod 2^w <= (e-s) mod 2^w */                                \
    uint64_t ab = (b - a) & mask;                                                                        \
    uint64_t cd = (d - c) & mask;                                                                        \
                                                                                                         \
    bool a_in_cd = ((a - c) & mask) <= cd;                                                               \
    bool b_in_cd = ((b - c) & mask) <= cd;                                                               \
    bool c_in_ab = ((c - a) & mask) <= ab;                                                               \
    bool d_in_ab = ((d - a) & mask) <= ab;                                                               \
                                                                                                         \
    bool g1_in_g2 = a_in_cd && b_in_cd;                                                                  \
    bool g2_in_g1 = c_in_ab && d_in_ab;                                                                  \
                                                                                                         \
    result->top = false;                                                                                 \
    result->bottom = false;                                                                              \
                                                                                                         \
    if (g1_in_g2 && g2_in_g1) {                                                                          \
      /* Both contain each other - return smaller cardinality */                                         \
      if (ab <= cd) {                                                                                    \
        result->start = d1->start;                                                                       \
        result->end = d1->end;                                                                           \
      } else {                                                                                           \
        result->start = d2->start;                                                                       \
        result->end = d2->end;                                                                           \
      }                                                                                                  \
    } else if (g1_in_g2) {                                                                               \
      result->start = d1->start;                                                                         \
      result->end = d1->end;                                                                             \
    } else if (g2_in_g1) {                                                                               \
      result->start = d2->start;                                                                         \
      result->end = d2->end;                                                                             \
    } else if (c_in_ab) {                                                                                \
      /* Overlapping: c is in [a,b] */                                                                   \
      result->start = d2->start;                                                                         \
      result->end = d1->end;                                                                             \
    } else if (a_in_cd) {                                                                                \
      /* Overlapping: a is in [c,d] */                                                                   \
      result->start = d1->start;                                                                         \
      result->end = d2->end;                                                                             \
    } else {                                                                                             \
      /* Disjoint */                                                                                     \
      result->top = false;                                                                               \
      result->bottom = true;                                                                             \
      result->start = 0;                                                                                 \
      result->end = 0;                                                                                   \
    }                                                                                                    \
                                                                                                         \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_copy_##cty(bennet_domain_wint(cty) * d) {                 \
    bennet_domain_wint(cty)* result = std_malloc(sizeof(bennet_domain_wint(cty)));                       \
    assert(result);                                                                                      \
    *result = *d;                                                                                        \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  cty bennet_domain_wint_arbitrary_##cty(bennet_domain_wint(cty) * d) {                                  \
    return bennet_arbitrary_wint_##cty(d);                                                               \
  }                                                                                                      \
                                                                                                         \
  bool bennet_domain_wint_check_##cty(cty v, bennet_domain_wint(cty) * d) {                              \
    if (d->bottom) {                                                                                     \
      return false;                                                                                      \
    }                                                                                                    \
                                                                                                         \
    if (d->top) {                                                                                        \
      return true;                                                                                       \
    }                                                                                                    \
                                                                                                         \
    if (d->start <= d->end) {                                                                            \
      return d->start <= v && v <= d->end;                                                               \
    }                                                                                                    \
                                                                                                         \
    return d->start <= v || v <= d->end;                                                                 \
  }                                                                                                      \
                                                                                                         \
  bool bennet_domain_wint_to_interval_##cty(                                                             \
      bennet_domain_wint(cty) * d, cty * lo_out, cty * hi_out) {                                         \
    if (d->top || d->bottom) {                                                                           \
      return false;                                                                                      \
    }                                                                                                    \
    /* Wrapping interval: start > end means the interval wraps around */                                 \
    if (d->start > d->end) {                                                                             \
      return false;                                                                                      \
    }                                                                                                    \
    *lo_out = d->start;                                                                                  \
    *hi_out = d->end;                                                                                    \
    return true;                                                                                         \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_from_assignment_##cty(                                    \
                                void* base_ptr, void* addr, size_t bytes) {                              \
    if (sizeof(cty) == sizeof(uintptr_t) && bytes > 0) {                                                 \
      uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();                                        \
      uintptr_t max_ptr = (uintptr_t)bennet_rand_alloc_max_ptr();                                        \
      uintptr_t offset = (uintptr_t)addr - (uintptr_t)base_ptr;                                          \
      /* Check for underflow: if offset > min_ptr, lo wraps */                                           \
      if (offset > min_ptr) {                                                                            \
        return bennet_domain_wint_top_##cty();                                                           \
      }                                                                                                  \
      uintptr_t lo = min_ptr - offset;                                                                   \
      /* Check for underflow: if offset + bytes - 1 > max_ptr, hi wraps */                               \
      if (offset + bytes - 1 > max_ptr) {                                                                \
        return bennet_domain_wint_top_##cty();                                                           \
      }                                                                                                  \
      uintptr_t hi = max_ptr - offset - bytes + 1;                                                       \
      if (hi < lo) {                                                                                     \
        return bennet_domain_wint_top_##cty();                                                           \
      }                                                                                                  \
      return bennet_domain_wint_of_##cty((cty)lo, (cty)hi);                                              \
    }                                                                                                    \
    return bennet_domain_wint_top_##cty();                                                               \
  }

WINT_DOMAIN_IMPL(uint8_t)
WINT_DOMAIN_IMPL(uint16_t)
WINT_DOMAIN_IMPL(uint32_t)
WINT_DOMAIN_IMPL(uint64_t)
WINT_DOMAIN_IMPL(uintptr_t)
WINT_DOMAIN_IMPL(int8_t)
WINT_DOMAIN_IMPL(int16_t)
WINT_DOMAIN_IMPL(int32_t)
WINT_DOMAIN_IMPL(int64_t)

/*=============================================================================
 * Abstract Transformers API Implementation
 *
 * Implements the abstract transformers declared in domain.h for wrapped
 * interval domains. Ported from wrappedInterval.ml.
 *===========================================================================*/

#include <bennet/internals/domains/wint.h>
#include <cn-smt/terms.h>

/*-----------------------------------------------------------------------------
 * Helper Functions for Wrapped Interval Arithmetic
 *---------------------------------------------------------------------------*/

/**
 * Return the bitmask for a given width: (2^width - 1).
 * For width >= 64, returns UINT64_MAX.
 */
static inline uint64_t wint_mask(int width) {
  if (width >= 64) {
    return UINT64_MAX;
  }
  return ((uint64_t)1 << width) - 1;
}

/**
 * Normalize a value to the range [0, 2^width) for unsigned interpretation.
 */
static inline uint64_t wint_normalize_unsigned(int64_t value, int width) {
  if (width >= 64) {
    return (uint64_t)value;
  }
  uint64_t modulus = (uint64_t)1 << width;
  int64_t result = value % (int64_t)modulus;
  if (result < 0) {
    result += (int64_t)modulus;
  }
  return (uint64_t)result;
}

/**
 * Get the minimum value for a bitvector type.
 */
static inline int64_t wint_get_min(bool is_signed, int width) {
  if (is_signed) {
    if (width >= 64) {
      return INT64_MIN;
    }
    return -((int64_t)1 << (width - 1));
  }
  return 0;
}

/**
 * Get the maximum value for a bitvector type.
 */
static inline int64_t wint_get_max(bool is_signed, int width) {
  if (is_signed) {
    if (width >= 64) {
      return INT64_MAX;
    }
    return ((int64_t)1 << (width - 1)) - 1;
  }
  if (width >= 64) {
    // UINT64_MAX bit pattern as int64_t is -1 (well-defined in C23/two's complement)
    return (int64_t)-1;
  }
  return ((int64_t)1 << width) - 1;
}

/**
 * Compute the cardinality of a wrapped interval [start, stop].
 * WCard(a,b) = (b-a+1) mod 2^w
 */
static inline uint64_t wint_cardinality(int64_t start, int64_t stop, int width) {
  uint64_t start_u = wint_normalize_unsigned(start, width);
  uint64_t stop_u = wint_normalize_unsigned(stop, width);
  if (width >= 64) {
    uint64_t result = stop_u - start_u + 1;
    return result == 0 ? UINT64_MAX : result;
  }
  uint64_t modulus = (uint64_t)1 << width;
  uint64_t result = (stop_u - start_u + 1) % modulus;
  if (result == 0) {
    return modulus;  // Full range
  }
  return result;
}

/**
 * Check if a value is a member of a wrapped interval [start, stop].
 */
static inline bool wint_member(int64_t value, int64_t start, int64_t stop, int width) {
  uint64_t v = wint_normalize_unsigned(value, width);
  uint64_t s = wint_normalize_unsigned(start, width);
  uint64_t e = wint_normalize_unsigned(stop, width);
  if (width >= 64) {
    // For 64-bit, subtraction wraps naturally in uint64_t
    return (v - s) <= (e - s);
  }
  uint64_t diff_v = (v - s) % ((uint64_t)1 << width);
  uint64_t diff_e = (e - s) % ((uint64_t)1 << width);
  return diff_v <= diff_e;
}

/**
 * Check if interval crosses south pole (unsigned wrap: MAX to 0).
 */
static inline bool wint_crosses_south(int64_t start, int64_t stop, int width) {
  uint64_t s = wint_normalize_unsigned(start, width);
  uint64_t e = wint_normalize_unsigned(stop, width);
  return s > e;
}

/*-----------------------------------------------------------------------------
 * Bitwise Operation Bounds (Hacker's Delight algorithms)
 *---------------------------------------------------------------------------*/

/**
 * Compute minimum OR: scan from MSB, try to reduce operands.
 */
static inline uint64_t wint_min_or(
    uint64_t a, uint64_t b, uint64_t c, uint64_t d, int width) {
  uint64_t m = (uint64_t)1 << (width - 1);
  while (m != 0) {
    uint64_t not_a_and_c_and_m = (~a) & c & m;
    uint64_t a_and_not_c_and_m = a & (~c) & m;
    if (not_a_and_c_and_m == m) {
      uint64_t temp = (a | m) & (~m + 1);
      if (temp <= b) {
        a = temp;
      }
    } else if (a_and_not_c_and_m == m) {
      uint64_t temp = (c | m) & (~m + 1);
      if (temp <= d) {
        c = temp;
      }
    }
    m >>= 1;
  }
  return a | c;
}

/**
 * Compute maximum OR: scan from MSB, try to maximize result.
 */
static inline uint64_t wint_max_or(
    uint64_t a, uint64_t b, uint64_t c, uint64_t d, int width) {
  uint64_t m = (uint64_t)1 << (width - 1);
  while (m != 0) {
    if ((b & d & m) == m) {
      uint64_t temp = (b - m) | (m - 1);
      if (temp >= a) {
        return temp | d;
      }
      temp = (d - m) | (m - 1);
      if (temp >= c) {
        return b | temp;
      }
    }
    m >>= 1;
  }
  return b | d;
}

/**
 * Compute minimum AND: scan from MSB, try to reduce result.
 */
static inline uint64_t wint_min_and(
    uint64_t a, uint64_t b, uint64_t c, uint64_t d, int width) {
  uint64_t m = (uint64_t)1 << (width - 1);
  while (m != 0) {
    if (((~a) & (~c) & m) == m) {
      uint64_t temp = (a | m) & (~m + 1);
      if (temp <= b) {
        return temp & c;
      }
      temp = (c | m) & (~m + 1);
      if (temp <= d) {
        return a & temp;
      }
    }
    m >>= 1;
  }
  return a & c;
}

/**
 * Compute maximum AND: scan from MSB, try to maximize result.
 */
static inline uint64_t wint_max_and(
    uint64_t a, uint64_t b, uint64_t c, uint64_t d, int width) {
  uint64_t m = (uint64_t)1 << (width - 1);
  while (m != 0) {
    if ((b & (~d) & m) == m) {
      uint64_t temp = (b & (~m)) | (m - 1);
      if (temp >= a) {
        return temp & d;
      }
    } else if (((~b) & d & m) == m) {
      uint64_t temp = (d & (~m)) | (m - 1);
      if (temp >= c) {
        return b & temp;
      }
    }
    m >>= 1;
  }
  return b & d;
}

/**
 * Compute minimum XOR using De Morgan's law: a^b = (a & ~b) | (~a & b)
 */
static inline uint64_t wint_min_xor(
    uint64_t a, uint64_t b, uint64_t c, uint64_t d, int width) {
  uint64_t mask = wint_mask(width);
  // ~[c,d] = [~d, ~c] (bitwise complement swaps and inverts bounds)
  uint64_t not_c = (~c) & mask;
  uint64_t not_d = (~d) & mask;
  // ~[a,b] = [~b, ~a]
  uint64_t not_a = (~a) & mask;
  uint64_t not_b = (~b) & mask;

  // min_xor = min_and(a,b,~d,~c) | min_and(~b,~a,c,d)
  uint64_t part1 = wint_min_and(a, b, not_d, not_c, width);
  uint64_t part2 = wint_min_and(not_b, not_a, c, d, width);
  return part1 | part2;
}

/**
 * Compute maximum XOR using De Morgan's law: a^b = (a & ~b) | (~a & b)
 */
static inline uint64_t wint_max_xor(
    uint64_t a, uint64_t b, uint64_t c, uint64_t d, int width) {
  uint64_t mask = wint_mask(width);
  // ~[c,d] = [~d, ~c]
  uint64_t not_c = (~c) & mask;
  uint64_t not_d = (~d) & mask;
  // ~[a,b] = [~b, ~a]
  uint64_t not_a = (~a) & mask;
  uint64_t not_b = (~b) & mask;

  // max_xor = max_or(0, max_and(a,b,~d,~c), 0, max_and(~b,~a,c,d))
  uint64_t part1 = wint_max_and(a, b, not_d, not_c, width);
  uint64_t part2 = wint_max_and(not_b, not_a, c, d, width);
  return wint_max_or(0, part1, 0, part2, width);
}

/*-----------------------------------------------------------------------------
 * Pole Splitting Helpers
 *---------------------------------------------------------------------------*/

/**
 * Maximum number of intervals from pole splits.
 * A north+south split can produce at most 4 intervals.
 */
#define WINT_MAX_SPLITS 4

/**
 * North pole split: cut at the north pole boundary (0111...1 -> 1000...0)
 * for signed operations. Returns the number of resulting intervals.
 */
static int wint_north_split(
    int64_t start, int64_t stop, int width, int64_t out_starts[], int64_t out_stops[]) {
  // North pole boundary values
  int64_t np_lb = wint_get_max(true, width);  // 0111...1
  int64_t np_ub = wint_get_min(true, width);  // 1000...0

  // Check if interval contains the north pole [np_lb, np_ub]
  bool contains_np =
      wint_member(np_lb, start, stop, width) && wint_member(np_ub, start, stop, width);

  if (!contains_np) {
    out_starts[0] = start;
    out_stops[0] = stop;
    return 1;
  }

  // Split into [start, np_lb] and [np_ub, stop]
  out_starts[0] = start;
  out_stops[0] = np_lb;
  out_starts[1] = np_ub;
  out_stops[1] = stop;
  return 2;
}

/**
 * South pole split: cut at the south pole boundary (111...1 -> 000...0)
 * for unsigned operations. Returns the number of resulting intervals.
 */
static int wint_south_split(
    int64_t start, int64_t stop, int width, int64_t out_starts[], int64_t out_stops[]) {
  // South pole boundary values
  int64_t sp_lb = wint_get_max(false, width);  // 111...1
  int64_t sp_ub = 0;                           // 000...0

  // Check if interval contains the south pole [sp_lb, sp_ub]
  bool contains_sp =
      wint_member(sp_lb, start, stop, width) && wint_member(sp_ub, start, stop, width);

  if (!contains_sp) {
    out_starts[0] = start;
    out_stops[0] = stop;
    return 1;
  }

  // Split into [start, sp_lb] and [sp_ub, stop]
  out_starts[0] = start;
  out_stops[0] = sp_lb;
  out_starts[1] = sp_ub;
  out_stops[1] = stop;
  return 2;
}

/**
 * Combined pole split (north + south). Returns number of intervals.
 */
static int wint_pole_split(
    int64_t start, int64_t stop, int width, int64_t out_starts[], int64_t out_stops[]) {
  int64_t ns[2], ne[2];
  int nn = wint_north_split(start, stop, width, ns, ne);

  int count = 0;
  for (int i = 0; i < nn; i++) {
    int64_t ss[2], se[2];
    int sn = wint_south_split(ns[i], ne[i], width, ss, se);
    for (int j = 0; j < sn; j++) {
      out_starts[count] = ss[j];
      out_stops[count] = se[j];
      count++;
    }
  }
  return count;
}

/**
 * Check if MSB is zero (value is non-negative in signed interpretation).
 */
static inline bool wint_is_msb_zero(int64_t value, int width) {
  if (width >= 64) {
    return value >= 0;
  }
  int64_t msb_mask = (int64_t)1 << (width - 1);
  return (value & msb_mask) == 0;
}

/*-----------------------------------------------------------------------------
 * Tagged Domain Implementation
 *---------------------------------------------------------------------------*/

/**
 * Extract width and signedness from cn_base_type.
 */
static void get_type_info(cn_base_type* type, int* width, bool* is_signed) {
  assert(type);
  if (type->tag == CN_BASE_BITS) {
    *width = type->data.bits.size_bits;
    *is_signed = type->data.bits.is_signed;
  } else if (type->tag == CN_BASE_LOC) {
    *width = 64;  // Pointer type - use 64-bit width
    *is_signed = false;
  } else {
    // Default to 64-bit unsigned for unsupported types
    *width = 64;
    *is_signed = false;
  }
}

/**
 * Internal structure for a generic wrapped interval.
 * We use uint64_t to store values and track signedness separately.
 */
typedef struct {
  bool is_top;
  bool is_bottom;
  bool is_signed;
  int width;
  int64_t start;
  int64_t stop;
} wint_generic;

/**
 * Create a generic wrapped interval from a tagged domain.
 */
static wint_generic wint_from_tagged(bennet_tagged_domain* d) {
  wint_generic result = {0};
  if (!d || !d->type || !d->domain) {
    result.is_top = true;
    result.width = 64;
    return result;
  }

  get_type_info(d->type, &result.width, &result.is_signed);

  // Dispatch based on width and signedness
  if (result.is_signed) {
    switch (result.width) {
      case 8: {
        bennet_domain_wint(int8_t)* dom = (bennet_domain_wint(int8_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = dom->start;
        result.stop = dom->end;
        break;
      }
      case 16: {
        bennet_domain_wint(int16_t)* dom = (bennet_domain_wint(int16_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = dom->start;
        result.stop = dom->end;
        break;
      }
      case 32: {
        bennet_domain_wint(int32_t)* dom = (bennet_domain_wint(int32_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = dom->start;
        result.stop = dom->end;
        break;
      }
      case 64:
      default: {
        bennet_domain_wint(int64_t)* dom = (bennet_domain_wint(int64_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = dom->start;
        result.stop = dom->end;
        break;
      }
    }
  } else {
    switch (result.width) {
      case 8: {
        bennet_domain_wint(uint8_t)* dom = (bennet_domain_wint(uint8_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = (int64_t)dom->start;
        result.stop = (int64_t)dom->end;
        break;
      }
      case 16: {
        bennet_domain_wint(uint16_t)* dom = (bennet_domain_wint(uint16_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = (int64_t)dom->start;
        result.stop = (int64_t)dom->end;
        break;
      }
      case 32: {
        bennet_domain_wint(uint32_t)* dom = (bennet_domain_wint(uint32_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = (int64_t)dom->start;
        result.stop = (int64_t)dom->end;
        break;
      }
      case 64:
      default: {
        bennet_domain_wint(uint64_t)* dom = (bennet_domain_wint(uint64_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.start = (int64_t)dom->start;
        result.stop = (int64_t)dom->end;
        break;
      }
    }
  }

  return result;
}

/**
 * Convert a generic wrapped interval back to a tagged domain.
 */
static bennet_tagged_domain wint_to_tagged(wint_generic* g, cn_base_type* type) {
  bennet_tagged_domain result;
  result.type = type;

  int width;
  bool is_signed;
  get_type_info(type, &width, &is_signed);

  if (is_signed) {
    switch (width) {
      case 8: {
        bennet_domain_wint(int8_t)* dom = std_malloc(sizeof(bennet_domain_wint(int8_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (int8_t)g->start;
        dom->end = (int8_t)g->stop;
        result.domain = dom;
        break;
      }
      case 16: {
        bennet_domain_wint(int16_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(int16_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (int16_t)g->start;
        dom->end = (int16_t)g->stop;
        result.domain = dom;
        break;
      }
      case 32: {
        bennet_domain_wint(int32_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(int32_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (int32_t)g->start;
        dom->end = (int32_t)g->stop;
        result.domain = dom;
        break;
      }
      case 64:
      default: {
        bennet_domain_wint(int64_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(int64_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = g->start;
        dom->end = g->stop;
        result.domain = dom;
        break;
      }
    }
  } else {
    switch (width) {
      case 8: {
        bennet_domain_wint(uint8_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(uint8_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (uint8_t)g->start;
        dom->end = (uint8_t)g->stop;
        result.domain = dom;
        break;
      }
      case 16: {
        bennet_domain_wint(uint16_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(uint16_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (uint16_t)g->start;
        dom->end = (uint16_t)g->stop;
        result.domain = dom;
        break;
      }
      case 32: {
        bennet_domain_wint(uint32_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(uint32_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (uint32_t)g->start;
        dom->end = (uint32_t)g->stop;
        result.domain = dom;
        break;
      }
      case 64:
      default: {
        bennet_domain_wint(uint64_t)* dom =
            std_malloc(sizeof(bennet_domain_wint(uint64_t)));
        assert(dom);
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->start = (uint64_t)g->start;
        dom->end = (uint64_t)g->stop;
        result.domain = dom;
        break;
      }
    }
  }

  return result;
}

/**
 * Create a generic interval that represents top.
 */
static wint_generic wint_generic_top(cn_base_type* type) {
  wint_generic result = {0};
  result.is_top = true;
  result.is_bottom = false;
  get_type_info(type, &result.width, &result.is_signed);
  result.start = wint_get_min(result.is_signed, result.width);
  result.stop = wint_get_max(result.is_signed, result.width);
  return result;
}

/**
 * Create a generic interval that represents bottom.
 */
static wint_generic wint_generic_bottom(cn_base_type* type) {
  wint_generic result = {0};
  result.is_top = false;
  result.is_bottom = true;
  get_type_info(type, &result.width, &result.is_signed);
  result.start = 0;
  result.stop = 0;
  return result;
}

/**
 * Check if generic interval is top.
 */
static bool wint_generic_is_top(wint_generic* g) {
  if (g->is_top)
    return true;
  if (g->is_bottom)
    return false;
  // Also check if the interval covers the full range
  int64_t min = wint_get_min(g->is_signed, g->width);
  int64_t max = wint_get_max(g->is_signed, g->width);
  if (g->start == min && g->stop == max)
    return true;
  // Check if start == stop + 1 (mod 2^width), which means full range
  uint64_t s = wint_normalize_unsigned(g->start, g->width);
  uint64_t e = wint_normalize_unsigned(g->stop, g->width);
  if (g->width >= 64)
    return (s == e + 1);
  return (s == ((e + 1) % ((uint64_t)1 << g->width)));
}

/**
 * Meet of two generic intervals.
 */
static wint_generic wint_generic_meet(wint_generic* g1, wint_generic* g2) {
  assert(g1->width == g2->width);
  wint_generic result = {0};
  result.width = g1->width;
  result.is_signed = g1->is_signed;

  if (g1->is_bottom || g2->is_bottom) {
    result.is_bottom = true;
    return result;
  }

  if (g1->is_top) {
    return *g2;
  }

  if (g2->is_top) {
    return *g1;
  }

  // Both are non-trivial intervals
  int64_t a = g1->start, b = g1->stop;
  int64_t c = g2->start, d = g2->stop;
  int w = g1->width;

  // Check containment
  bool g1_in_g2 = wint_member(a, c, d, w) && wint_member(b, c, d, w);
  bool g2_in_g1 = wint_member(c, a, b, w) && wint_member(d, a, b, w);

  if (g1_in_g2 && g2_in_g1) {
    // Both contain each other - return the one with smaller cardinality
    uint64_t card1 = wint_cardinality(a, b, w);
    uint64_t card2 = wint_cardinality(c, d, w);
    if (card1 <= card2) {
      result.start = a;
      result.stop = b;
    } else {
      result.start = c;
      result.stop = d;
    }
  } else if (g1_in_g2) {
    result.start = a;
    result.stop = b;
  } else if (g2_in_g1) {
    result.start = c;
    result.stop = d;
  } else if (wint_member(c, a, b, w)) {
    // Overlapping: c is in [a, b]
    result.start = c;
    result.stop = b;
  } else if (wint_member(a, c, d, w)) {
    // Overlapping: a is in [c, d]
    result.start = a;
    result.stop = d;
  } else {
    // Disjoint - return bottom
    result.is_bottom = true;
  }

  return result;
}

/**
 * Join of two generic intervals.
 */
static wint_generic wint_generic_join(wint_generic* g1, wint_generic* g2) {
  assert(g1->width == g2->width);
  wint_generic result = {0};
  result.width = g1->width;
  result.is_signed = g1->is_signed;

  if (g1->is_bottom) {
    return *g2;
  }

  if (g2->is_bottom) {
    return *g1;
  }

  if (g1->is_top || g2->is_top) {
    result.is_top = true;
    result.start = wint_get_min(g1->is_signed, g1->width);
    result.stop = wint_get_max(g1->is_signed, g1->width);
    return result;
  }

  int64_t a = g1->start, b = g1->stop;
  int64_t c = g2->start, d = g2->stop;
  int w = g1->width;

  // Check containment
  bool g1_in_g2 = wint_member(a, c, d, w) && wint_member(b, c, d, w);
  bool g2_in_g1 = wint_member(c, a, b, w) && wint_member(d, a, b, w);

  if (g1_in_g2 && g2_in_g1) {
    // Both cover each other - return top
    result.is_top = true;
    result.start = wint_get_min(g1->is_signed, g1->width);
    result.stop = wint_get_max(g1->is_signed, g1->width);
  } else if (g2_in_g1) {
    result.start = a;
    result.stop = b;
  } else if (g1_in_g2) {
    result.start = c;
    result.stop = d;
  } else if (wint_member(c, a, b, w)) {
    // c is in [a, b]
    result.start = a;
    result.stop = d;
  } else if (wint_member(a, c, d, w)) {
    // a is in [c, d]
    result.start = c;
    result.stop = b;
  } else {
    // Non-overlapping - return convex hull (conservative: return top for now)
    uint64_t card_bc = wint_cardinality(b, c, w);
    uint64_t card_da = wint_cardinality(d, a, w);
    if (card_bc < card_da) {
      result.start = a;
      result.stop = d;
    } else if (card_bc > card_da) {
      result.start = c;
      result.stop = b;
    } else {
      // Tie - use lexicographic ordering
      if (wint_normalize_unsigned(a, w) <= wint_normalize_unsigned(c, w)) {
        result.start = a;
        result.stop = d;
      } else {
        result.start = c;
        result.stop = b;
      }
    }
  }

  // Check if result is now top
  if (wint_generic_is_top(&result)) {
    result.is_top = true;
  }

  return result;
}

/* Public API: Tagged Domain Functions (wint-specific) */

bool bennet_tagged_domain_is_bottom_wint(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return false;
  wint_generic g = wint_from_tagged(d);
  return g.is_bottom;
}

bool bennet_tagged_domain_is_top_wint(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return true;
  wint_generic g = wint_from_tagged(d);
  return wint_generic_is_top(&g);
}

bennet_tagged_domain bennet_tagged_domain_copy_wint(bennet_tagged_domain* d) {
  if (!d || !d->domain) {
    return bennet_tagged_domain_top_wint(d ? d->type : NULL);
  }
  wint_generic g = wint_from_tagged(d);
  return wint_to_tagged(&g, d->type);
}

bennet_tagged_domain bennet_tagged_domain_top_wint(cn_base_type* type) {
  wint_generic g = wint_generic_top(type);
  return wint_to_tagged(&g, type);
}

bennet_tagged_domain bennet_tagged_domain_bottom_wint(cn_base_type* type) {
  wint_generic g = wint_generic_bottom(type);
  return wint_to_tagged(&g, type);
}

bennet_tagged_domain bennet_tagged_domain_meet_wint(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2 && d1->type && d2->type);
  wint_generic g1 = wint_from_tagged(d1);
  wint_generic g2 = wint_from_tagged(d2);
  wint_generic result = wint_generic_meet(&g1, &g2);
  return wint_to_tagged(&result, d1->type);
}

bennet_tagged_domain bennet_tagged_domain_join_wint(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2 && d1->type && d2->type);
  wint_generic g1 = wint_from_tagged(d1);
  wint_generic g2 = wint_from_tagged(d2);
  wint_generic result = wint_generic_join(&g1, &g2);
  return wint_to_tagged(&result, d1->type);
}

/*-----------------------------------------------------------------------------
 * Abstract State Implementation (wint)
 *---------------------------------------------------------------------------*/

BENNET_ABSINT_STATE_IMPL(wint)

/*-----------------------------------------------------------------------------
 * Forward Transformer Implementation
 *---------------------------------------------------------------------------*/

/**
 * Forward transformer for constant term.
 */
static bennet_tagged_domain wint_forward_const(cn_term* term) {
  assert(term && term->type == CN_TERM_CONST);

  wint_generic g = {0};
  int width;
  bool is_signed;
  get_type_info(&term->base_type, &width, &is_signed);
  g.width = width;
  g.is_signed = is_signed;

  cn_const* c = &term->data.const_val;
  switch (c->type) {
    case CN_CONST_BITS:
      g.start = c->data.bits.value;
      g.stop = c->data.bits.value;
      break;
    case CN_CONST_Z:
      g.start = c->data.z;
      g.stop = c->data.z;
      break;
    case CN_CONST_BOOL:
      // Treat bool as 0 or 1
      g.start = c->data.boolean ? 1 : 0;
      g.stop = g.start;
      g.width = 1;
      g.is_signed = false;
      break;
    case CN_CONST_POINTER:
      g.start = (int64_t)c->data.pointer;
      g.stop = g.start;
      break;
    case CN_CONST_NULL:
      g.start = 0;
      g.stop = 0;
      break;
    default:
      // For other constants, return top
      g.is_top = true;
      g.start = wint_get_min(is_signed, width);
      g.stop = wint_get_max(is_signed, width);
      break;
  }

  return wint_to_tagged(&g, &term->base_type);
}

/**
 * Forward transformer for symbol term.
 */
static bennet_tagged_domain wint_forward_sym(cn_term* term, bennet_absint_state* state) {
  assert(term && term->type == CN_TERM_SYM);
  bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
  return bennet_absint_state_get_wint(state, sym, &term->base_type);
}

/**
 * Forward transformer for binary operations.
 */
static bennet_tagged_domain wint_forward_binop(cn_binop op,
    bennet_tagged_domain* left,
    bennet_tagged_domain* right,
    cn_base_type* result_type) {
  wint_generic g1 = wint_from_tagged(left);
  wint_generic g2 = wint_from_tagged(right);
  wint_generic result = {0};

  int width;
  bool is_signed;
  get_type_info(result_type, &width, &is_signed);
  result.width = width;
  result.is_signed = is_signed;

  if (g1.is_bottom || g2.is_bottom) {
    result.is_bottom = true;
    return wint_to_tagged(&result, result_type);
  }

  if (g1.is_top || g2.is_top) {
    // For most ops, if either operand is top, result is top
    // (Exceptions: comparison ops return boolean)
    switch (op) {
      case CN_BINOP_LT:
      case CN_BINOP_LE:
      case CN_BINOP_EQ:
      case CN_BINOP_LT_POINTER:
      case CN_BINOP_LE_POINTER:
        // Boolean result - could be true or false
        result.width = 1;
        result.is_signed = false;
        result.start = 0;
        result.stop = 1;
        return wint_to_tagged(&result, result_type);
      default:
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
        return wint_to_tagged(&result, result_type);
    }
  }

  int64_t a = g1.start, b = g1.stop;
  int64_t c = g2.start, d = g2.stop;
  int w = g1.width;

  switch (op) {
    case CN_BINOP_ADD: {
      // Use unsigned arithmetic to avoid signed overflow UB
      int64_t new_start = (int64_t)((uint64_t)a + (uint64_t)c);
      int64_t new_stop = (int64_t)((uint64_t)b + (uint64_t)d);
      uint64_t card1 = wint_cardinality(a, b, w);
      uint64_t card2 = wint_cardinality(c, d, w);
      bool overflow;
      if (w >= 64) {
        // For 64-bit widths, check if sum of cardinalities wraps around
        overflow = (card1 > UINT64_MAX - card2);
      } else {
        uint64_t max_card = (uint64_t)1 << w;
        overflow = (card1 + card2 > max_card);
      }
      if (overflow) {
        // Overflow - return top
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      } else {
        result.start = new_start;
        result.stop = new_stop;
      }
      break;
    }

    case CN_BINOP_SUB: {
      // Use unsigned arithmetic to avoid signed overflow UB
      int64_t new_start = (int64_t)((uint64_t)a - (uint64_t)d);
      int64_t new_stop = (int64_t)((uint64_t)b - (uint64_t)c);
      uint64_t card1 = wint_cardinality(a, b, w);
      uint64_t card2 = wint_cardinality(c, d, w);
      bool sub_overflow;
      if (w >= 64) {
        sub_overflow = (card1 > UINT64_MAX - card2);
      } else {
        uint64_t max_card = (uint64_t)1 << w;
        sub_overflow = (card1 + card2 > max_card);
      }
      if (sub_overflow) {
        // Overflow - return top
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      } else {
        result.start = new_start;
        result.stop = new_stop;
      }
      break;
    }

    case CN_BINOP_MUL:
    case CN_BINOP_MULNOSMT: {
      // Zero special case
      if ((a == 0 && b == 0) || (c == 0 && d == 0)) {
        result.start = 0;
        result.stop = 0;
      } else {
        // Pole split both operands and compute corner products for each pair
        int64_t s1s[WINT_MAX_SPLITS], s1e[WINT_MAX_SPLITS];
        int64_t s2s[WINT_MAX_SPLITS], s2e[WINT_MAX_SPLITS];
        int n1 = wint_pole_split(a, b, w, s1s, s1e);
        int n2 = wint_pole_split(c, d, w, s2s, s2e);

        bool first = true;
        for (int i = 0; i < n1; i++) {
          for (int j = 0; j < n2; j++) {
            // Use unsigned arithmetic to avoid signed overflow UB.
            // After pole splitting, each sub-interval doesn't cross poles, so the
            // products are meaningful as wrapped values.
            int64_t products[4] = {(int64_t)((uint64_t)s1s[i] * (uint64_t)s2s[j]),
                (int64_t)((uint64_t)s1s[i] * (uint64_t)s2e[j]),
                (int64_t)((uint64_t)s1e[i] * (uint64_t)s2s[j]),
                (int64_t)((uint64_t)s1e[i] * (uint64_t)s2e[j])};
            int64_t min_p = products[0], max_p = products[0];
            for (int k = 1; k < 4; k++) {
              if (products[k] < min_p)
                min_p = products[k];
              if (products[k] > max_p)
                max_p = products[k];
            }
            if (first) {
              result.start = min_p;
              result.stop = max_p;
              first = false;
            } else {
              wint_generic pair = {
                  .width = width, .is_signed = is_signed, .start = min_p, .stop = max_p};
              result = wint_generic_join(&result, &pair);
            }
          }
        }
      }
      break;
    }

    case CN_BINOP_DIV:
    case CN_BINOP_DIVNOSMT: {
      // Division by zero check
      if (c == 0 && d == 0) {
        result.is_bottom = true;
      } else if (a == 0 && b == 0) {
        result.start = 0;
        result.stop = 0;
      } else {
        // Pole split both operands (signed: pole_split, unsigned: south_split)
        int64_t s1s[WINT_MAX_SPLITS], s1e[WINT_MAX_SPLITS];
        int64_t s2s[WINT_MAX_SPLITS], s2e[WINT_MAX_SPLITS];
        int n1 = is_signed ? wint_pole_split(a, b, w, s1s, s1e)
                           : wint_south_split(a, b, w, s1s, s1e);
        int n2 = is_signed ? wint_pole_split(c, d, w, s2s, s2e)
                           : wint_south_split(c, d, w, s2s, s2e);

        // Purge zero intervals from divisor splits
        int n2_purged = 0;
        int64_t s2s_p[WINT_MAX_SPLITS], s2e_p[WINT_MAX_SPLITS];
        for (int j = 0; j < n2; j++) {
          if (!(s2s[j] == 0 && s2e[j] == 0)) {
            s2s_p[n2_purged] = s2s[j];
            s2e_p[n2_purged] = s2e[j];
            n2_purged++;
          }
        }

        if (n2_purged == 0) {
          result.is_bottom = true;
        } else {
          bool first = true;
          for (int i = 0; i < n1; i++) {
            for (int j = 0; j < n2_purged; j++) {
              // Skip if divisor split contains zero (avoid div-by-zero)
              if (s2s_p[j] <= 0 && s2e_p[j] >= 0)
                continue;

              int64_t divs[4] = {s1s[i] / s2s_p[j],
                  s1s[i] / s2e_p[j],
                  s1e[i] / s2s_p[j],
                  s1e[i] / s2e_p[j]};
              int64_t min_d_val = divs[0], max_d_val = divs[0];
              for (int k = 1; k < 4; k++) {
                if (divs[k] < min_d_val)
                  min_d_val = divs[k];
                if (divs[k] > max_d_val)
                  max_d_val = divs[k];
              }
              if (first) {
                result.start = min_d_val;
                result.stop = max_d_val;
                first = false;
              } else {
                wint_generic pair = {.width = width,
                    .is_signed = is_signed,
                    .start = min_d_val,
                    .stop = max_d_val};
                result = wint_generic_join(&result, &pair);
              }
            }
          }
          if (first) {
            // All divisor splits contained zero
            result.is_top = true;
            result.start = wint_get_min(is_signed, width);
            result.stop = wint_get_max(is_signed, width);
          }
        }
      }
      break;
    }

    case CN_BINOP_MOD:
    case CN_BINOP_MODNOSMT:
    case CN_BINOP_REM:
    case CN_BINOP_REMNOSMT: {
      // Remainder/modulo
      if (c == 0 && d == 0) {
        result.is_bottom = true;
      } else if (a == 0 && b == 0) {
        result.start = 0;
        result.stop = 0;
      } else if (is_signed) {
        // Signed: use MSB-based logic matching OCaml
        bool dividend_pos = wint_is_msb_zero(a, w) && wint_is_msb_zero(b, w);
        bool dividend_neg = !wint_is_msb_zero(a, w) && !wint_is_msb_zero(b, w);
        bool divisor_pos = wint_is_msb_zero(c, w) && wint_is_msb_zero(d, w);
        bool divisor_neg = !wint_is_msb_zero(c, w) && !wint_is_msb_zero(d, w);

        if (dividend_pos && divisor_pos) {
          // Both positive: [0, divisor_max-1]
          result.start = 0;
          result.stop = d - 1;
        } else if (dividend_pos && divisor_neg) {
          // Dividend positive, divisor negative: [0, -divisor_min-1]
          result.start = 0;
          result.stop = (-c) - 1;
        } else if (dividend_neg && divisor_pos) {
          // Dividend negative, divisor positive: [-divisor_max+1, 0]
          result.start = -(d) + 1;
          result.stop = 0;
        } else if (dividend_neg && divisor_neg) {
          // Both negative: [divisor_min+1, 0]
          result.start = c + 1;
          result.stop = 0;
        } else {
          // Mixed signs - conservative bounds
          int64_t abs_c = (c < 0) ? -c : c;
          int64_t abs_d = (d < 0) ? -d : d;
          int64_t max_abs = (abs_c > abs_d) ? abs_c : abs_d;
          result.start = -(max_abs - 1);
          result.stop = max_abs - 1;
        }
      } else {
        // Unsigned: south pole split and compute for each pair
        int64_t s1s[WINT_MAX_SPLITS], s1e[WINT_MAX_SPLITS];
        int64_t s2s[WINT_MAX_SPLITS], s2e[WINT_MAX_SPLITS];
        int n1 = wint_south_split(a, b, w, s1s, s1e);
        int n2 = wint_south_split(c, d, w, s2s, s2e);

        // Purge zero intervals from divisor splits
        int n2_purged = 0;
        int64_t s2s_p[WINT_MAX_SPLITS], s2e_p[WINT_MAX_SPLITS];
        for (int j = 0; j < n2; j++) {
          if (!(s2s[j] == 0 && s2e[j] == 0)) {
            s2s_p[n2_purged] = s2s[j];
            s2e_p[n2_purged] = s2e[j];
            n2_purged++;
          }
        }
        (void)s2s_p;

        if (n2_purged == 0) {
          result.is_bottom = true;
        } else {
          bool first = true;
          for (int i = 0; i < n1; i++) {
            for (int j = 0; j < n2_purged; j++) {
              int64_t lb = 0;
              int64_t ub = s2e_p[j] - 1;
              if (first) {
                result.start = lb;
                result.stop = ub;
                first = false;
              } else {
                wint_generic pair = {
                    .width = width, .is_signed = is_signed, .start = lb, .stop = ub};
                result = wint_generic_join(&result, &pair);
              }
            }
          }
        }
      }
      break;
    }

    case CN_BINOP_BW_AND: {
      // South pole split both operands for precise Hacker's Delight bounds
      int64_t s1s[WINT_MAX_SPLITS], s1e[WINT_MAX_SPLITS];
      int64_t s2s[WINT_MAX_SPLITS], s2e[WINT_MAX_SPLITS];
      int n1 = wint_south_split(a, b, w, s1s, s1e);
      int n2 = wint_south_split(c, d, w, s2s, s2e);

      bool first = true;
      for (int i = 0; i < n1; i++) {
        for (int j = 0; j < n2; j++) {
          uint64_t ua = wint_normalize_unsigned(s1s[i], w);
          uint64_t ub = wint_normalize_unsigned(s1e[i], w);
          uint64_t uc = wint_normalize_unsigned(s2s[j], w);
          uint64_t ud = wint_normalize_unsigned(s2e[j], w);
          uint64_t lo = wint_min_and(ua, ub, uc, ud, w);
          uint64_t hi = wint_max_and(ua, ub, uc, ud, w);
          if (first) {
            result.start = (int64_t)lo;
            result.stop = (int64_t)hi;
            first = false;
          } else {
            wint_generic pair = {.width = width,
                .is_signed = is_signed,
                .start = (int64_t)lo,
                .stop = (int64_t)hi};
            result = wint_generic_join(&result, &pair);
          }
        }
      }
      break;
    }

    case CN_BINOP_BW_OR: {
      // South pole split both operands for precise Hacker's Delight bounds
      int64_t s1s[WINT_MAX_SPLITS], s1e[WINT_MAX_SPLITS];
      int64_t s2s[WINT_MAX_SPLITS], s2e[WINT_MAX_SPLITS];
      int n1 = wint_south_split(a, b, w, s1s, s1e);
      int n2 = wint_south_split(c, d, w, s2s, s2e);

      bool first = true;
      for (int i = 0; i < n1; i++) {
        for (int j = 0; j < n2; j++) {
          uint64_t ua = wint_normalize_unsigned(s1s[i], w);
          uint64_t ub = wint_normalize_unsigned(s1e[i], w);
          uint64_t uc = wint_normalize_unsigned(s2s[j], w);
          uint64_t ud = wint_normalize_unsigned(s2e[j], w);
          uint64_t lo = wint_min_or(ua, ub, uc, ud, w);
          uint64_t hi = wint_max_or(ua, ub, uc, ud, w);
          if (first) {
            result.start = (int64_t)lo;
            result.stop = (int64_t)hi;
            first = false;
          } else {
            wint_generic pair = {.width = width,
                .is_signed = is_signed,
                .start = (int64_t)lo,
                .stop = (int64_t)hi};
            result = wint_generic_join(&result, &pair);
          }
        }
      }
      break;
    }

    case CN_BINOP_BW_XOR: {
      // South pole split both operands for precise Hacker's Delight bounds
      int64_t s1s[WINT_MAX_SPLITS], s1e[WINT_MAX_SPLITS];
      int64_t s2s[WINT_MAX_SPLITS], s2e[WINT_MAX_SPLITS];
      int n1 = wint_south_split(a, b, w, s1s, s1e);
      int n2 = wint_south_split(c, d, w, s2s, s2e);

      bool first = true;
      for (int i = 0; i < n1; i++) {
        for (int j = 0; j < n2; j++) {
          uint64_t ua = wint_normalize_unsigned(s1s[i], w);
          uint64_t ub = wint_normalize_unsigned(s1e[i], w);
          uint64_t uc = wint_normalize_unsigned(s2s[j], w);
          uint64_t ud = wint_normalize_unsigned(s2e[j], w);
          uint64_t lo = wint_min_xor(ua, ub, uc, ud, w);
          uint64_t hi = wint_max_xor(ua, ub, uc, ud, w);
          if (first) {
            result.start = (int64_t)lo;
            result.stop = (int64_t)hi;
            first = false;
          } else {
            wint_generic pair = {.width = width,
                .is_signed = is_signed,
                .start = (int64_t)lo,
                .stop = (int64_t)hi};
            result = wint_generic_join(&result, &pair);
          }
        }
      }
      break;
    }

    case CN_BINOP_SHIFT_LEFT: {
      // Shift amount must be constant for precise result
      if (c == d && c >= 0 && c < w) {
        int64_t k = c;
        int num_bits_survive = w - (int)k;
        if (num_bits_survive <= 0) {
          result.start = 0;
          result.stop = 0;
        } else {
          // Check if lower bits fit: truncate operand to surviving bits
          uint64_t trunc_mask = (num_bits_survive >= 64)
                                    ? UINT64_MAX
                                    : (((uint64_t)1 << num_bits_survive) - 1);
          uint64_t trunc_start = (uint64_t)a & trunc_mask;
          uint64_t trunc_stop = (uint64_t)b & trunc_mask;
          // If truncation doesn't wrap, precise shift is safe
          if (trunc_start <= trunc_stop || wint_crosses_south(a, b, num_bits_survive)) {
            // Use unsigned shift to avoid UB on negative or overflowing values
            result.start = (int64_t)((uint64_t)a << k);
            result.stop = (int64_t)((uint64_t)b << k);
          } else {
            // Truncation wraps - conservative bounds
            int64_t max_val = (int64_t)(trunc_mask << k);
            result.start = 0;
            result.stop = max_val;
          }
        }
      } else {
        // Non-constant or out-of-range shift
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      }
      break;
    }

    case CN_BINOP_SHIFT_RIGHT: {
      // Shift amount must be constant for precise result
      if (c == d && c >= 0 && c < w) {
        int64_t k = c;
        if (is_signed) {
          // Check for north pole crossing
          bool crosses_north = wint_member(wint_get_max(true, w), a, b, w) &&
                               wint_member(wint_get_min(true, w), a, b, w);
          if (crosses_north) {
            // Conservative bounds from Crab:
            // lb: k leading 1's followed by (w-k) 0's
            // ub: k leading 0's followed by (w-k) 1's
            int remaining = w - (int)k;
            int64_t lb = remaining > 0 ? (int64_t)((uint64_t)(((uint64_t)1 << (int)k) - 1)
                                                   << remaining)
                                       : 0;
            // Sign-extend lb for the full width
            if (width < 64) {
              int64_t sign_bit = (int64_t)1 << (width - 1);
              if (lb & sign_bit)
                lb |= ~(((int64_t)1 << width) - 1);
            }
            int64_t ub = remaining > 0 ? ((int64_t)1 << remaining) - 1 : 0;
            result.start = lb;
            result.stop = ub;
          } else {
            // No pole crossing - precise arithmetic shift
            result.start = a >> k;
            result.stop = b >> k;
          }
        } else {
          // Unsigned: check for south pole crossing
          if (wint_crosses_south(a, b, w)) {
            int remaining = w - (int)k;
            result.start = 0;
            result.stop = remaining > 0 ? ((int64_t)1 << remaining) - 1 : 0;
          } else {
            // Logical shift
            result.start = (int64_t)((uint64_t)a >> k);
            result.stop = (int64_t)((uint64_t)b >> k);
          }
        }
      } else {
        // Non-constant or out-of-range shift
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      }
      break;
    }

    case CN_BINOP_LT:
    case CN_BINOP_LT_POINTER: {
      // Boolean result
      result.width = 1;
      result.is_signed = false;
      // Use wrapped membership for proper comparison
      // For non-wrapped intervals, use direct comparison; for wrapped, be conservative
      if (!wint_crosses_south(a, b, w) && !wint_crosses_south(c, d, w)) {
        // Use unsigned comparison for unsigned types and pointer comparisons
        bool use_unsigned = !g1.is_signed || op == CN_BINOP_LT_POINTER;
        if (use_unsigned) {
          uint64_t ub = wint_normalize_unsigned(b, w);
          uint64_t uc = wint_normalize_unsigned(c, w);
          uint64_t ua = wint_normalize_unsigned(a, w);
          uint64_t ud = wint_normalize_unsigned(d, w);
          if (ub < uc) {
            result.start = 1;
            result.stop = 1;
          } else if (ua >= ud) {
            result.start = 0;
            result.stop = 0;
          } else {
            result.start = 0;
            result.stop = 1;
          }
        } else {
          if (b < c) {
            result.start = 1;
            result.stop = 1;
          } else if (a >= d) {
            result.start = 0;
            result.stop = 0;
          } else {
            result.start = 0;
            result.stop = 1;
          }
        }
      } else {
        // Wrapped interval - could be either
        result.start = 0;
        result.stop = 1;
      }
      break;
    }

    case CN_BINOP_LE:
    case CN_BINOP_LE_POINTER: {
      // Boolean result
      result.width = 1;
      result.is_signed = false;
      if (!wint_crosses_south(a, b, w) && !wint_crosses_south(c, d, w)) {
        bool use_unsigned = !g1.is_signed || op == CN_BINOP_LE_POINTER;
        if (use_unsigned) {
          uint64_t ub = wint_normalize_unsigned(b, w);
          uint64_t uc = wint_normalize_unsigned(c, w);
          uint64_t ua = wint_normalize_unsigned(a, w);
          uint64_t ud = wint_normalize_unsigned(d, w);
          if (ub <= uc) {
            result.start = 1;
            result.stop = 1;
          } else if (ua > ud) {
            result.start = 0;
            result.stop = 0;
          } else {
            result.start = 0;
            result.stop = 1;
          }
        } else {
          if (b <= c) {
            result.start = 1;
            result.stop = 1;
          } else if (a > d) {
            result.start = 0;
            result.stop = 0;
          } else {
            result.start = 0;
            result.stop = 1;
          }
        }
      } else {
        result.start = 0;
        result.stop = 1;
      }
      break;
    }

    case CN_BINOP_EQ: {
      // Boolean result
      result.width = 1;
      result.is_signed = false;
      if (a == b && c == d && a == c) {
        // Both are singleton and equal
        result.start = 1;
        result.stop = 1;
      } else if (!wint_crosses_south(a, b, w) && !wint_crosses_south(c, d, w)) {
        // Check disjointness using the correct comparison for the type
        bool disjoint;
        if (g1.is_signed) {
          disjoint = (b < c || d < a);
        } else {
          uint64_t ub = wint_normalize_unsigned(b, w);
          uint64_t uc = wint_normalize_unsigned(c, w);
          uint64_t ua = wint_normalize_unsigned(a, w);
          uint64_t ud = wint_normalize_unsigned(d, w);
          disjoint = (ub < uc || ud < ua);
        }
        if (disjoint) {
          result.start = 0;
          result.stop = 0;
        } else {
          result.start = 0;
          result.stop = 1;
        }
      } else {
        // Could be either (including wrapped cases)
        result.start = 0;
        result.stop = 1;
      }
      break;
    }

    case CN_BINOP_MIN: {
      // MIN only works precisely for non-wrapped intervals
      if (wint_crosses_south(a, b, w) || wint_crosses_south(c, d, w)) {
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      } else {
        int64_t min_val = (a < c) ? a : c;
        int64_t max_val = (b < d) ? b : d;
        result.start = min_val;
        result.stop = max_val;
      }
      break;
    }

    case CN_BINOP_MAX: {
      // MAX only works precisely for non-wrapped intervals
      if (wint_crosses_south(a, b, w) || wint_crosses_south(c, d, w)) {
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      } else {
        int64_t min_val = (a > c) ? a : c;
        int64_t max_val = (b > d) ? b : d;
        result.start = min_val;
        result.stop = max_val;
      }
      break;
    }

    default:
      // Unsupported operation - return top
      result.is_top = true;
      result.start = wint_get_min(is_signed, width);
      result.stop = wint_get_max(is_signed, width);
      break;
  }

  return wint_to_tagged(&result, result_type);
}

/**
 * Forward transformer for unary operations.
 */
static bennet_tagged_domain wint_forward_unop(
    cn_unop op, bennet_tagged_domain* operand, cn_base_type* result_type) {
  wint_generic g = wint_from_tagged(operand);
  wint_generic result = {0};

  int width;
  bool is_signed;
  get_type_info(result_type, &width, &is_signed);
  result.width = width;
  result.is_signed = is_signed;

  if (g.is_bottom) {
    result.is_bottom = true;
    return wint_to_tagged(&result, result_type);
  }

  if (g.is_top) {
    result.is_top = true;
    result.start = wint_get_min(is_signed, width);
    result.stop = wint_get_max(is_signed, width);
    return wint_to_tagged(&result, result_type);
  }

  switch (op) {
    case CN_UNOP_NOT: {
      // Logical NOT (for boolean)
      if (g.start == 0 && g.stop == 0) {
        result.start = 1;
        result.stop = 1;
      } else if (g.start == 1 && g.stop == 1) {
        result.start = 0;
        result.stop = 0;
      } else {
        result.start = 0;
        result.stop = 1;
      }
      result.width = 1;
      result.is_signed = false;
      break;
    }

    case CN_UNOP_NEGATE: {
      // Unary minus: -x
      result.start = -g.stop;
      result.stop = -g.start;
      // Check for overflow at MIN_INT
      int64_t min_val = wint_get_min(true, g.width);
      if (g.start == min_val) {
        // -MIN_INT overflows
        result.is_top = true;
        result.start = wint_get_min(is_signed, width);
        result.stop = wint_get_max(is_signed, width);
      }
      break;
    }

    case CN_UNOP_BW_COMPL: {
      // Bitwise NOT: ~x = -x - 1
      result.start = -(g.stop + 1);
      result.stop = -(g.start + 1);
      break;
    }

    default:
      // Unsupported unary op - return top
      result.is_top = true;
      result.start = wint_get_min(is_signed, width);
      result.stop = wint_get_max(is_signed, width);
      break;
  }

  return wint_to_tagged(&result, result_type);
}

/**
 * Forward transformer - main entry point.
 */
bennet_tagged_domain bennet_wint_transform_forward(
    cn_term* term, bennet_absint_state* state) {
  if (!term) {
    // Return top for null term
    cn_base_type bt = cn_base_type_bits(false, 64);
    return bennet_tagged_domain_top_wint(&bt);
  }

  switch (term->type) {
    case CN_TERM_CONST:
      return wint_forward_const(term);

    case CN_TERM_SYM:
      return wint_forward_sym(term, state);

    case CN_TERM_UNOP: {
      bennet_tagged_domain operand_dom =
          bennet_wint_transform_forward(term->data.unop.operand, state);
      return wint_forward_unop(term->data.unop.op, &operand_dom, &term->base_type);
    }

    case CN_TERM_BINOP: {
      bennet_tagged_domain left_dom =
          bennet_wint_transform_forward(term->data.binop.left, state);
      bennet_tagged_domain right_dom =
          bennet_wint_transform_forward(term->data.binop.right, state);
      return wint_forward_binop(
          term->data.binop.op, &left_dom, &right_dom, &term->base_type);
    }

    case CN_TERM_ITE: {
      bennet_tagged_domain then_dom =
          bennet_wint_transform_forward(term->data.ite.then_term, state);
      bennet_tagged_domain else_dom =
          bennet_wint_transform_forward(term->data.ite.else_term, state);
      return bennet_tagged_domain_join_wint(&then_dom, &else_dom);
    }

    case CN_TERM_CAST: {
      /* Recursively evaluate the inner term */
      bennet_tagged_domain src_dom =
          bennet_wint_transform_forward(term->data.cast.value, state);

      /* Source and destination type metadata */
      int src_width, dst_width;
      bool src_signed, dst_signed;
      get_type_info(src_dom.type, &src_width, &src_signed);
      get_type_info(&term->base_type, &dst_width, &dst_signed);

      wint_generic src = wint_from_tagged(&src_dom);

      /* Bottom propagates */
      if (src.is_bottom) {
        wint_generic bot = wint_generic_bottom(&term->base_type);
        return wint_to_tagged(&bot, &term->base_type);
      }

      if (src_width == dst_width) {
        /* Same-width: just change type metadata */
        src.is_signed = dst_signed;
        src.width = dst_width;
        return wint_to_tagged(&src, &term->base_type);
      } else if (src_width > dst_width) {
        /* Truncation */
        if (src.is_top) {
          return bennet_tagged_domain_top_wint(&term->base_type);
        }
        /* If cardinality >= 2^dst_width the truncated range covers everything */
        uint64_t card = wint_cardinality(src.start, src.stop, src_width);
        if (dst_width < 64 && card >= ((uint64_t)1 << dst_width)) {
          return bennet_tagged_domain_top_wint(&term->base_type);
        }
        /* Mask both bounds to dst_width bits */
        uint64_t mask = (dst_width >= 64) ? UINT64_MAX : (((uint64_t)1 << dst_width) - 1);
        int64_t new_start = (int64_t)((uint64_t)src.start & mask);
        int64_t new_stop = (int64_t)((uint64_t)src.stop & mask);
        wint_generic result = {.is_top = false,
            .is_bottom = false,
            .is_signed = dst_signed,
            .width = dst_width,
            .start = new_start,
            .stop = new_stop};
        return wint_to_tagged(&result, &term->base_type);
      } else {
        /* Extension (src_width < dst_width) */
        if (src_signed) {
          /* Sign extension: if sign bit set, fill upper bits with 1s */
          int64_t sign_bit = (int64_t)1 << (src_width - 1);
          int64_t upper_ones = (int64_t)(UINT64_MAX << src_width);
          int64_t new_start =
              (src.start & sign_bit) ? (src.start | upper_ones) : src.start;
          int64_t new_stop = (src.stop & sign_bit) ? (src.stop | upper_ones) : src.stop;
          wint_generic result = {.is_top = false,
              .is_bottom = false,
              .is_signed = dst_signed,
              .width = dst_width,
              .start = new_start,
              .stop = new_stop};
          return wint_to_tagged(&result, &term->base_type);
        } else {
          /* Zero extension: upper bits are implicitly 0, preserve interval */
          src.is_signed = dst_signed;
          src.width = dst_width;
          return wint_to_tagged(&src, &term->base_type);
        }
      }
    }

    case CN_TERM_ARRAY_SHIFT: {
      bennet_tagged_domain base_dom =
          bennet_wint_transform_forward(term->data.array_shift.base, state);
      bennet_tagged_domain index_dom =
          bennet_wint_transform_forward(term->data.array_shift.index, state);

      // Create constant domain for element_size
      wint_generic elem_size_g = {0};
      int idx_width;
      bool idx_signed;
      get_type_info(index_dom.type, &idx_width, &idx_signed);
      elem_size_g.width = idx_width;
      elem_size_g.is_signed = idx_signed;
      elem_size_g.start = (int64_t)term->data.array_shift.element_size;
      elem_size_g.stop = (int64_t)term->data.array_shift.element_size;
      bennet_tagged_domain elem_size_dom = wint_to_tagged(&elem_size_g, index_dom.type);

      // index * element_size
      bennet_tagged_domain offset_dom =
          wint_forward_binop(CN_BINOP_MUL, &index_dom, &elem_size_dom, index_dom.type);

      // base + offset
      return wint_forward_binop(CN_BINOP_ADD, &base_dom, &offset_dom, &term->base_type);
    }

    case CN_TERM_MEMBER_SHIFT: {
      bennet_tagged_domain base_dom =
          bennet_wint_transform_forward(term->data.member_shift.base, state);

      // Create constant domain for offset
      wint_generic offset_g = {0};
      int base_width;
      bool base_signed;
      get_type_info(base_dom.type, &base_width, &base_signed);
      offset_g.width = base_width;
      offset_g.is_signed = base_signed;
      offset_g.start = (int64_t)term->data.member_shift.offset;
      offset_g.stop = (int64_t)term->data.member_shift.offset;
      bennet_tagged_domain offset_dom = wint_to_tagged(&offset_g, base_dom.type);

      // base + offset
      return wint_forward_binop(CN_BINOP_ADD, &base_dom, &offset_dom, &term->base_type);
    }

    default:
      // For unsupported term types, return top
      return bennet_tagged_domain_top_wint(&term->base_type);
  }
}

/**
 * Collect all sym IDs from a term into a fixed-size buffer.
 * Returns the number of syms found (up to max_syms).
 */
static int term_collect_syms(cn_term* term, bennet_absint_sym* syms, int max_syms) {
  if (!term || max_syms <= 0) {
    return 0;
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      syms[0] = (bennet_absint_sym){.name = term->data.sym.name, .id = term->data.sym.id};
      return 1;
    }
    case CN_TERM_UNOP:
      return term_collect_syms(term->data.unop.operand, syms, max_syms);
    case CN_TERM_BINOP: {
      int n = term_collect_syms(term->data.binop.left, syms, max_syms);
      n += term_collect_syms(term->data.binop.right, syms + n, max_syms - n);
      return n;
    }
    case CN_TERM_CAST:
      return term_collect_syms(term->data.cast.value, syms, max_syms);
    case CN_TERM_ITE: {
      int n = term_collect_syms(term->data.ite.then_term, syms, max_syms);
      n += term_collect_syms(term->data.ite.else_term, syms + n, max_syms - n);
      return n;
    }
    case CN_TERM_ARRAY_SHIFT: {
      int n = term_collect_syms(term->data.array_shift.base, syms, max_syms);
      n += term_collect_syms(term->data.array_shift.index, syms + n, max_syms - n);
      return n;
    }
    case CN_TERM_MEMBER_SHIFT:
      return term_collect_syms(term->data.member_shift.base, syms, max_syms);
    default:
      return 0;
  }
}

/**
 * Apply a refined domain to all SYMs in a term by calling transform_backward
 * for each SYM found. Accumulates refinements into the state.
 */
static bennet_absint_state* backward_apply_to_all_syms(
    cn_term* term, bennet_tagged_domain* refined_dom, bennet_absint_state* state) {
  bennet_absint_sym syms[16];
  int n = term_collect_syms(term, syms, 16);

  bennet_absint_state* result = state;
  for (int i = 0; i < n; i++) {
    result = bennet_wint_transform_backward(term, syms[i], *refined_dom, result);
  }
  return result;
}

/*-----------------------------------------------------------------------------
 * Backward Transformer Implementation
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_wint_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state) {
  if (!term || !state) {
    return state;
  }

  // Check if output domain is bottom - if so, whole state is bottom
  if (bennet_tagged_domain_is_bottom_wint(&output_domain)) {
    bennet_absint_state* result = bennet_absint_state_copy_wint(state);
    // Mark the state as bottom by setting target_sym to bottom
    result = bennet_absint_state_set_wint(
        result, target_sym, bennet_tagged_domain_bottom_wint(&term->base_type));
    return result;
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      // If this symbol is our target, meet with output domain
      if (term->data.sym.id == target_sym.id) {
        return bennet_absint_state_meet_wint(state, target_sym, output_domain);
      }
      return bennet_absint_state_copy_wint(state);
    }

    case CN_TERM_BINOP: {
      cn_term* left = term->data.binop.left;
      cn_term* right = term->data.binop.right;
      bool left_has_target = term_contains_sym(left, target_sym.id);
      bool right_has_target = term_contains_sym(right, target_sym.id);

      if (!left_has_target && !right_has_target) {
        return bennet_absint_state_copy_wint(state);
      }

      // For comparison operations, handle specially
      switch (term->data.binop.op) {
        case CN_BINOP_EQ:
        case CN_BINOP_LT:
        case CN_BINOP_LE:
        case CN_BINOP_LT_POINTER:
        case CN_BINOP_LE_POINTER: {
          // These are handled by backward_assume
          return bennet_absint_state_copy_wint(state);
        }
        default:
          break;
      }

      // For arithmetic operations, invert the operation to propagate backward.
      // Given output domain [L, R] for op(a, b), compute the domain for the
      // operand containing the target sym.
      {
        wint_generic out = wint_from_tagged(&output_domain);
        cn_term* other = left_has_target ? right : left;
        cn_term* target_side = left_has_target ? left : right;
        bennet_tagged_domain other_dom = bennet_wint_transform_forward(other, state);
        wint_generic og = wint_from_tagged(&other_dom);

        // Only invert when other side is non-top (we have concrete bounds)
        if (!out.is_top && !og.is_top) {
          wint_generic inverted = {.width = out.width, .is_signed = out.is_signed};
          switch (term->data.binop.op) {
            case CN_BINOP_ADD:
              // out = target + other => target = out - other
              if (left_has_target) {
                inverted.start = out.start - og.stop;
                inverted.stop = out.stop - og.start;
              } else {
                inverted.start = out.start - og.stop;
                inverted.stop = out.stop - og.start;
              }
              break;
            case CN_BINOP_SUB:
              if (left_has_target) {
                // out = target - other => target = out + other
                inverted.start = out.start + og.start;
                inverted.stop = out.stop + og.stop;
              } else {
                // out = other - target => target = other - out
                inverted.start = og.start - out.stop;
                inverted.stop = og.stop - out.start;
              }
              break;
            default:
              // For non-invertible ops (MOD, etc.), don't propagate
              return bennet_absint_state_copy_wint(state);
          }
          bennet_tagged_domain inv_dom =
              wint_to_tagged(&inverted, &target_side->base_type);
          return bennet_wint_transform_backward(target_side, target_sym, inv_dom, state);
        }

        // Fallback: propagate output unchanged for ADD/SUB only
        switch (term->data.binop.op) {
          case CN_BINOP_ADD:
          case CN_BINOP_SUB:
            return bennet_wint_transform_backward(
                target_side, target_sym, output_domain, state);
          default:
            return bennet_absint_state_copy_wint(state);
        }
      }
    }

    case CN_TERM_UNOP: {
      // Propagate to operand
      return bennet_wint_transform_backward(
          term->data.unop.operand, target_sym, output_domain, state);
    }

    case CN_TERM_ITE: {
      // Propagate to both branches and join results
      bennet_absint_state* then_state = bennet_wint_transform_backward(
          term->data.ite.then_term, target_sym, output_domain, state);
      bennet_absint_state* else_state = bennet_wint_transform_backward(
          term->data.ite.else_term, target_sym, output_domain, state);

      // Join the states
      if (bennet_absint_state_is_bottom_wint(then_state)) {
        return else_state;
      }
      if (bennet_absint_state_is_bottom_wint(else_state)) {
        return then_state;
      }

      // Get domain for target from both states and join
      bennet_tagged_domain then_dom =
          bennet_absint_state_get_wint(then_state, target_sym, &term->base_type);
      bennet_tagged_domain else_dom =
          bennet_absint_state_get_wint(else_state, target_sym, &term->base_type);
      bennet_tagged_domain joined = bennet_tagged_domain_join_wint(&then_dom, &else_dom);
      return bennet_absint_state_set_wint(state, target_sym, joined);
    }

    case CN_TERM_ARRAY_SHIFT: {
      cn_term* base = term->data.array_shift.base;
      cn_term* index = term->data.array_shift.index;
      bool base_has_target = term_contains_sym(base, target_sym.id);
      bool index_has_target = term_contains_sym(index, target_sym.id);

      if (!base_has_target && !index_has_target) {
        return bennet_absint_state_copy_wint(state);
      }

      wint_generic out = wint_from_tagged(&output_domain);
      int64_t elem_size = (int64_t)term->data.array_shift.element_size;

      if (base_has_target) {
        if (!out.is_top && !out.is_bottom) {
          // result = base + index * elem_size => base = result - index * elem_size
          bennet_tagged_domain index_dom = bennet_wint_transform_forward(index, state);
          wint_generic index_g = wint_from_tagged(&index_dom);

          if (!index_g.is_top) {
            // Create constant elem_size domain with index's type info
            int idx_width;
            bool idx_signed;
            get_type_info(&index->base_type, &idx_width, &idx_signed);
            wint_generic elem_g = {.width = idx_width,
                .is_signed = idx_signed,
                .start = elem_size,
                .stop = elem_size};
            bennet_tagged_domain elem_dom = wint_to_tagged(&elem_g, &index->base_type);

            // offset = index * elem_size
            bennet_tagged_domain offset_dom =
                wint_forward_binop(CN_BINOP_MUL, &index_dom, &elem_dom, &term->base_type);
            wint_generic offset_g = wint_from_tagged(&offset_dom);

            if (!offset_g.is_top) {
              // base = result - offset
              wint_generic inverted = {.width = out.width,
                  .is_signed = out.is_signed,
                  .start = out.start - offset_g.stop,
                  .stop = out.stop - offset_g.start};
              bennet_tagged_domain inv_dom = wint_to_tagged(&inverted, &base->base_type);
              return bennet_wint_transform_backward(base, target_sym, inv_dom, state);
            }
          }
        }
        // Fallback: propagate output unchanged
        return bennet_wint_transform_backward(base, target_sym, output_domain, state);
      }

      if (index_has_target) {
        if (!out.is_top && !out.is_bottom) {
          // result = base + index * elem_size => index = (result - base) / elem_size
          bennet_tagged_domain base_dom = bennet_wint_transform_forward(base, state);
          wint_generic base_g = wint_from_tagged(&base_dom);

          if (!base_g.is_top && elem_size != 0) {
            // diff = result - base (in index's type)
            bennet_tagged_domain out_as_idx = wint_to_tagged(&out, &index->base_type);
            bennet_tagged_domain base_as_idx = wint_to_tagged(&base_g, &index->base_type);
            bennet_tagged_domain diff_dom = wint_forward_binop(
                CN_BINOP_SUB, &out_as_idx, &base_as_idx, &index->base_type);
            wint_generic diff_g = wint_from_tagged(&diff_dom);

            if (!diff_g.is_top) {
              // Create constant elem_size domain
              int idx_width;
              bool idx_signed;
              get_type_info(&index->base_type, &idx_width, &idx_signed);
              wint_generic elem_g = {.width = idx_width,
                  .is_signed = idx_signed,
                  .start = elem_size,
                  .stop = elem_size};
              bennet_tagged_domain elem_dom = wint_to_tagged(&elem_g, &index->base_type);

              // refined_index = diff / elem_size
              bennet_tagged_domain refined_dom = wint_forward_binop(
                  CN_BINOP_DIV, &diff_dom, &elem_dom, &index->base_type);
              wint_generic refined_g = wint_from_tagged(&refined_dom);

              if (!refined_g.is_top) {
                // Meet with current index domain
                bennet_tagged_domain cur_index_dom =
                    bennet_wint_transform_forward(index, state);
                wint_generic cur_index_g = wint_from_tagged(&cur_index_dom);
                wint_generic met = wint_generic_meet(&cur_index_g, &refined_g);
                bennet_tagged_domain met_dom = wint_to_tagged(&met, &index->base_type);
                return bennet_wint_transform_backward(index, target_sym, met_dom, state);
              }
            }
          }
        }
        // Fallback: propagate output unchanged
        return bennet_wint_transform_backward(index, target_sym, output_domain, state);
      }

      return bennet_absint_state_copy_wint(state);
    }

    case CN_TERM_MEMBER_SHIFT: {
      cn_term* base = term->data.member_shift.base;
      if (!term_contains_sym(base, target_sym.id)) {
        return bennet_absint_state_copy_wint(state);
      }

      wint_generic out = wint_from_tagged(&output_domain);
      if (!out.is_top && !out.is_bottom) {
        // result = base + offset => base = result - offset
        int64_t offset = (int64_t)term->data.member_shift.offset;
        wint_generic inverted = {.width = out.width,
            .is_signed = out.is_signed,
            .start = out.start - offset,
            .stop = out.stop - offset};
        bennet_tagged_domain inv_dom = wint_to_tagged(&inverted, &base->base_type);
        return bennet_wint_transform_backward(base, target_sym, inv_dom, state);
      }

      // If output is top or bottom, propagate unchanged
      return bennet_wint_transform_backward(base, target_sym, output_domain, state);
    }

    case CN_TERM_CAST: {
      cn_term* inner = term->data.cast.value;
      if (!term_contains_sym(inner, target_sym.id)) {
        return bennet_absint_state_copy_wint(state);
      }
      // Clamp output domain to source type range and meet, then recurse
      int src_width, dst_width;
      bool src_signed, dst_signed;
      get_type_info(&inner->base_type, &src_width, &src_signed);
      get_type_info(&term->base_type, &dst_width, &dst_signed);

      wint_generic out = wint_from_tagged(&output_domain);
      if (!out.is_top && !out.is_bottom) {
        int64_t src_min = wint_get_min(src_signed, src_width);
        int64_t src_max = wint_get_max(src_signed, src_width);

        if (src_width < dst_width) {
          // Widening cast: intersect in destination (wider) width to avoid
          // mod-2^src_width wrap-around corruption of 64-bit interval bounds
          wint_generic clamped_wide = {.width = dst_width,
              .is_signed = dst_signed,
              .start = src_min,
              .stop = src_max};
          wint_generic refined_wide = wint_generic_meet(&out, &clamped_wide);
          if (refined_wide.is_bottom) {
            bennet_tagged_domain bot_dom =
                wint_to_tagged(&refined_wide, &inner->base_type);
            return bennet_wint_transform_backward(inner, target_sym, bot_dom, state);
          }
          // Result values are within [src_min, src_max], safe to narrow width
          wint_generic refined = {.width = src_width,
              .is_signed = src_signed,
              .start = refined_wide.start,
              .stop = refined_wide.stop};
          bennet_tagged_domain refined_dom = wint_to_tagged(&refined, &inner->base_type);
          return bennet_wint_transform_backward(inner, target_sym, refined_dom, state);
        }

        // Same-width or narrowing cast: original logic (values fit in src_width)
        wint_generic clamped = {.width = src_width,
            .is_signed = src_signed,
            .start = src_min,
            .stop = src_max};
        wint_generic out_in_src = {.width = src_width,
            .is_signed = src_signed,
            .start = out.start,
            .stop = out.stop};
        wint_generic refined = wint_generic_meet(&out_in_src, &clamped);
        bennet_tagged_domain refined_dom = wint_to_tagged(&refined, &inner->base_type);
        return bennet_wint_transform_backward(inner, target_sym, refined_dom, state);
      }
      // If output is top or bottom, just propagate as-is
      bennet_tagged_domain inner_out = wint_to_tagged(&out, &inner->base_type);
      return bennet_wint_transform_backward(inner, target_sym, inner_out, state);
    }

    default:
      return bennet_absint_state_copy_wint(state);
  }
}

/**
 * Backward transformer for boolean constraint.
 */
bennet_absint_state* bennet_wint_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state) {
  if (!term || !state) {
    return state;
  }

  // Handle NOT(expr) by recursing with flipped value
  if (term->type == CN_TERM_UNOP && term->data.unop.op == CN_UNOP_NOT) {
    return bennet_wint_transform_backward_assume(term->data.unop.operand, !value, state);
  }

  // Handle comparison operators
  if (term->type == CN_TERM_BINOP) {
    cn_term* left = term->data.binop.left;
    cn_term* right = term->data.binop.right;
    cn_binop op = term->data.binop.op;

    // Get current domains
    bennet_tagged_domain left_dom = bennet_wint_transform_forward(left, state);
    bennet_tagged_domain right_dom = bennet_wint_transform_forward(right, state);

    wint_generic lg = wint_from_tagged(&left_dom);
    wint_generic rg = wint_from_tagged(&right_dom);
    wint_generic lg_refined = lg;
    wint_generic rg_refined = rg;

    int width = lg.width;

    switch (op) {
      case CN_BINOP_EQ: {
        if (value) {
          // a == b must be true: meet both domains
          wint_generic meet = wint_generic_meet(&lg, &rg);
          lg_refined = meet;
          rg_refined = meet;
        } else {
          // a != b must be true: if one is constant at boundary, remove from other
          if (lg.start == lg.stop) {
            // Left is constant
            int64_t c = lg.start;
            if (rg.start == c) {
              rg_refined.start = c + 1;
              rg_refined.is_top = false;
            } else if (rg.stop == c) {
              rg_refined.stop = c - 1;
              rg_refined.is_top = false;
            }
          } else if (rg.start == rg.stop) {
            // Right is constant
            int64_t c = rg.start;
            if (lg.start == c) {
              lg_refined.start = c + 1;
              lg_refined.is_top = false;
            } else if (lg.stop == c) {
              lg_refined.stop = c - 1;
              lg_refined.is_top = false;
            }
          }
        }
        break;
      }

      case CN_BINOP_LE:
      case CN_BINOP_LE_POINTER: {
        bool is_signed = lg.is_signed;
        if (value) {
          // a <= b must be true
          // Refine left upper bound: [min, b.stop]
          if (!rg.is_top) {
            if (lg.is_top) {
              lg_refined.start = wint_get_min(is_signed, width);
              lg_refined.stop = rg.stop;
              lg_refined.is_top = false;
            } else {
              lg_refined.stop = (lg_refined.stop < rg.stop) ? lg_refined.stop : rg.stop;
            }
          }
          // Refine right lower bound: [a.start, max]
          if (!lg.is_top) {
            if (rg.is_top) {
              rg_refined.start = lg.start;
              rg_refined.stop = wint_get_max(is_signed, width);
              rg_refined.is_top = false;
            } else {
              rg_refined.start =
                  (rg_refined.start > lg.start) ? rg_refined.start : lg.start;
            }
          }
        } else {
          // a > b must be true
          // Refine left lower bound: [b.start + 1, max]
          if (!rg.is_top) {
            int64_t new_lg_start = rg.start + 1;
            if (lg.is_top) {
              lg_refined.start = new_lg_start;
              lg_refined.stop = wint_get_max(is_signed, width);
              lg_refined.is_top = false;
            } else if (new_lg_start > lg_refined.start) {
              lg_refined.start = new_lg_start;
            }
          }
          // Refine right upper bound: [min, a.stop - 1]
          if (!lg.is_top) {
            int64_t new_rg_stop = lg.stop - 1;
            if (rg.is_top) {
              rg_refined.start = wint_get_min(is_signed, width);
              rg_refined.stop = new_rg_stop;
              rg_refined.is_top = false;
            } else if (new_rg_stop < rg_refined.stop) {
              rg_refined.stop = new_rg_stop;
            }
          }
        }
        break;
      }

      case CN_BINOP_LT:
      case CN_BINOP_LT_POINTER: {
        bool is_signed = lg.is_signed;
        if (value) {
          // a < b must be true
          // Refine left upper bound: [min, b.stop - 1]
          if (!rg.is_top) {
            int64_t new_lg_stop = rg.stop - 1;
            if (lg.is_top) {
              lg_refined.start = wint_get_min(is_signed, width);
              lg_refined.stop = new_lg_stop;
              lg_refined.is_top = false;
            } else if (new_lg_stop < lg_refined.stop) {
              lg_refined.stop = new_lg_stop;
            }
          }
          // Refine right lower bound: [a.start + 1, max]
          if (!lg.is_top) {
            int64_t new_rg_start = lg.start + 1;
            if (rg.is_top) {
              rg_refined.start = new_rg_start;
              rg_refined.stop = wint_get_max(is_signed, width);
              rg_refined.is_top = false;
            } else if (new_rg_start > rg_refined.start) {
              rg_refined.start = new_rg_start;
            }
          }
        } else {
          // a >= b must be true
          // Refine left lower bound: [b.start, max]
          if (!rg.is_top) {
            if (lg.is_top) {
              lg_refined.start = rg.start;
              lg_refined.stop = wint_get_max(is_signed, width);
              lg_refined.is_top = false;
            } else if (rg.start > lg_refined.start) {
              lg_refined.start = rg.start;
            }
          }
          // Refine right upper bound: [min, a.stop]
          if (!lg.is_top) {
            if (rg.is_top) {
              rg_refined.start = wint_get_min(is_signed, width);
              rg_refined.stop = lg.stop;
              rg_refined.is_top = false;
            } else if (lg.stop < rg_refined.stop) {
              rg_refined.stop = lg.stop;
            }
          }
        }
        break;
      }

      default:
        // Other binary operations - no refinement
        return bennet_absint_state_copy_wint(state);
    }

    // Check for empty intervals using unsigned comparison.
    // Use the ORIGINAL interval for the wrapping check: if the original
    // didn't wrap but refinement made start > stop, the result is empty.
    {
      uint64_t lg_start_u = wint_normalize_unsigned(lg_refined.start, width);
      uint64_t lg_stop_u = wint_normalize_unsigned(lg_refined.stop, width);
      if (lg_start_u > lg_stop_u && !wint_crosses_south(lg.start, lg.stop, width)) {
        lg_refined.is_bottom = true;
      }
    }
    {
      uint64_t rg_start_u = wint_normalize_unsigned(rg_refined.start, width);
      uint64_t rg_stop_u = wint_normalize_unsigned(rg_refined.stop, width);
      if (rg_start_u > rg_stop_u && !wint_crosses_south(rg.start, rg.stop, width)) {
        rg_refined.is_bottom = true;
      }
    }

    // If either refined interval is bottom, the constraint is unsatisfiable.
    // Propagate bottom to all syms in the term to make the state bottom.
    if (lg_refined.is_bottom || rg_refined.is_bottom) {
      bennet_absint_state* bot_state = bennet_absint_state_copy_wint(state);
      bennet_absint_sym all_syms[16];
      int nl = term_collect_syms(left, all_syms, 16);
      int nr = term_collect_syms(right, all_syms + nl, 16 - nl);
      for (int i = 0; i < nl + nr; i++) {
        cn_base_type loc_bt = {.tag = CN_BASE_LOC};
        bot_state = bennet_absint_state_set_wint(
            bot_state, all_syms[i], bennet_tagged_domain_bottom_wint(&loc_bt));
      }
      return bot_state;
    }

    // Apply refinements by propagating backward through the term structure.
    // This handles SYM, CAST(SYM), ADD(CONST, SYM), etc.
    bennet_absint_state* result = bennet_absint_state_copy_wint(state);

    if (!lg_refined.is_top) {
      bennet_tagged_domain lg_dom = wint_to_tagged(&lg_refined,
          left->base_type.tag == CN_BASE_LOC ? &left->base_type : left_dom.type);
      result = backward_apply_to_all_syms(left, &lg_dom, result);
    }

    if (!rg_refined.is_top) {
      bennet_tagged_domain rg_dom = wint_to_tagged(&rg_refined,
          right->base_type.tag == CN_BASE_LOC ? &right->base_type : right_dom.type);
      result = backward_apply_to_all_syms(right, &rg_dom, result);
    }

    return result;
  }

  // Handle AND
  if (term->type == CN_TERM_BINOP && term->data.binop.op == CN_BINOP_AND) {
    if (value) {
      // Both sides must be true
      bennet_absint_state* result =
          bennet_wint_transform_backward_assume(term->data.binop.left, true, state);
      return bennet_wint_transform_backward_assume(term->data.binop.right, true, result);
    }
    // At least one side must be false - can't refine much
    return bennet_absint_state_copy_wint(state);
  }

  // Handle OR
  if (term->type == CN_TERM_BINOP && term->data.binop.op == CN_BINOP_OR) {
    if (!value) {
      // Both sides must be false
      bennet_absint_state* result =
          bennet_wint_transform_backward_assume(term->data.binop.left, false, state);
      return bennet_wint_transform_backward_assume(term->data.binop.right, false, result);
    }
    // At least one side must be true - can't refine much
    return bennet_absint_state_copy_wint(state);
  }

  return bennet_absint_state_copy_wint(state);
}

/*-----------------------------------------------------------------------------
 * Comparison Refinement Helper
 *---------------------------------------------------------------------------*/

void bennet_wint_transform_refine_comparison(bennet_absint_binop op,
    bool must_be_true,
    bennet_tagged_domain* left_domain,
    bennet_tagged_domain* right_domain,
    bennet_tagged_domain* out_left,
    bennet_tagged_domain* out_right) {
  assert(left_domain && right_domain && out_left && out_right);

  wint_generic lg = wint_from_tagged(left_domain);
  wint_generic rg = wint_from_tagged(right_domain);
  wint_generic lg_refined = lg;
  wint_generic rg_refined = rg;

  int width = lg.width;

  switch (op) {
    case BENNET_ABSINT_BINOP_EQ: {
      if (must_be_true) {
        wint_generic meet = wint_generic_meet(&lg, &rg);
        lg_refined = meet;
        rg_refined = meet;
      } else {
        // For inequality, refine if one is constant at boundary
        if (lg.start == lg.stop) {
          int64_t c = lg.start;
          if (rg.start == c) {
            rg_refined.start = c + 1;
          } else if (rg.stop == c) {
            rg_refined.stop = c - 1;
          }
        } else if (rg.start == rg.stop) {
          int64_t c = rg.start;
          if (lg.start == c) {
            lg_refined.start = c + 1;
          } else if (lg.stop == c) {
            lg_refined.stop = c - 1;
          }
        }
      }
      break;
    }

    case BENNET_ABSINT_BINOP_LE: {
      if (must_be_true) {
        // a <= b: constrain a.stop <= b.stop, b.start >= a.start
        if (!lg.is_top && !rg.is_top) {
          if (rg.stop < lg_refined.stop) {
            lg_refined.stop = rg.stop;
          }
          if (lg.start > rg_refined.start) {
            rg_refined.start = lg.start;
          }
        }
      } else {
        // a > b: constrain a.start > b.start, b.stop < a.stop
        if (!lg.is_top && !rg.is_top) {
          if (rg.start + 1 > lg_refined.start) {
            lg_refined.start = rg.start + 1;
          }
          if (lg.stop - 1 < rg_refined.stop) {
            rg_refined.stop = lg.stop - 1;
          }
        }
      }
      break;
    }

    case BENNET_ABSINT_BINOP_LT: {
      if (must_be_true) {
        // a < b: constrain a.stop < b.stop, b.start > a.start
        if (!lg.is_top && !rg.is_top) {
          if (rg.stop - 1 < lg_refined.stop) {
            lg_refined.stop = rg.stop - 1;
          }
          if (lg.start + 1 > rg_refined.start) {
            rg_refined.start = lg.start + 1;
          }
        }
      } else {
        // a >= b: constrain a.start >= b.start, b.stop <= a.stop
        if (!lg.is_top && !rg.is_top) {
          if (rg.start > lg_refined.start) {
            lg_refined.start = rg.start;
          }
          if (lg.stop < rg_refined.stop) {
            rg_refined.stop = lg.stop;
          }
        }
      }
      break;
    }
  }

  // Check for empty intervals
  if (lg_refined.start > lg_refined.stop &&
      !wint_crosses_south(lg_refined.start, lg_refined.stop, width)) {
    lg_refined.is_bottom = true;
  }
  if (rg_refined.start > rg_refined.stop &&
      !wint_crosses_south(rg_refined.start, rg_refined.stop, width)) {
    rg_refined.is_bottom = true;
  }

  *out_left = wint_to_tagged(&lg_refined, left_domain->type);
  *out_right = wint_to_tagged(&rg_refined, right_domain->type);
}
