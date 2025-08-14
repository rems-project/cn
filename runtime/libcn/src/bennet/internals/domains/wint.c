#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils.h>

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
      start -= (end == INT##sm##_MAX);                                                   \
    }                                                                                    \
                                                                                         \
    int32_t sz = (int32_t)bennet_get_size();                                             \
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
          int32_t excess = (sz - end);                                                   \
          if (start < -sz + 1 - excess) {                                                \
            start = -sz + 1 - excess;                                                    \
          }                                                                              \
        }                                                                                \
      } else {                                                                           \
        int32_t excess = start - (-sz + 1);                                              \
        if (end > sz - 1 + excess) {                                                     \
          end = sz - 1 + excess;                                                         \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      assert(end - start < 2 * sz);                                                      \
    }                                                                                    \
                                                                                         \
    size_t width = end - start + 1;                                                      \
    if (width >= 2 * sz) {                                                               \
      width = 2 * sz - 1;                                                                \
    }                                                                                    \
                                                                                         \
    uint##sm##_t res = bennet_uniform_uint##sm##_t(width);                               \
                                                                                         \
    if (offset) {                                                                        \
      res += (end == INT##sm##_MAX);                                                     \
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
    bennet_domain_wint(cty)* result = malloc(sizeof(bennet_domain_wint(cty)));                           \
    result->top = true;                                                                                  \
    result->bottom = false;                                                                              \
    result->start = BV_MIN(cty);                                                                         \
    result->end = BV_MAX(cty);                                                                           \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_bottom_##cty(void) {                                      \
    bennet_domain_wint(cty)* result = malloc(sizeof(bennet_domain_wint(cty)));                           \
    result->top = false;                                                                                 \
    result->bottom = true;                                                                               \
    result->start = 0;                                                                                   \
    result->end = 0;                                                                                     \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_of_##cty(cty start, cty end) {                            \
    bennet_domain_wint(cty)* result = malloc(sizeof(bennet_domain_wint(cty)));                           \
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
    bennet_domain_wint(cty)* result = malloc(sizeof(bennet_domain_wint(cty)));                           \
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
    bennet_domain_wint(cty)* result = malloc(sizeof(bennet_domain_wint(cty)));                           \
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
    /* For simplicity, conservative meet: if one contains the other, return the smaller */               \
    if (bennet_domain_wint_leq_##cty(d1, d2)) {                                                          \
      *result = *d1;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    if (bennet_domain_wint_leq_##cty(d2, d1)) {                                                          \
      *result = *d2;                                                                                     \
      return result;                                                                                     \
    }                                                                                                    \
                                                                                                         \
    /* Conservative: return bottom for complex cases */                                                  \
    result->top = false;                                                                                 \
    result->bottom = true;                                                                               \
    result->start = 0;                                                                                   \
    result->end = 0;                                                                                     \
    return result;                                                                                       \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_copy_##cty(bennet_domain_wint(cty) * d) {                 \
    bennet_domain_wint(cty)* result = malloc(sizeof(bennet_domain_wint(cty)));                           \
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
    if (d->start < d->end) {                                                                             \
      return d->start <= v && v <= d->end;                                                               \
    }                                                                                                    \
                                                                                                         \
    return d->start <= v || v <= d->end;                                                                 \
  }                                                                                                      \
                                                                                                         \
  bennet_domain_wint(cty) * bennet_domain_wint_from_assignment_##cty(                                    \
                                void* base_ptr, void* addr, size_t bytes) {                              \
    /* Conservative: return top for any assignment */                                                    \
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
