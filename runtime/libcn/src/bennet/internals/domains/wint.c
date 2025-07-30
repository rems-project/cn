#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>

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
