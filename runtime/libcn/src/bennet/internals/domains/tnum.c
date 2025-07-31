#include <assert.h>
#include <limits.h>

#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>

#define TNUM_GEN(sm)                                                                     \
  static int count_ones_##sm(uint##sm##_t n) {                                           \
    int count = 0;                                                                       \
                                                                                         \
    for (int i = 0; i < sm; i++) {                                                       \
      if (((n >> i) & 1)) {                                                              \
        count++;                                                                         \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    return count;                                                                        \
  }                                                                                      \
                                                                                         \
  uint##sm##_t bennet_arbitrary_tnum_uint##sm##_t(                                       \
      bennet_domain_tnum(uint##sm##_t) * d) {                                            \
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
      if (d->mask & (1 << i)) {                                                          \
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
    if (d->mask & sign) {                                                                \
      end <<= 1;                                                                         \
      end |= 1;                                                                          \
    }                                                                                    \
                                                                                         \
    uint##sm##_t bits = bennet_range_uint##sm##_t(0, end);                               \
    uint##sm##_t res = d->value;                                                         \
    /* If unknown sign */                                                                \
                                                                                         \
    if (d->mask & sign) {                                                                \
      if (bits & 1) {                                                                    \
        res |= sign;                                                                     \
      }                                                                                  \
      bits >>= 1;                                                                        \
    }                                                                                    \
                                                                                         \
    for (uint##sm##_t i = 0; i < sm && bits != 0; i++) {                                 \
      uint##sm##_t shift = (d->value & sign) ? (sm - 1 - i) : i;                         \
      if (d->mask & (1 << shift)) {                                                      \
        res |= (bits & 1) << shift;                                                      \
        bits >>= 1;                                                                      \
      }                                                                                  \
    }                                                                                    \
    assert(bits == 0);                                                                   \
                                                                                         \
    return res;                                                                          \
  }

TNUM_GEN(8);
TNUM_GEN(16);
TNUM_GEN(32);
TNUM_GEN(64);
