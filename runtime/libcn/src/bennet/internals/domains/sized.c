#include <assert.h>

#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>

#define SIZED_GEN(sm)                                                                    \
  uint##sm##_t bennet_arbitrary_sized_uint##sm##_t(                                      \
      bennet_domain_sized(uint##sm##_t) * d) {                                           \
    uint##sm##_t s = d->size;                                                            \
    size_t sz = bennet_get_size();                                                       \
    if (s != 0 && s < sz) {                                                              \
      sz = s;                                                                            \
    }                                                                                    \
                                                                                         \
    /* FIXME: Unsound, should move elsewhere */                                          \
    if (sz <= 8 * sizeof(size_t)) {                                                      \
      size_t extremes_likelihood = 1 << (sz / 2 + 1);                                    \
      if (!bennet_uniform_uint64_t(extremes_likelihood)) {                               \
        return (s == 0) ? UINT##sm##_MAX : (s - 1);                                      \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    return bennet_uniform_uint##sm##_t(sz);                                              \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_arbitrary_sized_int##sm##_t(bennet_domain_sized(int##sm##_t) * d) { \
    int##sm##_t s = d->size;                                                             \
    assert(s >= 0);                                                                      \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
    if (s != 0 && s < sz) {                                                              \
      sz = s;                                                                            \
    }                                                                                    \
                                                                                         \
    /* FIXME: Unsound, should move elsewhere */                                          \
    if (sz <= 8 * sizeof(size_t)) {                                                      \
      size_t extremes_likelihood = 1 << (sz / 2 + 1);                                    \
      if (!bennet_uniform_uint64_t(extremes_likelihood)) {                               \
        switch (bennet_uniform_uint8_t(2)) {                                             \
          case 0:                                                                        \
            return (s == 0) ? INT##sm##_MIN : -(s - 1);                                  \
          case 1:                                                                        \
            return (s == 0) ? INT##sm##_MAX : (s - 1);                                   \
          default:                                                                       \
            assert(0);                                                                   \
        }                                                                                \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    return bennet_uniform_int##sm##_t(sz);                                               \
  }

SIZED_GEN(8);
SIZED_GEN(16);
SIZED_GEN(32);
SIZED_GEN(64);

uintptr_t bennet_arbitrary_sized_uintptr_t(bennet_domain_sized(uintptr_t) * d) {
  uintptr_t s = d->size;
  size_t sz = bennet_get_size();
  if (s != 0 && s < sz) {
    sz = s;
  }

  /* FIXME: Unsound, should move elsewhere */
  if (sz <= 8 * sizeof(size_t)) {
    size_t extremes_likelihood = 1 << (sz / 2 + 1);
    if (!bennet_uniform_uint64_t(extremes_likelihood)) {
      return (s == 0) ? UINTPTR_MAX : (s - 1);
    }
  }

  return bennet_uniform_uintptr_t(sz);
}
