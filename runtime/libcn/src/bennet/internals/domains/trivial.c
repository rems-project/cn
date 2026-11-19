#include <assert.h>

#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/trivial.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>

#define TRIVIAL_GEN(sm)                                                                  \
  uint##sm##_t bennet_arbitrary_trivial_uint##sm##_t(                                    \
      bennet_domain_trivial(uint##sm##_t) * d) {                                         \
    return bennet_arbitrary_sized(uint##sm##_t, 0);                                      \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_arbitrary_trivial_int##sm##_t(                                      \
      bennet_domain_trivial(int##sm##_t) * d) {                                          \
    return bennet_arbitrary_sized(int##sm##_t, 0);                                       \
  }

TRIVIAL_GEN(8);
TRIVIAL_GEN(16);
TRIVIAL_GEN(32);
TRIVIAL_GEN(64);
