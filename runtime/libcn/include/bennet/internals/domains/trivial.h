#ifndef BENNET_DOMAINS_TRIVIAL_H
#define BENNET_DOMAINS_TRIVIAL_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_trivial(cty) struct bennet_domain_trivial_##cty

#define bennet_domain_trivial_top(cty) ((bennet_domain_trivial(cty)){})

#define BENNET_DOMAIN_TRIVIAL_DECL(cty)                                                  \
  bennet_domain_trivial(cty) {                                                           \
    char unused;                                                                         \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_trivial_##cty(bennet_domain_trivial(cty)*);                       \
                                                                                         \
  static inline cty bennet_arbitrary_trivial_##cty##_top(void) {                         \
    bennet_domain_trivial(cty) d = bennet_domain_trivial_top(cty);                       \
    return bennet_arbitrary_trivial_##cty(&d);                                           \
  }

#define bennet_arbitrary_trivial_top(cty) (bennet_arbitrary_trivial_##cty##_top())

#define bennet_arbitrary_trivial(cty) (bennet_arbitrary_trivial_top(cty))

BENNET_DOMAIN_TRIVIAL_DECL(uint8_t)
BENNET_DOMAIN_TRIVIAL_DECL(uint16_t)
BENNET_DOMAIN_TRIVIAL_DECL(uint32_t)
BENNET_DOMAIN_TRIVIAL_DECL(uint64_t)

BENNET_DOMAIN_TRIVIAL_DECL(int8_t)
BENNET_DOMAIN_TRIVIAL_DECL(int16_t)
BENNET_DOMAIN_TRIVIAL_DECL(int32_t)
BENNET_DOMAIN_TRIVIAL_DECL(int64_t)

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_TRIVIAL_H
