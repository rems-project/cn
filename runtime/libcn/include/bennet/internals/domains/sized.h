#ifndef BENNET_DOMAINS_SIZED_H
#define BENNET_DOMAINS_SIZED_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_sized(cty) struct bennet_domain_sized_##cty

#define bennet_domain_sized_top(cty) ((bennet_domain_sized(cty)){.size = 0})

#define BENNET_DOMAIN_SIZED_DECL(cty)                                                    \
  bennet_domain_sized(cty) {                                                             \
    cty size;                                                                            \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_sized_##cty(bennet_domain_sized(cty)*);                           \
                                                                                         \
  static inline cty bennet_arbitrary_sized_##cty##_top(void) {                           \
    bennet_domain_sized(cty) d = bennet_domain_sized_top(cty);                           \
    return bennet_arbitrary_sized_##cty(&d);                                             \
  }

#define bennet_arbitrary_sized_top(cty) (bennet_arbitrary_sized_##cty##_top())

#define bennet_arbitrary_sized(cty, sz)                                                  \
  ({                                                                                     \
    bennet_domain_sized(cty) bennet_arbitrary_sized_tmp =                                \
        (bennet_domain_sized(cty)){.size = sz};                                          \
    bennet_arbitrary_sized_##cty(&bennet_arbitrary_sized_tmp);                           \
  })

BENNET_DOMAIN_SIZED_DECL(uint8_t)
BENNET_DOMAIN_SIZED_DECL(uint16_t)
BENNET_DOMAIN_SIZED_DECL(uint32_t)
BENNET_DOMAIN_SIZED_DECL(uint64_t)

BENNET_DOMAIN_SIZED_DECL(int8_t)
BENNET_DOMAIN_SIZED_DECL(int16_t)
BENNET_DOMAIN_SIZED_DECL(int32_t)
BENNET_DOMAIN_SIZED_DECL(int64_t)

BENNET_DOMAIN_SIZED_DECL(uintptr_t)

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_SIZED_H
