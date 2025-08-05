#ifndef BENNET_DOMAINS_OWNERSHIP_H
#define BENNET_DOMAINS_OWNERSHIP_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_ownership(cty) struct bennet_domain_ownership_##cty

#define bennet_domain_ownership_top(cty)                                                 \
  ((bennet_domain_ownership(cty)){.bottom = 0, .before = 0, .after = 0})

#define bennet_domain_ownership_bottom(cty)                                              \
  ((bennet_domain_ownership(cty)){.bottom = 1, .before = 0, .after = 0})

#define BENNET_DOMAIN_OWNERSHIP_DECL(cty)                                                \
  bennet_domain_ownership(cty) {                                                         \
    bool bottom;                                                                         \
    size_t before;                                                                       \
    size_t after;                                                                        \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_ownership_##cty(bennet_domain_ownership(cty)*);                   \
                                                                                         \
  static inline cty bennet_arbitrary_ownership_##cty##_top(void) {                       \
    bennet_domain_ownership(cty) d = bennet_domain_ownership_top(cty);                   \
    return bennet_arbitrary_ownership_##cty(&d);                                         \
  }                                                                                      \
                                                                                         \
  static inline cty bennet_arbitrary_ownership_##cty##_bottom(void) {                    \
    bennet_domain_ownership(cty) d = bennet_domain_ownership_bottom(cty);                \
    return bennet_arbitrary_ownership_##cty(&d);                                         \
  }

#define bennet_arbitrary_ownership_top(cty) (bennet_arbitrary_ownership_##cty##_top())

#define bennet_arbitrary_ownership_bottom(cty)                                           \
  (bennet_arbitrary_ownership_##cty##_bottom())

#define bennet_arbitrary_ownership(cty, before, after)                                   \
  ({                                                                                     \
    bennet_domain_ownership(cty) bennet_arbitrary_ownership_tmp =                        \
        (bennet_domain_ownership(cty)){.before = before, .after = after};                \
    bennet_arbitrary_ownership_##cty(&bennet_arbitrary_ownership_tmp);                   \
  })

BENNET_DOMAIN_OWNERSHIP_DECL(uint8_t)
BENNET_DOMAIN_OWNERSHIP_DECL(uint16_t)
BENNET_DOMAIN_OWNERSHIP_DECL(uint32_t)
BENNET_DOMAIN_OWNERSHIP_DECL(uint64_t)

BENNET_DOMAIN_OWNERSHIP_DECL(int8_t)
BENNET_DOMAIN_OWNERSHIP_DECL(int16_t)
BENNET_DOMAIN_OWNERSHIP_DECL(int32_t)
BENNET_DOMAIN_OWNERSHIP_DECL(int64_t)

BENNET_DOMAIN_OWNERSHIP_DECL(uintptr_t)

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_OWNERSHIP_H
