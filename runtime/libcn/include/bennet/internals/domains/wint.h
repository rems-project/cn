#ifndef BENNET_DOMAINS_WINT_H
#define BENNET_DOMAINS_WINT_H

#include <stdbool.h>
#include <stdint.h>

#include <bennet/internals/domains/wint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_wint(cty) struct bennet_domain_wint_##cty

#define bennet_domain_wint_top(cty)                                                      \
  ((bennet_domain_wint(cty)){.top = true, .bottom = false})

#define BENNET_DOMAIN_WINT_DECL(cty)                                                     \
  bennet_domain_wint(cty) {                                                              \
    bool top;                                                                            \
    bool bottom;                                                                         \
    cty start;                                                                           \
    cty end;                                                                             \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_wint_##cty(bennet_domain_wint(cty)*);                             \
                                                                                         \
  static inline cty bennet_arbitrary_wint_##cty##_top(void) {                            \
    bennet_domain_wint(cty) d = bennet_domain_wint_top(cty);                             \
    return bennet_arbitrary_wint_##cty(&d);                                              \
  }

#define bennet_arbitrary_wint_top(cty) bennet_arbitrary_wint_##cty##_top()

#define bennet_arbitrary_wint(cty, s, e)                                                 \
  ({                                                                                     \
    ((e) - (s) == 1) ? bennet_arbitrary_wint_top(cty) : ({                               \
      bennet_domain_wint(cty) bennet_arbitrary_wint_tmp = (bennet_domain_wint(cty)){     \
          .top = false, .bottom = false, .start = s, .end = e};                          \
      bennet_arbitrary_wint_##cty(&bennet_arbitrary_wint_tmp);                           \
    });                                                                                  \
  })

BENNET_DOMAIN_WINT_DECL(uint8_t)
BENNET_DOMAIN_WINT_DECL(uint16_t)
BENNET_DOMAIN_WINT_DECL(uint32_t)
BENNET_DOMAIN_WINT_DECL(uint64_t)

BENNET_DOMAIN_WINT_DECL(int8_t)
BENNET_DOMAIN_WINT_DECL(int16_t)
BENNET_DOMAIN_WINT_DECL(int32_t)
BENNET_DOMAIN_WINT_DECL(int64_t)

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_WINT_H
