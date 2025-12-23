#ifndef BENNET_DOMAINS_TNUM_H
#define BENNET_DOMAINS_TNUM_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_tnum(cty) struct bennet_domain_tnum_##cty

#define BENNET_DOMAIN_TNUM_DECL(cty)                                                     \
  bennet_domain_tnum(cty) {                                                              \
    cty value;                                                                           \
    cty mask;                                                                            \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_tnum_##cty(bennet_domain_tnum(cty)*);

BENNET_DOMAIN_TNUM_DECL(uint8_t)
BENNET_DOMAIN_TNUM_DECL(uint16_t)
BENNET_DOMAIN_TNUM_DECL(uint32_t)
BENNET_DOMAIN_TNUM_DECL(uint64_t)

BENNET_DOMAIN_TNUM_DECL(int8_t)
BENNET_DOMAIN_TNUM_DECL(int16_t)
BENNET_DOMAIN_TNUM_DECL(int32_t)
BENNET_DOMAIN_TNUM_DECL(int64_t)

#define bennet_domain_tnum_default(cty) (bennet_domain_tnum_default_##cty())

#define bennet_arbitrary_tnum_default(cty) bennet_arbitrary_tnum_##cty##_default()

#define bennet_arbitrary_tnum(cty, v, m)                                                 \
  ({                                                                                     \
    bennet_domain_tnum(cty) bennet_arbitrary_tnum_tmp =                                  \
        (bennet_domain_tnum(cty)){.value = v, .mask = m};                                \
    bennet_arbitrary_tnum_##cty(&bennet_arbitrary_tnum_tmp);                             \
  })

#define BENNET_DOMAIN_TNUM_DEFAULT_IMPL(bits)                                            \
  static inline bennet_domain_tnum(uint##bits##_t)                                       \
      bennet_domain_tnum_default_uint##bits##_t(void) {                                  \
    return (bennet_domain_tnum(uint##bits##_t)){.value = 0, .mask = UINT##bits##_MAX};   \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_tnum(int##bits##_t)                                        \
      bennet_domain_tnum_default_int##bits##_t(void) {                                   \
    return (bennet_domain_tnum(int##bits##_t)){                                          \
        .value = 0, .mask = (int##bits##_t)UINT##bits##_MAX};                            \
  }                                                                                      \
                                                                                         \
  static inline uint##bits##_t bennet_arbitrary_tnum_uint##bits##_t_default(void) {      \
    bennet_domain_tnum(uint##bits##_t) d = bennet_domain_tnum_default(uint##bits##_t);   \
    return bennet_arbitrary_tnum_uint##bits##_t(&d);                                     \
  }

BENNET_DOMAIN_TNUM_DEFAULT_IMPL(8)
BENNET_DOMAIN_TNUM_DEFAULT_IMPL(16)
BENNET_DOMAIN_TNUM_DEFAULT_IMPL(32)
BENNET_DOMAIN_TNUM_DEFAULT_IMPL(64)

#undef BENNET_DOMAIN_TNUM_DEFAULT_IMPL

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_TNUM_H
