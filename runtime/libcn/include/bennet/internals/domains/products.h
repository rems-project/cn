
#ifndef BENNET_DOMAINS_PRODUCTS_H
#define BENNET_DOMAINS_PRODUCTS_H

#include <bennet/internals/domains/congr.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/domains/wint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(cty)                                         \
  cty bennet_domain_ownership_wint_arbitrary_##cty(                                      \
      bennet_domain_ownership(cty)*, bennet_domain_wint(cty)*);

BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uintptr_t)

#define BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(cty)                                  \
  void bennet_domain_ownership_wint_reduce_##cty(                                        \
      bennet_domain_ownership(cty)*, bennet_domain_wint(cty)*);

BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(int8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(int16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(int32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(int64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(uint8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(uint16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(uint32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(uint64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_REDUCE_DECLS(uintptr_t)

/* congr_ownership combined arbitrary + reduce */

#define BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(cty)                               \
  cty bennet_domain_congr_ownership_arbitrary_##cty(                                     \
      bennet_domain_congr(cty)*, bennet_domain_ownership(cty)*);

BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_DECLS(uintptr_t)

#define BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(cty)                                  \
  void bennet_domain_congr_ownership_reduce_##cty(                                       \
      bennet_domain_congr(cty)*, bennet_domain_ownership(cty)*);

BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_DECLS(uintptr_t)

/* congr_wint combined arbitrary + reduce */

#define BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(cty)                                    \
  cty bennet_domain_congr_wint_arbitrary_##cty(                                          \
      bennet_domain_congr(cty)*, bennet_domain_wint(cty)*);

BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(int8_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(int16_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(int32_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(int64_t)

BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(uint8_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(uint16_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(uint32_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(uint64_t)

BENNET_DOMAIN_CONGR_WINT_ARBITRARY_DECLS(uintptr_t)

#define BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(cty)                                       \
  void bennet_domain_congr_wint_reduce_##cty(                                            \
      bennet_domain_congr(cty)*, bennet_domain_wint(cty)*);

BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(int8_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(int16_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(int32_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(int64_t)

BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(uint8_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(uint16_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(uint32_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(uint64_t)

BENNET_DOMAIN_CONGR_WINT_REDUCE_DECLS(uintptr_t)

/* congr_ownership_wint combined arbitrary + reduce */

#define BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(cty)                          \
  cty bennet_domain_congr_ownership_wint_arbitrary_##cty(bennet_domain_congr(cty)*,      \
      bennet_domain_ownership(cty)*,                                                     \
      bennet_domain_wint(cty)*);

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_DECLS(uintptr_t)

#define BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(cty)                             \
  void bennet_domain_congr_ownership_wint_reduce_##cty(bennet_domain_congr(cty)*,        \
      bennet_domain_ownership(cty)*,                                                     \
      bennet_domain_wint(cty)*);

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_DECLS(uintptr_t)

/* ownership_tnum combined arbitrary + reduce */

#define BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(cty)                                \
  cty bennet_domain_ownership_tnum_arbitrary_##cty(                                      \
      bennet_domain_ownership(cty)*, bennet_domain_tnum(cty)*);

BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(int8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(int16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(int32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(int64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(uint8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(uint16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(uint32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(uint64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_DECLS(uintptr_t)

#define BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(cty)                                   \
  void bennet_domain_ownership_tnum_reduce_##cty(                                        \
      bennet_domain_ownership(cty)*, bennet_domain_tnum(cty)*);

BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(int8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(int16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(int32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(int64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(uint8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(uint16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(uint32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(uint64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_DECLS(uintptr_t)

#ifdef __cplusplus
}
#endif

#endif /* BENNET_DOMAINS_PRODUCTS_H */
