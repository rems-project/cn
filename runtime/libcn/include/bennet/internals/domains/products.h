
#ifndef BENNET_DOMAINS_PRODUCTS_H
#define BENNET_DOMAINS_PRODUCTS_H

#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/wint.h>

#define BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(cty)                                         \
  cty bennet_domain_ownership_wint_arbitrary_##cty(                                      \
      bennet_domain_ownership(cty) *, bennet_domain_wint(cty) *);

BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(int64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uint64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_DECLS(uintptr_t)

#define bennet_arbitrary_ownership_wint(cty, d)                                          \
  (bennet_arbitrary_ownership_wint_##cty##(d))

#endif /* BENNET_DOMAINS_PRODUCTS_H */
