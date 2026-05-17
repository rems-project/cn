#ifndef BENNET_DOMAINS_TNUM_H
#define BENNET_DOMAINS_TNUM_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_tnum(cty) struct bennet_domain_tnum_##cty

#define bennet_domain_tnum_top(cty)    (bennet_domain_tnum_top_##cty())
#define bennet_domain_tnum_bottom(cty) (bennet_domain_tnum_bottom_##cty())

#define BENNET_DOMAIN_TNUM_DECL(cty)                                                     \
  bennet_domain_tnum(cty) {                                                              \
    bool top;                                                                            \
    bool bottom;                                                                         \
    cty value;                                                                           \
    cty mask;                                                                            \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_tnum_##cty(bennet_domain_tnum(cty)*);                             \
  bennet_domain_tnum(cty) * bennet_domain_tnum_top_##cty(void);                          \
  bennet_domain_tnum(cty) * bennet_domain_tnum_bottom_##cty(void);                       \
  bennet_domain_tnum(cty) * bennet_domain_tnum_of_##cty(cty value, cty mask);            \
  bool bennet_domain_tnum_is_top_##cty(bennet_domain_tnum(cty)*);                        \
  bool bennet_domain_tnum_is_bottom_##cty(bennet_domain_tnum(cty)*);                     \
                                                                                         \
  bool bennet_domain_tnum_leq_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*); \
  bool bennet_domain_tnum_equal_##cty(                                                   \
      bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);                               \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_join_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*); \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_meet_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*); \
  bennet_domain_tnum(cty) * bennet_domain_tnum_copy_##cty(bennet_domain_tnum(cty)*);     \
                                                                                         \
  cty bennet_domain_tnum_arbitrary_##cty(bennet_domain_tnum(cty)*);                      \
  bool bennet_domain_tnum_check_##cty(cty, bennet_domain_tnum(cty)*);                    \
                                                                                         \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_from_assignment_##cty(void*, void*, size_t);                    \
                                                                                         \
  static inline cty bennet_arbitrary_tnum_##cty##_top(void) {                            \
    return bennet_arbitrary_tnum_##cty(bennet_domain_tnum_top(cty));                     \
  }

#define bennet_arbitrary_tnum_top(cty) bennet_arbitrary_tnum_##cty##_top()

#define bennet_arbitrary_tnum(cty, d) (bennet_arbitrary_tnum_##cty(d))

#define bennet_domain_tnum_of(cty, value, mask) bennet_domain_tnum_of_##cty(value, mask)

#define bennet_arbitrary_tnum_of(cty, v, m)                                              \
  ({                                                                                     \
    bennet_domain_tnum(cty) bennet_arbitrary_tnum_tmp =                                  \
        (bennet_domain_tnum(cty)){.top = false, .bottom = false, .value = v, .mask = m}; \
    bennet_arbitrary_tnum_##cty(&bennet_arbitrary_tnum_tmp);                             \
  })

BENNET_DOMAIN_TNUM_DECL(uint8_t)
BENNET_DOMAIN_TNUM_DECL(uint16_t)
BENNET_DOMAIN_TNUM_DECL(uint32_t)
BENNET_DOMAIN_TNUM_DECL(uint64_t)
BENNET_DOMAIN_TNUM_DECL(uintptr_t)

BENNET_DOMAIN_TNUM_DECL(int8_t)
BENNET_DOMAIN_TNUM_DECL(int16_t)
BENNET_DOMAIN_TNUM_DECL(int32_t)
BENNET_DOMAIN_TNUM_DECL(int64_t)

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_TNUM_H
