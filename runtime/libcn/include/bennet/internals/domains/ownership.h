#ifndef BENNET_DOMAINS_OWNERSHIP_H
#define BENNET_DOMAINS_OWNERSHIP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_ownership(cty) struct bennet_domain_ownership_##cty
#define bennet_domain_ownership_of(cty, before, after)                                   \
  bennet_domain_ownership_of_##cty((before), (after))

#define bennet_domain_ownership_top(cty)        (bennet_domain_ownership_top_##cty())
#define bennet_domain_ownership_is_top(cty, cs) (bennet_domain_ownership_is_top_##cty(cs))

#define bennet_domain_ownership_bottom(cty) (bennet_domain_ownership_bottom_##cty())
#define bennet_domain_ownership_is_bottom(cty, cs)                                       \
  (bennet_domain_ownership_is_bottom_##cty(cs))

#define BENNET_DOMAIN_OWNERSHIP_DECL(cty)                                                \
  bennet_domain_ownership(cty) {                                                         \
    bool bottom;                                                                         \
    size_t before;                                                                       \
    size_t after;                                                                        \
  };                                                                                     \
                                                                                         \
  static inline bennet_domain_ownership(cty) *                                           \
      bennet_domain_ownership_of_##cty(size_t before, size_t after) {                    \
    bennet_domain_ownership(cty) *ret =                                                  \
        (bennet_domain_ownership(cty) *)malloc(sizeof(bennet_domain_ownership(cty)));    \
    ret->bottom = 0;                                                                     \
    ret->before = before;                                                                \
    ret->after = after;                                                                  \
                                                                                         \
    return ret;                                                                          \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) * bennet_domain_ownership_top_##cty(void) { \
    bennet_domain_ownership(cty) *ret =                                                  \
        (bennet_domain_ownership(cty) *)malloc(sizeof(bennet_domain_ownership(cty)));    \
    ret->bottom = 0;                                                                     \
    ret->before = 0;                                                                     \
    ret->after = 0;                                                                      \
                                                                                         \
    return ret;                                                                          \
  }                                                                                      \
  static inline bool bennet_domain_ownership_is_top_##cty(                               \
      bennet_domain_ownership(cty) * cs) {                                               \
    return (cs->before == 0) && (cs->after == 0);                                        \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) *                                           \
      bennet_domain_ownership_bottom_##cty(void) {                                       \
    bennet_domain_ownership(cty) *ret =                                                  \
        (bennet_domain_ownership(cty) *)malloc(sizeof(bennet_domain_ownership(cty)));    \
    ret->bottom = 1;                                                                     \
                                                                                         \
    return ret;                                                                          \
  }                                                                                      \
  static inline bool bennet_domain_ownership_is_bottom_##cty(                            \
      bennet_domain_ownership(cty) * cs) {                                               \
    return cs->bottom;                                                                   \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_domain_ownership_leq_##cty(                                  \
      bennet_domain_ownership(cty) * cs1, bennet_domain_ownership(cty) * cs2) {          \
    if (cs1->bottom) {                                                                   \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    if (cs2->bottom) {                                                                   \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    return (cs1->before >= cs2->before) && (cs1->after >= cs2->after);                   \
  }                                                                                      \
                                                                                         \
  static inline bool bennet_domain_ownership_equal_##cty(                                \
      bennet_domain_ownership(cty) * cs1, bennet_domain_ownership(cty) * cs2) {          \
    if (cs1->bottom && cs2->bottom) {                                                    \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    if (cs1->bottom || cs2->bottom) {                                                    \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    return (cs1->before == cs2->before) && (cs1->after == cs2->after);                   \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) *                                           \
      bennet_domain_ownership_join_##cty(                                                \
          bennet_domain_ownership(cty) * cs1, bennet_domain_ownership(cty) * cs2) {      \
    bennet_domain_ownership(cty) *ret =                                                  \
        (bennet_domain_ownership(cty) *)malloc(sizeof(bennet_domain_ownership(cty)));    \
    if (cs1->bottom) {                                                                   \
      *ret = *cs2;                                                                       \
      return ret;                                                                        \
    }                                                                                    \
                                                                                         \
    if (cs2->bottom) {                                                                   \
      *ret = *cs1;                                                                       \
      return ret;                                                                        \
    }                                                                                    \
                                                                                         \
    ret->bottom = false;                                                                 \
    ret->before = (cs1->before < cs2->before) ? cs1->before : cs2->before;               \
    ret->after = (cs1->after < cs2->after) ? cs1->after : cs2->after;                    \
    return ret;                                                                          \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) *                                           \
      bennet_domain_ownership_meet_##cty(                                                \
          bennet_domain_ownership(cty) * cs1, bennet_domain_ownership(cty) * cs2) {      \
    bennet_domain_ownership(cty) *ret =                                                  \
        (bennet_domain_ownership(cty) *)malloc(sizeof(bennet_domain_ownership(cty)));    \
    if (cs1->bottom || cs2->bottom) {                                                    \
      ret->bottom = true;                                                                \
      return ret;                                                                        \
    }                                                                                    \
                                                                                         \
    ret->bottom = false;                                                                 \
    ret->before = (cs1->before > cs2->before) ? cs1->before : cs2->before;               \
    ret->after = (cs1->after > cs2->after) ? cs1->after : cs2->after;                    \
    return ret;                                                                          \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) *                                           \
      bennet_domain_ownership_copy_##cty(bennet_domain_ownership(cty) * cs) {            \
    bennet_domain_ownership(cty) *ret =                                                  \
        (bennet_domain_ownership(cty) *)malloc(sizeof(bennet_domain_ownership(cty)));    \
    *ret = *cs;                                                                          \
    return ret;                                                                          \
  }                                                                                      \
                                                                                         \
  bennet_domain_ownership(cty) * bennet_domain_ownership_from_assignment_##cty(          \
                                     void *base_ptr, void *addr, size_t bytes);          \
                                                                                         \
  cty bennet_domain_ownership_arbitrary_##cty(bennet_domain_ownership(cty) *);           \
                                                                                         \
  bool bennet_domain_ownership_check_##cty(cty, bennet_domain_ownership(cty) *);         \
                                                                                         \
  static inline cty bennet_arbitrary_ownership_##cty##_top(void) {                       \
    bennet_domain_ownership(cty) *d = bennet_domain_ownership_top(cty);                  \
    return bennet_domain_ownership_arbitrary_##cty(d);                                   \
  }                                                                                      \
                                                                                         \
  static inline cty bennet_arbitrary_ownership_##cty##_bottom(void) {                    \
    bennet_domain_ownership(cty) *d = bennet_domain_ownership_bottom(cty);               \
    return bennet_domain_ownership_arbitrary_##cty(d);                                   \
  }

#define bennet_arbitrary_ownership_top(cty) (bennet_arbitrary_ownership_##cty##_top())

#define bennet_arbitrary_ownership_bottom(cty)                                           \
  (bennet_arbitrary_ownership_##cty##_bottom())

#define bennet_arbitrary_ownership(cty, d) (bennet_arbitrary_ownership_##cty##(d))

#define bennet_arbitrary_ownership_of(cty, before, after)                                \
  ({                                                                                     \
    bennet_domain_ownership(cty) bennet_arbitrary_ownership_tmp =                        \
        (bennet_domain_ownership(cty)){.before = before, .after = after};                \
    bennet_domain_ownership_arbitrary_##cty(&bennet_arbitrary_ownership_tmp);            \
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
