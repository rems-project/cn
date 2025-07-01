#ifndef BENNET_EXP_DOMAIN_H
#define BENNET_EXP_DOMAIN_H

#include <stddef.h>
#include <stdint.h>

#include <bennet-exp/optional.h>

#define bennet_domain(ty) struct bennet_domain_##ty

#define BENNET_DOMAIN_DECL(ty)                                                           \
  bennet_domain(ty) {                                                                    \
    /* General inequality constraints */                                                 \
    bennet_optional(ty) lower_bound_inc;                                                 \
    bennet_optional(ty) upper_bound_inc;                                                 \
    bennet_optional(ty) multiple;                                                        \
                                                                                         \
    /* Pointer constraints */                                                            \
    bool is_owned;                                                                       \
    size_t lower_offset_bound; /* Inclusive */                                           \
    size_t upper_offset_bound; /* Exclusive */                                           \
  }

BENNET_DOMAIN_DECL(int8_t);
BENNET_DOMAIN_DECL(uint8_t);
BENNET_DOMAIN_DECL(int16_t);
BENNET_DOMAIN_DECL(uint16_t);
BENNET_DOMAIN_DECL(int32_t);
BENNET_DOMAIN_DECL(uint32_t);
BENNET_DOMAIN_DECL(int64_t);
BENNET_DOMAIN_DECL(uint64_t);
BENNET_DOMAIN_DECL(intptr_t);
BENNET_DOMAIN_DECL(uintptr_t);
BENNET_DOMAIN_DECL(intmax_t);
BENNET_DOMAIN_DECL(uintmax_t);

#define bennet_domain_default(ty)                                                        \
  ((bennet_domain(ty)){/* General inequality constraints */                              \
      .lower_bound_inc = bennet_optional_none(ty),                                       \
      .upper_bound_inc = bennet_optional_none(ty),                                       \
      .multiple = bennet_optional_none(ty),                                              \
                                                                                         \
      /* Pointer constraints */                                                          \
      .is_owned = false,                                                                 \
      .lower_offset_bound = 0,                                                           \
      .upper_offset_bound = 0})

#define bennet_domain_cast(ty, cs)                                                       \
  ((bennet_domain(ty)){.lower_bound_inc = bennet_optional_cast(ty, cs->lower_bound_inc), \
      .upper_bound_inc = bennet_optional_cast(ty, cs->upper_bound_inc),                  \
      .multiple = bennet_optional_cast(ty, cs->multiple),                                \
                                                                                         \
      .is_owned = cs->is_owned,                                                          \
      .lower_offset_bound = cs->lower_offset_bound,                                      \
      .upper_offset_bound = cs->upper_offset_bound})

// FIXME: Check that bounds contain a multiple
#define bennet_domain_is_empty(cs)                                                       \
  ((bennet_optional_is_some(cs->lower_bound_inc) &&                                      \
       bennet_optional_is_some(cs->upper_bound_inc) &&                                   \
       bennet_optional_unwrap(cs->upper_bound_inc) <                                     \
           bennet_optional_unwrap(cs->lower_bound_inc)) ||                               \
      (bennet_optional_is_some(cs->multiple) &&                                          \
          (bennet_optional_unwrap(cs->multiple) == 0)))

#define bennet_domain_equal(ty) bennet_domain_equal_##ty

#define BENNET_DOMAIN_EQUAL_IMPL(ty)                                                     \
  static inline bool bennet_domain_equal(ty)(                                            \
      bennet_domain(ty) * cs1, bennet_domain(ty) * cs2) {                                \
    return (bennet_optional_equal(ty)(&cs1->lower_bound_inc, &cs2->lower_bound_inc) &&   \
            bennet_optional_equal(ty)(&cs1->upper_bound_inc, &cs2->upper_bound_inc) &&   \
            bennet_optional_equal(ty)(&cs1->multiple, &cs2->multiple) &&                 \
                                                                                         \
            cs1->is_owned == cs2->is_owned &&                                            \
            cs1->lower_offset_bound == cs2->lower_offset_bound &&                        \
            cs1->upper_offset_bound == cs2->upper_offset_bound);                         \
  }

BENNET_DOMAIN_EQUAL_IMPL(uint8_t)
BENNET_DOMAIN_EQUAL_IMPL(int8_t)
BENNET_DOMAIN_EQUAL_IMPL(uint16_t)
BENNET_DOMAIN_EQUAL_IMPL(int16_t)
BENNET_DOMAIN_EQUAL_IMPL(uint32_t)
BENNET_DOMAIN_EQUAL_IMPL(int32_t)
BENNET_DOMAIN_EQUAL_IMPL(uint64_t)
BENNET_DOMAIN_EQUAL_IMPL(int64_t)
BENNET_DOMAIN_EQUAL_IMPL(uintptr_t)
BENNET_DOMAIN_EQUAL_IMPL(intptr_t)
BENNET_DOMAIN_EQUAL_IMPL(uintmax_t)
BENNET_DOMAIN_EQUAL_IMPL(intmax_t)

typedef bennet_domain(intmax_t) bennet_domain_failure_info;

#define bennet_domain_failure_default() bennet_domain_default(intmax_t)

#define bennet_domain_update(ty, cs, new_cs)                                             \
  {                                                                                      \
    if (new_cs != NULL) {                                                                \
      if (bennet_optional_is_some(new_cs->lower_bound_inc)) {                            \
        ty lower_bound_inc = bennet_optional_unwrap(new_cs->lower_bound_inc);            \
                                                                                         \
        if (bennet_optional_is_some(cs->lower_bound_inc)) {                              \
          if (lower_bound_inc > bennet_optional_unwrap(cs->lower_bound_inc)) {           \
            cs->lower_bound_inc = bennet_optional_some(ty, lower_bound_inc);             \
          }                                                                              \
        } else {                                                                         \
          cs->lower_bound_inc = bennet_optional_some(ty, lower_bound_inc);               \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      if (bennet_optional_is_some(new_cs->upper_bound_inc)) {                            \
        ty upper_bound_inc = bennet_optional_unwrap(new_cs->upper_bound_inc);            \
                                                                                         \
        if (bennet_optional_is_some(cs->upper_bound_inc)) {                              \
          if (upper_bound_inc < bennet_optional_unwrap(cs->upper_bound_inc)) {           \
            cs->upper_bound_inc = bennet_optional_some(ty, upper_bound_inc);             \
          }                                                                              \
        } else {                                                                         \
          cs->upper_bound_inc = bennet_optional_some(ty, upper_bound_inc);               \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      if (new_cs->is_owned) {                                                            \
        assert(sizeof(ty) == sizeof(void*));                                             \
                                                                                         \
        cs->is_owned = true;                                                             \
                                                                                         \
        if (new_cs->lower_offset_bound > cs->lower_offset_bound) {                       \
          cs->lower_offset_bound = new_cs->lower_offset_bound;                           \
        }                                                                                \
                                                                                         \
        if (new_cs->upper_offset_bound > cs->upper_offset_bound) {                       \
          cs->upper_offset_bound = new_cs->upper_offset_bound;                           \
        }                                                                                \
      }                                                                                  \
    }                                                                                    \
  }

bennet_domain_failure_info bennet_domain_from_assignment(
    const void* p_alloc, const void* p, size_t bytes);

#endif  // BENNET_EXP_DOMAIN_H
