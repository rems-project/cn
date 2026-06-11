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
  /* Bitwise operations */                                                               \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_and_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_or_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);   \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_xor_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) * bennet_domain_tnum_not_##cty(bennet_domain_tnum(cty)*);      \
                                                                                         \
  /* Shift operations */                                                                 \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_shl_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_lshr_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*); \
                                                                                         \
  /* Arithmetic operations */                                                            \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_add_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_sub_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_mul_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_div_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
  bennet_domain_tnum(cty) *                                                              \
      bennet_domain_tnum_mod_##cty(bennet_domain_tnum(cty)*, bennet_domain_tnum(cty)*);  \
                                                                                         \
  /* Interval conversion */                                                              \
  bennet_domain_tnum(cty) * bennet_domain_tnum_of_interval_##cty(cty lo, cty hi);        \
  bool bennet_domain_tnum_to_interval_##cty(                                             \
      bennet_domain_tnum(cty)*, cty* lo_out, cty* hi_out);                               \
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

/*-----------------------------------------------------------------------------
 * Tagged Domain Transformers for Tnum
 *
 * Forward and backward abstract transformers operating on bennet_tagged_domain
 * using tnum internals.
 *---------------------------------------------------------------------------*/

#include <bennet/internals/domain.h>

/**
 * Forward abstract transformer for tnum domain.
 * Computes an abstract tnum domain for the result of evaluating a term.
 */
bennet_tagged_domain bennet_tnum_transform_forward(
    cn_term* term, bennet_absint_state* state);

/**
 * Backward abstract transformer for tnum domain.
 * Refines the domain of a specific symbol to satisfy a constraint.
 */
bennet_absint_state* bennet_tnum_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state);

/**
 * Backward transformer for boolean constraint using tnum domain.
 * Refines state to make the term evaluate to the given boolean value.
 */
bennet_absint_state* bennet_tnum_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_TNUM_H
