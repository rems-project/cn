#ifndef BENNET_DOMAINS_CONGR_H
#define BENNET_DOMAINS_CONGR_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <bennet/internals/absint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_congr(cty) struct bennet_domain_congr_##cty

#define bennet_domain_congr_top(cty)    (bennet_domain_congr_top_##cty())
#define bennet_domain_congr_bottom(cty) (bennet_domain_congr_bottom_##cty())

#define BENNET_DOMAIN_CONGR_DECL(cty)                                                    \
  bennet_domain_congr(cty) {                                                             \
    bool top;                                                                            \
    bool bottom;                                                                         \
    cty modulus;                                                                         \
    cty residue;                                                                         \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_congr_##cty(bennet_domain_congr(cty)*);                           \
  bennet_domain_congr(cty) * bennet_domain_congr_top_##cty(void);                        \
  bennet_domain_congr(cty) * bennet_domain_congr_bottom_##cty(void);                     \
  bennet_domain_congr(cty) * bennet_domain_congr_of_##cty(cty modulus, cty residue);     \
  bool bennet_domain_congr_is_top_##cty(bennet_domain_congr(cty)*);                      \
  bool bennet_domain_congr_is_bottom_##cty(bennet_domain_congr(cty)*);                   \
                                                                                         \
  bool bennet_domain_congr_leq_##cty(                                                    \
      bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);                             \
  bool bennet_domain_congr_equal_##cty(                                                  \
      bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);                             \
  bennet_domain_congr(cty) * bennet_domain_congr_join_##cty(                             \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_meet_##cty(                             \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_copy_##cty(bennet_domain_congr(cty)*);  \
                                                                                         \
  cty bennet_domain_congr_arbitrary_##cty(bennet_domain_congr(cty)*);                    \
  bool bennet_domain_congr_check_##cty(cty, bennet_domain_congr(cty)*);                  \
                                                                                         \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_from_assignment_##cty(void*, void*, size_t);                   \
                                                                                         \
  /* Arithmetic operations */                                                            \
  bennet_domain_congr(cty) * bennet_domain_congr_add_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_sub_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_mul_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_div_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_mod_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
                                                                                         \
  /* Bitwise operations */                                                               \
  bennet_domain_congr(cty) * bennet_domain_congr_and_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_or_##cty(                               \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_xor_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
                                                                                         \
  /* Shift operations */                                                                 \
  bennet_domain_congr(cty) * bennet_domain_congr_shl_##cty(                              \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
  bennet_domain_congr(cty) * bennet_domain_congr_lshr_##cty(                             \
                                 bennet_domain_congr(cty)*, bennet_domain_congr(cty)*);  \
                                                                                         \
  /* Interval conversion */                                                              \
  bennet_domain_congr(cty) * bennet_domain_congr_of_interval_##cty(cty lo, cty hi);      \
  bool bennet_domain_congr_to_interval_##cty(                                            \
      bennet_domain_congr(cty)*, cty* lo_out, cty* hi_out);                              \
                                                                                         \
  static inline cty bennet_arbitrary_congr_##cty##_top(void) {                           \
    return bennet_arbitrary_congr_##cty(bennet_domain_congr_top(cty));                   \
  }

#define bennet_arbitrary_congr_top(cty) bennet_arbitrary_congr_##cty##_top()

#define bennet_arbitrary_congr(cty, d) (bennet_arbitrary_congr_##cty(d))

#define bennet_domain_congr_of(cty, modulus, residue)                                    \
  bennet_domain_congr_of_##cty(modulus, residue)

#define bennet_arbitrary_congr_of(cty, m, r)                                             \
  ({                                                                                     \
    bennet_domain_congr(cty) bennet_arbitrary_congr_tmp = (bennet_domain_congr(cty)){    \
        .top = false, .bottom = false, .modulus = m, .residue = r};                      \
    bennet_arbitrary_congr_##cty(&bennet_arbitrary_congr_tmp);                           \
  })

BENNET_DOMAIN_CONGR_DECL(uint8_t)
BENNET_DOMAIN_CONGR_DECL(uint16_t)
BENNET_DOMAIN_CONGR_DECL(uint32_t)
BENNET_DOMAIN_CONGR_DECL(uint64_t)
BENNET_DOMAIN_CONGR_DECL(uintptr_t)

BENNET_DOMAIN_CONGR_DECL(int8_t)
BENNET_DOMAIN_CONGR_DECL(int16_t)
BENNET_DOMAIN_CONGR_DECL(int32_t)
BENNET_DOMAIN_CONGR_DECL(int64_t)

/**
 * Forward abstract transformer for congr domain.
 * Evaluates a term given abstract domains for all symbols.
 */
bennet_tagged_domain bennet_congr_transform_forward(
    cn_term* term, bennet_absint_state* state);

/**
 * Backward abstract transformer for congr domain.
 * Refines the domain of a specific symbol to satisfy a constraint.
 */
bennet_absint_state* bennet_congr_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state);

/**
 * Backward transformer for boolean constraint using congr domain.
 * Refines state to make the term evaluate to the given boolean value.
 */
bennet_absint_state* bennet_congr_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_CONGR_H
