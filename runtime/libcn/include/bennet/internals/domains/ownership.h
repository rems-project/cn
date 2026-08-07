#ifndef BENNET_DOMAINS_OWNERSHIP_H
#define BENNET_DOMAINS_OWNERSHIP_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <cn-smt/memory/std_alloc.h>

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
    bennet_domain_ownership(cty)* ret =                                                  \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
    ret->bottom = 0;                                                                     \
    ret->before = before;                                                                \
    ret->after = after;                                                                  \
                                                                                         \
    return ret;                                                                          \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) * bennet_domain_ownership_top_##cty(void) { \
    bennet_domain_ownership(cty)* ret =                                                  \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
    ret->bottom = 0;                                                                     \
    ret->before = 0;                                                                     \
    ret->after = 0;                                                                      \
                                                                                         \
    return ret;                                                                          \
  }                                                                                      \
  static inline bool bennet_domain_ownership_is_top_##cty(                               \
      bennet_domain_ownership(cty) * cs) {                                               \
    return !cs->bottom && (cs->before == 0) && (cs->after == 0);                         \
  }                                                                                      \
                                                                                         \
  static inline bennet_domain_ownership(cty) *                                           \
      bennet_domain_ownership_bottom_##cty(void) {                                       \
    bennet_domain_ownership(cty)* ret =                                                  \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
    ret->bottom = 1;                                                                     \
    ret->before = 0;                                                                     \
    ret->after = 0;                                                                      \
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
    bennet_domain_ownership(cty)* ret =                                                  \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
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
    bennet_domain_ownership(cty)* ret =                                                  \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
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
    bennet_domain_ownership(cty)* ret =                                                  \
        (bennet_domain_ownership(cty)*)std_malloc(sizeof(bennet_domain_ownership(cty))); \
    *ret = *cs;                                                                          \
    return ret;                                                                          \
  }                                                                                      \
                                                                                         \
  bennet_domain_ownership(cty) * bennet_domain_ownership_from_assignment_##cty(          \
                                     void* base_ptr, void* addr, size_t bytes);          \
                                                                                         \
  bool bennet_domain_ownership_to_interval_##cty(                                        \
      bennet_domain_ownership(cty)*, cty* lo_out, cty* hi_out);                          \
  bennet_domain_ownership(cty) *                                                         \
      bennet_domain_ownership_of_interval_##cty(cty lo, cty hi);                         \
                                                                                         \
  cty bennet_domain_ownership_arbitrary_##cty(bennet_domain_ownership(cty)*);            \
                                                                                         \
  bool bennet_domain_ownership_check_##cty(cty, bennet_domain_ownership(cty)*);          \
                                                                                         \
  static inline cty bennet_arbitrary_ownership_##cty##_top(void) {                       \
    bennet_domain_ownership(cty)* d = bennet_domain_ownership_top(cty);                  \
    return bennet_domain_ownership_arbitrary_##cty(d);                                   \
  }                                                                                      \
                                                                                         \
  static inline cty bennet_arbitrary_ownership_##cty##_bottom(void) {                    \
    bennet_domain_ownership(cty)* d = bennet_domain_ownership_bottom(cty);               \
    return bennet_domain_ownership_arbitrary_##cty(d);                                   \
  }

#define bennet_arbitrary_ownership_top(cty) (bennet_arbitrary_ownership_##cty##_top())

#define bennet_arbitrary_ownership_bottom(cty)                                           \
  (bennet_arbitrary_ownership_##cty##_bottom())

#define bennet_arbitrary_ownership(cty, d) (bennet_arbitrary_ownership_##cty(d))

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

/**
 * Forward ownership transformer for member_shift.
 * Given ownership domain for base pointer, compute ownership for base + offset.
 */
#define BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(cty)                                          \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_member_shift_##cty(                                               \
          bennet_domain_ownership(cty) * base, size_t offset);

/**
 * Forward ownership transformer for array_shift.
 * Given ownership domain for base pointer, compute ownership for base + elem_size *
 * index. Index must be non-negative.
 */
#define BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(cty)                                           \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_array_shift_##cty(                                                \
          bennet_domain_ownership(cty) * base, size_t elem_size, size_t index);

/**
 * Backward ownership transformer for member_shift.
 * Given required ownership at shifted pointer, compute required ownership at base.
 */
#define BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(cty)                                 \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_member_shift_backward_##cty(                                      \
          bennet_domain_ownership(cty) * shifted, size_t offset);

/**
 * Backward ownership transformer for array_shift.
 * Given required ownership at shifted pointer, compute required ownership at base.
 */
#define BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(cty)                                  \
  bennet_domain_ownership(cty) *                                                         \
      bennet_ownership_array_shift_backward_##cty(                                       \
          bennet_domain_ownership(cty) * shifted, size_t elem_size, size_t index);

BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(uint8_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(uint16_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(uint32_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(uint64_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(int8_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(int16_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(int32_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(int64_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_DECL(uintptr_t)

BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(uint8_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(uint16_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(uint32_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(uint64_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(int8_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(int16_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(int32_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(int64_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_DECL(uintptr_t)

BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(uint8_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(uint16_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(uint32_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(uint64_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(int8_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(int16_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(int32_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(int64_t)
BENNET_OWNERSHIP_MEMBER_SHIFT_BACKWARD_DECL(uintptr_t)

BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(uint8_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(uint16_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(uint32_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(uint64_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(int8_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(int16_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(int32_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(int64_t)
BENNET_OWNERSHIP_ARRAY_SHIFT_BACKWARD_DECL(uintptr_t)

#define bennet_ownership_member_shift(cty, base, offset)                                 \
  bennet_ownership_member_shift_##cty(base, offset)

#define bennet_ownership_array_shift(cty, base, elem_size, index)                        \
  bennet_ownership_array_shift_##cty(base, elem_size, index)

#define bennet_ownership_member_shift_backward(cty, shifted, offset)                     \
  bennet_ownership_member_shift_backward_##cty(shifted, offset)

#define bennet_ownership_array_shift_backward(cty, shifted, elem_size, index)            \
  bennet_ownership_array_shift_backward_##cty(shifted, elem_size, index)

/*-----------------------------------------------------------------------------
 * Ownership Abstract Transformers
 *
 * Forward and backward abstract transformers for ownership domain refinement.
 *---------------------------------------------------------------------------*/

struct cn_term;
struct bennet_absint_state;
struct bennet_tagged_domain;

/**
 * Forward ownership transformer.
 * Evaluates a term in the given abstract state to produce an ownership domain.
 */
struct bennet_tagged_domain bennet_ownership_transform_forward(
    struct cn_term* term, struct bennet_absint_state* state);

/**
 * Backward ownership transformer.
 * Given a desired output domain, propagates constraints backward to refine
 * the domain of the target symbol in the state.
 */
struct bennet_absint_state* bennet_ownership_transform_backward(struct cn_term* term,
    bennet_absint_sym target_sym,
    struct bennet_tagged_domain output_domain,
    struct bennet_absint_state* state);

/**
 * Backward assume transformer for ownership.
 * Refines the abstract state given that a boolean term evaluates to `value`.
 */
struct bennet_absint_state* bennet_ownership_transform_backward_assume(
    struct cn_term* term, bool value, struct bennet_absint_state* state);

/**
 * Backward propagation to all symbolic variables.
 * Walks the term tree; at each SYM node, meets the ownership domain into state.
 * At MEMBER_SHIFT / ARRAY_SHIFT, inverts the shift before recursing into base.
 * At CAST, recurses into inner (casts preserve ownership).
 */
struct bennet_absint_state* bennet_ownership_backward_propagate_to_syms(
    struct cn_term* term,
    bennet_domain_ownership(uintptr_t) * own_dom,
    struct bennet_absint_state* state);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_OWNERSHIP_H
