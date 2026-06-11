#ifndef BENNET_DOMAINS_WINT_H
#define BENNET_DOMAINS_WINT_H

#include <stdbool.h>
#include <stdint.h>

#include <bennet/internals/domain.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_domain_wint(cty) struct bennet_domain_wint_##cty

#define bennet_domain_wint_top(cty)    (bennet_domain_wint_top_##cty())
#define bennet_domain_wint_bottom(cty) (bennet_domain_wint_bottom_##cty())

#define BENNET_DOMAIN_WINT_DECL(cty)                                                     \
  bennet_domain_wint(cty) {                                                              \
    bool top;                                                                            \
    bool bottom;                                                                         \
    cty start;                                                                           \
    cty end;                                                                             \
  };                                                                                     \
                                                                                         \
  cty bennet_arbitrary_wint_##cty(bennet_domain_wint(cty)*);                             \
  bennet_domain_wint(cty) * bennet_domain_wint_top_##cty(void);                          \
  bennet_domain_wint(cty) * bennet_domain_wint_bottom_##cty(void);                       \
  bennet_domain_wint(cty) * bennet_domain_wint_of_##cty(cty start, cty end);             \
  bool bennet_domain_wint_is_top_##cty(bennet_domain_wint(cty)*);                        \
  bool bennet_domain_wint_is_bottom_##cty(bennet_domain_wint(cty)*);                     \
                                                                                         \
  bool bennet_domain_wint_leq_##cty(bennet_domain_wint(cty)*, bennet_domain_wint(cty)*); \
  bool bennet_domain_wint_equal_##cty(                                                   \
      bennet_domain_wint(cty)*, bennet_domain_wint(cty)*);                               \
  bennet_domain_wint(cty) *                                                              \
      bennet_domain_wint_join_##cty(bennet_domain_wint(cty)*, bennet_domain_wint(cty)*); \
  bennet_domain_wint(cty) *                                                              \
      bennet_domain_wint_meet_##cty(bennet_domain_wint(cty)*, bennet_domain_wint(cty)*); \
  bennet_domain_wint(cty) * bennet_domain_wint_copy_##cty(bennet_domain_wint(cty)*);     \
                                                                                         \
  cty bennet_domain_wint_arbitrary_##cty(bennet_domain_wint(cty)*);                      \
  bool bennet_domain_wint_check_##cty(cty, bennet_domain_wint(cty)*);                    \
                                                                                         \
  bennet_domain_wint(cty) *                                                              \
      bennet_domain_wint_from_assignment_##cty(void*, void*, size_t);                    \
                                                                                         \
  bool bennet_domain_wint_to_interval_##cty(                                             \
      bennet_domain_wint(cty)*, cty* lo_out, cty* hi_out);                               \
                                                                                         \
  static inline bennet_domain_wint(cty) *                                                \
      bennet_domain_wint_of_interval_##cty(cty lo, cty hi) {                             \
    return bennet_domain_wint_of_##cty(lo, hi);                                          \
  }                                                                                      \
                                                                                         \
  static inline cty bennet_arbitrary_wint_##cty##_top(void) {                            \
    return bennet_arbitrary_wint_##cty(bennet_domain_wint_top(cty));                     \
  }

#define bennet_arbitrary_wint_top(cty) bennet_arbitrary_wint_##cty##_top()

#define bennet_arbitrary_wint(cty, d) (bennet_arbitrary_wint_##cty(d))

#define bennet_domain_wint_of(cty, start, end) bennet_domain_wint_of_##cty(start, end)

#define bennet_arbitrary_wint_of(cty, s, e)                                              \
  ({                                                                                     \
    ((s) - (e) == 1 || (s == BV_MIN(cty) && e == BV_MAX(cty)))                           \
        ? bennet_arbitrary_wint_top(cty)                                                 \
        : ({                                                                             \
            bennet_domain_wint(cty) bennet_arbitrary_wint_tmp = (bennet_domain_wint(     \
                cty)){.top = false, .bottom = false, .start = s, .end = e};              \
            bennet_arbitrary_wint_##cty(&bennet_arbitrary_wint_tmp);                     \
          });                                                                            \
  })

BENNET_DOMAIN_WINT_DECL(uint8_t)
BENNET_DOMAIN_WINT_DECL(uint16_t)
BENNET_DOMAIN_WINT_DECL(uint32_t)
BENNET_DOMAIN_WINT_DECL(uint64_t)
BENNET_DOMAIN_WINT_DECL(uintptr_t)

BENNET_DOMAIN_WINT_DECL(int8_t)
BENNET_DOMAIN_WINT_DECL(int16_t)
BENNET_DOMAIN_WINT_DECL(int32_t)
BENNET_DOMAIN_WINT_DECL(int64_t)

/*-----------------------------------------------------------------------------
 * Forward Transformer
 *
 * Computes an abstract domain for the result of evaluating a term,
 * given abstract domains for all symbols in the state.
 *---------------------------------------------------------------------------*/

/**
 * Forward abstract transformer.
 * Computes an abstract domain for the result of evaluating a term,
 * given abstract domains for all symbols in the state.
 *
 * @param term    The term to evaluate abstractly
 * @param state   The abstract state (symbol -> domain mapping)
 * @return        Tagged domain representing possible values of the term
 *
 * Supported term types:
 * - CN_TERM_CONST: Singleton domain from constant value
 * - CN_TERM_SYM: Look up in state (top if not found)
 * - CN_TERM_UNOP: NOT, NEGATE, BW_COMPL
 * - CN_TERM_BINOP: ADD, SUB, MUL, DIV, REM, MOD, LT, LE, EQ, MIN, MAX,
 *                  BW_AND, BW_OR, BW_XOR, SHIFT_LEFT, SHIFT_RIGHT
 * - CN_TERM_ITE: Forward through conditional
 *
 * For comparison operators (LT, LE, EQ), returns a boolean domain
 * indicating which outcomes are possible.
 */
bennet_tagged_domain bennet_wint_transform_forward(
    cn_term* term, bennet_absint_state* state);

/*-----------------------------------------------------------------------------
 * Backward Transformer
 *
 * Refines the domain of a specific symbol to satisfy a constraint
 * on the term's output.
 *---------------------------------------------------------------------------*/

/**
 * Backward abstract transformer.
 * Refines the domain of a specific symbol to satisfy a constraint
 * on the term's output.
 *
 * @param term           The term whose output is constrained
 * @param target_sym     The symbol to refine
 * @param output_domain  The constraint on the term's output
 * @param state          The current abstract state
 * @return               New state with refined domain for target_sym
 *                       (returns state with bottom if unsatisfiable)
 *
 * Example: backward_refine(x + y, x, [0,10], {y: [5,20]})
 *          -> refines x to [0,5] (since x + 5..20 must be in [0,10])
 *
 * For boolean output_domain (from comparisons):
 *   - {true}:  Refine to make comparison true
 *   - {false}: Refine to make comparison false
 *
 * This function traces through nested expressions automatically.
 * For x in (x + 1) * y with output constraint, it computes the
 * intermediate constraints and propagates backward to x.
 */
bennet_absint_state* bennet_wint_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state);

/**
 * Backward transformer for boolean constraint (convenience function).
 * Refines state to make the term evaluate to the given boolean value.
 *
 * @param term    Boolean term (typically a comparison)
 * @param value   Required truth value (true or false)
 * @param state   Current abstract state
 * @return        New state refined to satisfy the constraint
 *
 * Example: backward_assume(x < y, true, state)
 *          -> refines domains of x and y to satisfy x < y
 */
bennet_absint_state* bennet_wint_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state);

/*-----------------------------------------------------------------------------
 * Numeric Refinement from Comparisons
 *
 * Refines numeric domains based on comparison constraints.
 *---------------------------------------------------------------------------*/

/**
 * Refine numeric domains based on a comparison constraint.
 * Given that `left op right` must be true/false, compute refined
 * domains for both operands.
 *
 * @param op           The comparison operator (LT, LE, EQ)
 * @param must_be_true Whether the comparison must be true or false
 * @param left_domain  Current domain of left operand
 * @param right_domain Current domain of right operand
 * @param out_left     Output: refined domain for left operand
 * @param out_right    Output: refined domain for right operand
 *
 * Example: refine_comparison(LT, true, [0,10], [5,20])
 *          -> out_left = [0,19], out_right = [1,20]
 *          (x < y, x in [0,10], y in [5,20] => x in [0,19], y in [1,20])
 */
void bennet_wint_transform_refine_comparison(bennet_absint_binop op,
    bool must_be_true,
    bennet_tagged_domain* left_domain,
    bennet_tagged_domain* right_domain,
    bennet_tagged_domain* out_left,
    bennet_tagged_domain* out_right);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAINS_WINT_H
