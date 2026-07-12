#ifndef BENNET_DOMAIN_H
#define BENNET_DOMAIN_H

#include <stddef.h>
#include <stdint.h>

#include <bennet/internals/absint.h>
#include <bennet/utils/optional.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BENNET_DOMAIN_DECL(ty)                                                           \
  bennet_domain(ty);                                                                     \
                                                                                         \
  bennet_domain(ty) * bennet_domain_top_##ty(void);                                      \
  bool bennet_domain_is_top_##ty(bennet_domain(ty)*);                                    \
                                                                                         \
  bennet_domain(ty) * bennet_domain_bottom_##ty(void);                                   \
  bool bennet_domain_is_bottom_##ty(bennet_domain(ty)*);                                 \
                                                                                         \
  bool bennet_domain_leq_##ty(bennet_domain(ty)*, bennet_domain(ty)*);                   \
  bool bennet_domain_equal_##ty(bennet_domain(ty)*, bennet_domain(ty)*);                 \
                                                                                         \
  bennet_domain(ty) * bennet_domain_join_##ty(bennet_domain(ty)*, bennet_domain(ty)*);   \
  bennet_domain(ty) * bennet_domain_meet_##ty(bennet_domain(ty)*, bennet_domain(ty)*);   \
                                                                                         \
  bennet_domain(ty) * bennet_domain_copy_##ty(bennet_domain(ty)*);                       \
                                                                                         \
  ty bennet_domain_arbitrary_##ty(bennet_domain(ty)*);                                   \
  bool bennet_domain_check_##ty(ty, bennet_domain(ty)*);                                 \
                                                                                         \
  bennet_domain(ty) *                                                                    \
      bennet_domain_from_assignment_##ty(void* base_ptr, void* addr, size_t bytes);      \
                                                                                         \
  bool bennet_domain_to_interval_##ty(bennet_domain(ty)*, ty* lo_out, ty* hi_out);       \
  bennet_domain(ty) * bennet_domain_of_interval_##ty(ty lo, ty hi);

#define bennet_domain(ty) struct bennet_domain_##ty

BENNET_DOMAIN_DECL(int8_t);
BENNET_DOMAIN_DECL(uint8_t);
BENNET_DOMAIN_DECL(int16_t);
BENNET_DOMAIN_DECL(uint16_t);
BENNET_DOMAIN_DECL(int32_t);
BENNET_DOMAIN_DECL(uint32_t);
BENNET_DOMAIN_DECL(int64_t);
BENNET_DOMAIN_DECL(uint64_t);
BENNET_DOMAIN_DECL(uintptr_t);

#define bennet_domain_top(ty)        (bennet_domain_top_##ty())
#define bennet_domain_is_top(ty, cs) (bennet_domain_is_top_##ty(cs))

#define bennet_domain_bottom(ty)        (bennet_domain_bottom_##ty())
#define bennet_domain_is_bottom(ty, cs) (bennet_domain_is_bottom_##ty(cs))

#define bennet_domain_leq(ty, cs1, cs2)   (bennet_domain_leq_##ty(cs1, cs2))
#define bennet_domain_equal(ty, cs1, cs2) (bennet_domain_equal_##ty(cs1, cs2))

#define bennet_domain_join(ty, cs1, cs2) (bennet_domain_join_##ty(cs1, cs2))
#define bennet_domain_meet(ty, cs1, cs2) (bennet_domain_meet_##ty(cs1, cs2))

#define bennet_domain_copy(ty, cs) (bennet_domain_copy_##ty(cs))

#define bennet_domain_arbitrary(ty, cs)  (bennet_domain_arbitrary_##ty(cs))
#define bennet_domain_check(ty, val, cs) (bennet_domain_check_##ty(val, cs))
#define bennet_domain_check_ownership(ty, val, cs)                                       \
  (bennet_domain_check_ownership_##ty(val, cs))

#define bennet_domain_refine(ty, cs, sym, bt, constraint, out)                           \
  (bennet_domain_refine_##ty(cs, sym, bt, constraint, out))

#define bennet_domain_to_interval(ty, ptr, lo, hi)                                       \
  bennet_domain_to_interval_##ty(ptr, lo, hi)
#define bennet_domain_of_interval(ty, lo, hi) bennet_domain_of_interval_##ty(lo, hi)

#define bennet_domain_top_except_ownership(ty, ptr)                                      \
  (bennet_domain_top_except_ownership_##ty(ptr))

#define bennet_domain_transform_backward(ty, term, sym, out_bt, tgt_bt, out)             \
  (bennet_domain_transform_backward_##ty(term, sym, out_bt, tgt_bt, out))

#define BENNET_DOMAIN_CAST_DECL(from_ty, to_ty)                                          \
  bennet_domain(to_ty) *                                                                 \
      bennet_domain_cast_##from_ty##_to_##to_ty(bennet_domain(from_ty)*);

#define bennet_domain_cast(from_ty, to_ty, cs)                                           \
  (bennet_domain_cast_##from_ty##_to_##to_ty(cs))

/*=============================================================================
 * Abstract Transformers API
 *
 * Forward and backward abstract transformers for runtime refinement during
 * test generation (Bennet). These operate over the cn_term AST.
 *===========================================================================*/

/* Declare refine functions now that the types are available */
#define BENNET_DOMAIN_REFINE_DECL(ty)                                                    \
  bennet_domain(ty) * bennet_domain_refine_##ty(bennet_domain(ty) * cs,                  \
                          bennet_absint_sym x_sym,                                       \
                          cn_base_type * x_bt,                                           \
                          cn_term * constraint_term,                                     \
                          bool* is_bottom_out);

BENNET_DOMAIN_REFINE_DECL(int8_t);
BENNET_DOMAIN_REFINE_DECL(uint8_t);
BENNET_DOMAIN_REFINE_DECL(int16_t);
BENNET_DOMAIN_REFINE_DECL(uint16_t);
BENNET_DOMAIN_REFINE_DECL(int32_t);
BENNET_DOMAIN_REFINE_DECL(uint32_t);
BENNET_DOMAIN_REFINE_DECL(int64_t);
BENNET_DOMAIN_REFINE_DECL(uint64_t);
BENNET_DOMAIN_REFINE_DECL(uintptr_t);

/* Declare refine_with_state functions */
#define bennet_domain_refine_with_state(                                                 \
    ty, cs, sym, bt, constraint, out, extra_sym, extra_domain)                           \
  (bennet_domain_refine_with_state_##ty(                                                 \
      cs, sym, bt, constraint, out, extra_sym, extra_domain))

#define BENNET_DOMAIN_REFINE_WITH_STATE_DECL(ty)                                         \
  bennet_domain(ty) * bennet_domain_refine_with_state_##ty(bennet_domain(ty) * cs,       \
                          bennet_absint_sym x_sym,                                       \
                          cn_base_type * x_bt,                                           \
                          cn_term * constraint_term,                                     \
                          bool* is_bottom_out,                                           \
                          bennet_absint_sym extra_sym,                                   \
                          bennet_tagged_domain extra_domain);

BENNET_DOMAIN_REFINE_WITH_STATE_DECL(int8_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(uint8_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(int16_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(uint16_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(int32_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(uint32_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(int64_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(uint64_t);
BENNET_DOMAIN_REFINE_WITH_STATE_DECL(uintptr_t);

/* Declare transform_backward functions */
#define BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(ty)                                        \
  bennet_domain(ty) * bennet_domain_transform_backward_##ty(cn_term* term,               \
                          bennet_absint_sym target_sym,                                  \
                          cn_base_type* output_bt,                                       \
                          cn_base_type* target_bt,                                       \
                          bennet_domain(ty) * output_domain);

BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(int8_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(uint8_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(int16_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(uint16_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(int32_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(uint32_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(int64_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(uint64_t);
BENNET_DOMAIN_TRANSFORM_BACKWARD_DECL(uintptr_t);

/* Lift a bare ownership element into the product: top except the ownership
 * component (analogous to top_except_ownership). assign.c's backward-blame
 * path builds the blamed product with this instead of type-punning the
 * ownership struct as the product. The
 * struct forward declaration avoids pulling ownership.h into this header. */
#define bennet_domain_from_ownership(ty, own) (bennet_domain_from_ownership_##ty(own))

#define BENNET_DOMAIN_FROM_OWNERSHIP_DECL(ty)                                            \
  struct bennet_domain_ownership_##ty;                                                   \
  bennet_domain(ty) *                                                                    \
      bennet_domain_from_ownership_##ty(struct bennet_domain_ownership_##ty* own);

BENNET_DOMAIN_FROM_OWNERSHIP_DECL(int8_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(uint8_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(int16_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(uint16_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(int32_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(uint32_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(int64_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(uint64_t);
BENNET_DOMAIN_FROM_OWNERSHIP_DECL(uintptr_t);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAIN_H
