#ifndef BENNET_DOMAIN_H
#define BENNET_DOMAIN_H

#include <stddef.h>
#include <stdint.h>

#include <bennet/utils/optional.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BENNET_DOMAIN_DECL(ty)                                                           \
  bennet_domain(ty);                                                                     \
                                                                                         \
  bennet_domain(ty) * bennet_domain_top_##ty(void);                                      \
  bool bennet_domain_is_top_##ty(bennet_domain(ty) *);                                   \
                                                                                         \
  bennet_domain(ty) * bennet_domain_bottom_##ty(void);                                   \
  bool bennet_domain_is_bottom_##ty(bennet_domain(ty) *);                                \
                                                                                         \
  bool bennet_domain_leq_##ty(bennet_domain(ty) *, bennet_domain(ty) *);                 \
  bool bennet_domain_equal_##ty(bennet_domain(ty) *, bennet_domain(ty) *);               \
                                                                                         \
  bennet_domain(ty) * bennet_domain_join_##ty(bennet_domain(ty) *, bennet_domain(ty) *); \
  bennet_domain(ty) * bennet_domain_meet_##ty(bennet_domain(ty) *, bennet_domain(ty) *); \
                                                                                         \
  bennet_domain(ty) * bennet_domain_copy_##ty(bennet_domain(ty) *);                      \
                                                                                         \
  ty bennet_domain_arbitrary_##ty(bennet_domain(ty) *);                                  \
  bool bennet_domain_check_##ty(ty, bennet_domain(ty) *);                                \
                                                                                         \
  bennet_domain(ty) *                                                                    \
      bennet_domain_from_assignment_##ty(void *base_ptr, void *addr, size_t bytes);

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

#define bennet_domain_arbitrary(ty, cs) (bennet_domain_arbitrary_##ty(cs))
#define bennet_domain_check(ty, cs)     (bennet_domain_check_##ty(cs))

#define BENNET_DOMAIN_CAST_DECL(from_ty, to_ty)                                          \
  bennet_domain(to_ty) *                                                                 \
      bennet_domain_cast_##from_ty##_to_##to_ty(bennet_domain(from_ty) *);

#define bennet_domain_cast(from_ty, to_ty, cs)                                           \
  (bennet_domain_cast_##from_ty##_to_##to_ty(cs))

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DOMAIN_H
