#include <bennet/internals/domain.h>
#include <bennet/internals/domains/ownership.h>

extern "C" {
#define BENNET_DOMAIN_INDIRECTION(ty)                                                    \
  bennet_domain(ty) {                                                                    \
    bennet_domain_ownership(ty) car;                                                     \
  };                                                                                     \
                                                                                         \
  bennet_domain(ty) * bennet_domain_top_##ty(void) {                                     \
    return (bennet_domain(ty) *)bennet_domain_ownership_top(ty);                         \
  }                                                                                      \
  bool bennet_domain_is_top_##ty(bennet_domain(ty) * cs) {                               \
    return bennet_domain_ownership_is_top(ty, &cs->car);                                 \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) * bennet_domain_bottom_##ty(void) {                                  \
    return (bennet_domain(ty) *)bennet_domain_ownership_bottom(ty);                      \
  }                                                                                      \
  bool bennet_domain_is_bottom_##ty(bennet_domain(ty) * cs) {                            \
    return bennet_domain_ownership_is_bottom(ty, &cs->car);                              \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_leq_##ty(bennet_domain(ty) * cs1, bennet_domain(ty) * cs2) {        \
    return bennet_domain_ownership_leq_##ty(&cs1->car, &cs2->car);                       \
  }                                                                                      \
  bool bennet_domain_equal_##ty(bennet_domain(ty) * cs1, bennet_domain(ty) * cs2) {      \
    return bennet_domain_ownership_equal_##ty(&cs1->car, &cs2->car);                     \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) *                                                                    \
      bennet_domain_join_##ty(bennet_domain(ty) * cs1, bennet_domain(ty) * cs2) {        \
    return (bennet_domain(ty) *)bennet_domain_ownership_join_##ty(&cs1->car, &cs2->car); \
  }                                                                                      \
  bennet_domain(ty) *                                                                    \
      bennet_domain_meet_##ty(bennet_domain(ty) * cs1, bennet_domain(ty) * cs2) {        \
    return (bennet_domain(ty) *)bennet_domain_ownership_meet_##ty(&cs1->car, &cs2->car); \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) * bennet_domain_copy_##ty(bennet_domain(ty) * cs) {                  \
    return (bennet_domain(ty) *)bennet_domain_ownership_copy_##ty(&cs->car);             \
  }                                                                                      \
  ty bennet_domain_arbitrary_##ty(bennet_domain(ty) * cs) {                              \
    return bennet_domain_ownership_arbitrary_##ty(&cs->car);                             \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) *                                                                    \
      bennet_domain_from_assignment_##ty(void *base_ptr, void *addr, size_t bytes) {     \
    return (bennet_domain(ty) *)bennet_domain_ownership_from_assignment_##ty(            \
        base_ptr, addr, bytes);                                                          \
  }

BENNET_DOMAIN_INDIRECTION(int8_t)
BENNET_DOMAIN_INDIRECTION(uint8_t)
BENNET_DOMAIN_INDIRECTION(int16_t)
BENNET_DOMAIN_INDIRECTION(uint16_t)
BENNET_DOMAIN_INDIRECTION(int32_t)
BENNET_DOMAIN_INDIRECTION(uint32_t)
BENNET_DOMAIN_INDIRECTION(int64_t)
BENNET_DOMAIN_INDIRECTION(uint64_t)
BENNET_DOMAIN_INDIRECTION(uintptr_t)
}
