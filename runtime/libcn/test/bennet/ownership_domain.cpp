#include <bennet/internals/domain.h>
#include <bennet/internals/domains/ownership.h>

extern "C" {
#define BENNET_DOMAIN_INDIRECTION(ty)                                                    \
  bennet_domain(ty) {                                                                    \
    bennet_domain_ownership(ty) car;                                                     \
  };                                                                                     \
                                                                                         \
  bennet_domain(ty) * bennet_domain_top_##ty(void) {                                     \
    return (bennet_domain(ty)*)bennet_domain_ownership_top(ty);                          \
  }                                                                                      \
  bool bennet_domain_is_top_##ty(bennet_domain(ty) * cs) {                               \
    return bennet_domain_ownership_is_top(ty, &cs->car);                                 \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) * bennet_domain_bottom_##ty(void) {                                  \
    return (bennet_domain(ty)*)bennet_domain_ownership_bottom(ty);                       \
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
    return (bennet_domain(ty)*)bennet_domain_ownership_join_##ty(&cs1->car, &cs2->car);  \
  }                                                                                      \
  bennet_domain(ty) *                                                                    \
      bennet_domain_meet_##ty(bennet_domain(ty) * cs1, bennet_domain(ty) * cs2) {        \
    return (bennet_domain(ty)*)bennet_domain_ownership_meet_##ty(&cs1->car, &cs2->car);  \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) * bennet_domain_copy_##ty(bennet_domain(ty) * cs) {                  \
    return (bennet_domain(ty)*)bennet_domain_ownership_copy_##ty(&cs->car);              \
  }                                                                                      \
  ty bennet_domain_arbitrary_##ty(bennet_domain(ty) * cs) {                              \
    return bennet_domain_ownership_arbitrary_##ty(&cs->car);                             \
  }                                                                                      \
                                                                                         \
  bennet_domain(ty) *                                                                    \
      bennet_domain_from_assignment_##ty(void* base_ptr, void* addr, size_t bytes) {     \
    return (bennet_domain(ty)*)bennet_domain_ownership_from_assignment_##ty(             \
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

#include "harness.hpp"
#include <gtest/gtest.h>

// Forward member_shift tests
TEST_F(LibBennet, OwnershipMemberShiftForward) {
  // base: {before=10, after=20}, offset=5
  // result: {before=15, after=15}
  auto* base = bennet_domain_ownership_of(uintptr_t, 10, 20);
  auto* result = bennet_ownership_member_shift(uintptr_t, base, 5);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 15u);
  EXPECT_EQ(result->after, 15u);
}

TEST_F(LibBennet, OwnershipMemberShiftForwardExact) {
  // Shift exactly to the end of allocation
  auto* base = bennet_domain_ownership_of(uintptr_t, 10, 20);
  auto* result = bennet_ownership_member_shift(uintptr_t, base, 20);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 30u);
  EXPECT_EQ(result->after, 0u);
}

TEST_F(LibBennet, OwnershipMemberShiftForwardPastEnd) {
  // Shifting past allocation -> bottom
  auto* base = bennet_domain_ownership_of(uintptr_t, 10, 20);
  auto* result = bennet_ownership_member_shift(uintptr_t, base, 25);
  EXPECT_TRUE(result->bottom);
}

TEST_F(LibBennet, OwnershipMemberShiftForwardTop) {
  // Top stays top
  auto* base = bennet_domain_ownership_top(uintptr_t);
  auto* result = bennet_ownership_member_shift(uintptr_t, base, 5);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 0u);
  EXPECT_EQ(result->after, 0u);
}

TEST_F(LibBennet, OwnershipMemberShiftForwardBottom) {
  // Bottom stays bottom
  auto* base = bennet_domain_ownership_bottom(uintptr_t);
  auto* result = bennet_ownership_member_shift(uintptr_t, base, 5);
  EXPECT_TRUE(result->bottom);
}

// Forward array_shift tests
TEST_F(LibBennet, OwnershipArrayShiftForward) {
  // base: {before=0, after=40}, elem_size=4, index=3
  // offset = 12, result: {before=12, after=28}
  auto* base = bennet_domain_ownership_of(uintptr_t, 0, 40);
  auto* result = bennet_ownership_array_shift(uintptr_t, base, 4, 3);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 12u);
  EXPECT_EQ(result->after, 28u);
}

TEST_F(LibBennet, OwnershipArrayShiftForwardZeroIndex) {
  // Zero index -> no shift
  auto* base = bennet_domain_ownership_of(uintptr_t, 10, 30);
  auto* result = bennet_ownership_array_shift(uintptr_t, base, 4, 0);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 10u);
  EXPECT_EQ(result->after, 30u);
}

// Backward member_shift tests
TEST_F(LibBennet, OwnershipMemberShiftBackward) {
  // shifted: {before=15, after=15}, offset=5
  // base: {before=10, after=20}
  auto* shifted = bennet_domain_ownership_of(uintptr_t, 15, 15);
  auto* result = bennet_ownership_member_shift_backward(uintptr_t, shifted, 5);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 10u);
  EXPECT_EQ(result->after, 20u);
}

TEST_F(LibBennet, OwnershipMemberShiftBackwardOffsetExceedsBefore) {
  // offset > before -> base is before the owned range, but can still be
  // the start of a larger allocation: {before=0, after=15+5=20}
  auto* shifted = bennet_domain_ownership_of(uintptr_t, 3, 15);
  auto* result = bennet_ownership_member_shift_backward(uintptr_t, shifted, 5);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 0u);
  EXPECT_EQ(result->after, 20u);
}

// Backward array_shift tests
TEST_F(LibBennet, OwnershipArrayShiftBackward) {
  // shifted: {before=12, after=28}, elem_size=4, index=3
  // base: {before=0, after=40}
  auto* shifted = bennet_domain_ownership_of(uintptr_t, 12, 28);
  auto* result = bennet_ownership_array_shift_backward(uintptr_t, shifted, 4, 3);
  EXPECT_FALSE(result->bottom);
  EXPECT_EQ(result->before, 0u);
  EXPECT_EQ(result->after, 40u);
}

// Round-trip tests
TEST_F(LibBennet, OwnershipMemberShiftRoundTrip) {
  // forward then backward should give back original
  auto* base = bennet_domain_ownership_of(uintptr_t, 10, 20);
  auto* shifted = bennet_ownership_member_shift(uintptr_t, base, 5);
  auto* recovered = bennet_ownership_member_shift_backward(uintptr_t, shifted, 5);
  EXPECT_FALSE(recovered->bottom);
  EXPECT_EQ(recovered->before, base->before);
  EXPECT_EQ(recovered->after, base->after);
}
