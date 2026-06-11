/**
 * @file congr.cpp
 * @brief Tests for the Congruence abstract domain
 *
 * Congruence domain represents sets of integers congruent to b modulo a (aZ + b).
 * - modulus = 0: singleton {residue}
 * - modulus = 1: top (all values, after xi-normalization)
 * - modulus > 1: power-of-2 stride (after xi-normalization)
 *
 * Check: v in aZ+b iff (v - b) mod a == 0, or (v - b) & (a - 1) == 0 for pow2 a.
 */

#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/internals/domains/congr.h>
#include <bennet/prelude.h>

// =============================================================================
// Helper functions
// =============================================================================

inline bennet_domain_congr_uint8_t* make_congr_u8(uint8_t modulus, uint8_t residue) {
  return bennet_domain_congr_of_uint8_t(modulus, residue);
}

inline bennet_domain_congr_uint16_t* make_congr_u16(uint16_t modulus, uint16_t residue) {
  return bennet_domain_congr_of_uint16_t(modulus, residue);
}

inline bennet_domain_congr_uint32_t* make_congr_u32(uint32_t modulus, uint32_t residue) {
  return bennet_domain_congr_of_uint32_t(modulus, residue);
}

inline bennet_domain_congr_uint64_t* make_congr_u64(uint64_t modulus, uint64_t residue) {
  return bennet_domain_congr_of_uint64_t(modulus, residue);
}

// =============================================================================
// Basic Creation Tests
// =============================================================================

TEST_F(LibBennet, CongrBottom) {
  auto bottom = bennet_domain_congr_bottom_uint8_t();
  EXPECT_TRUE(bennet_domain_congr_is_bottom_uint8_t(bottom));
  EXPECT_FALSE(bennet_domain_congr_is_top_uint8_t(bottom));
}

TEST_F(LibBennet, CongrTop) {
  auto top = bennet_domain_congr_top_uint8_t();
  EXPECT_TRUE(bennet_domain_congr_is_top_uint8_t(top));
  EXPECT_FALSE(bennet_domain_congr_is_bottom_uint8_t(top));
  EXPECT_EQ(top->modulus, 1);
  EXPECT_EQ(top->residue, 0);
}

TEST_F(LibBennet, CongrConst) {
  // Singleton 5: modulus=0, residue=5
  auto const_5 = make_congr_u8(0, 5);
  EXPECT_FALSE(bennet_domain_congr_is_bottom_uint8_t(const_5));
  EXPECT_FALSE(bennet_domain_congr_is_top_uint8_t(const_5));
  EXPECT_EQ(const_5->modulus, 0);
  EXPECT_EQ(const_5->residue, 5);
}

TEST_F(LibBennet, CongrXiNormalization) {
  // 6Z+3 in uint8: gcd(6, 256) = 2, 3 mod 2 = 1 → 2Z+1
  auto t = make_congr_u8(6, 3);
  EXPECT_EQ(t->modulus, 2);
  EXPECT_EQ(t->residue, 1);

  // Power-of-2 modulus stays: 8Z+3 → 8Z+3
  auto t2 = make_congr_u8(8, 3);
  EXPECT_EQ(t2->modulus, 8);
  EXPECT_EQ(t2->residue, 3);
}

// =============================================================================
// Membership (check) Tests
// =============================================================================

TEST_F(LibBennet, CongrMembership) {
  // Singleton 5
  auto const_5 = make_congr_u8(0, 5);
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(5, const_5));
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(4, const_5));
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(6, const_5));

  // 4Z+1 = {1, 5, 9, 13, ..., 253}
  auto stride4 = make_congr_u8(4, 1);
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(1, stride4));
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(5, stride4));
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(9, stride4));
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(253, stride4));
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(0, stride4));
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(2, stride4));
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(4, stride4));

  // Top contains everything
  auto top = bennet_domain_congr_top_uint8_t();
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(0, top));
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(127, top));
  EXPECT_TRUE(bennet_domain_congr_check_uint8_t(255, top));

  // Bottom contains nothing
  auto bottom = bennet_domain_congr_bottom_uint8_t();
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(0, bottom));
  EXPECT_FALSE(bennet_domain_congr_check_uint8_t(5, bottom));
}

// =============================================================================
// Lattice Ordering (leq) Tests
// =============================================================================

TEST_F(LibBennet, CongrLeq) {
  auto bottom = bennet_domain_congr_bottom_uint8_t();
  auto top = bennet_domain_congr_top_uint8_t();
  auto const_5 = make_congr_u8(0, 5);
  auto stride4_1 = make_congr_u8(4, 1);  // 4Z+1

  // Bottom is below everything
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(bottom, top));
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(bottom, const_5));
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(bottom, stride4_1));

  // Everything is below top
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(const_5, top));
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(stride4_1, top));

  // Singleton 5 is in 4Z+1 (since 5 = 4*1+1)
  // But 4Z+1 is NOT <= singleton 5
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(const_5, stride4_1));
  EXPECT_FALSE(bennet_domain_congr_leq_uint8_t(stride4_1, const_5));

  // 8Z+1 <= 4Z+1 (since 4|8 and 1 == 1 mod 4)
  auto stride8_1 = make_congr_u8(8, 1);
  EXPECT_TRUE(bennet_domain_congr_leq_uint8_t(stride8_1, stride4_1));
  EXPECT_FALSE(bennet_domain_congr_leq_uint8_t(stride4_1, stride8_1));
}

// =============================================================================
// Join Tests
// =============================================================================

TEST_F(LibBennet, CongrJoin) {
  // Join of singleton 3 and singleton 7: gcd(0, 0, |3-7|) = gcd(0, 0, 4) = 4
  // 4Z + 3
  auto c3 = make_congr_u8(0, 3);
  auto c7 = make_congr_u8(0, 7);
  auto j = bennet_domain_congr_join_uint8_t(c3, c7);
  EXPECT_EQ(j->modulus, 4);
  EXPECT_EQ(j->residue, 3);

  // Join with bottom returns the other
  auto bottom = bennet_domain_congr_bottom_uint8_t();
  auto j2 = bennet_domain_congr_join_uint8_t(bottom, c3);
  EXPECT_EQ(j2->modulus, 0);
  EXPECT_EQ(j2->residue, 3);

  // Join with top returns top
  auto top = bennet_domain_congr_top_uint8_t();
  auto j3 = bennet_domain_congr_join_uint8_t(c3, top);
  EXPECT_TRUE(bennet_domain_congr_is_top_uint8_t(j3));
}

// =============================================================================
// Meet Tests
// =============================================================================

TEST_F(LibBennet, CongrMeet) {
  // Meet of 4Z+1 and 8Z+1: since 8 > 4, lcm = 8, residue from larger = 1
  auto s4 = make_congr_u8(4, 1);
  auto s8 = make_congr_u8(8, 1);
  auto m = bennet_domain_congr_meet_uint8_t(s4, s8);
  EXPECT_EQ(m->modulus, 8);
  EXPECT_EQ(m->residue, 1);

  // Meet of incompatible: 4Z+1 and 4Z+2 → bottom
  auto s4_2 = make_congr_u8(4, 2);
  auto m2 = bennet_domain_congr_meet_uint8_t(s4, s4_2);
  EXPECT_TRUE(bennet_domain_congr_is_bottom_uint8_t(m2));

  // Meet with top returns the other
  auto top = bennet_domain_congr_top_uint8_t();
  auto m3 = bennet_domain_congr_meet_uint8_t(top, s4);
  EXPECT_EQ(m3->modulus, 4);
  EXPECT_EQ(m3->residue, 1);

  // Meet of same singletons
  auto c5a = make_congr_u8(0, 5);
  auto c5b = make_congr_u8(0, 5);
  auto m4 = bennet_domain_congr_meet_uint8_t(c5a, c5b);
  EXPECT_EQ(m4->modulus, 0);
  EXPECT_EQ(m4->residue, 5);

  // Meet of different singletons → bottom
  auto c7 = make_congr_u8(0, 7);
  auto m5 = bennet_domain_congr_meet_uint8_t(c5a, c7);
  EXPECT_TRUE(bennet_domain_congr_is_bottom_uint8_t(m5));
}

// =============================================================================
// Arbitrary Generation Tests
// =============================================================================

TEST_F(LibBennet, CongrArbitrarySingleton) {
  bennet_set_size(15);
  auto const_42 = make_congr_u8(0, 42);
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_arbitrary_congr_uint8_t(const_42);
    EXPECT_EQ(val, 42);
  }
}

TEST_F(LibBennet, CongrArbitraryStride4) {
  bennet_set_size(15);
  auto stride4 = make_congr_u8(4, 2);  // 4Z+2 = {2, 6, 10, ..., 254}

  for (int i = 0; i < 1000; i++) {
    uint8_t val = bennet_arbitrary_congr_uint8_t(stride4);
    EXPECT_TRUE(bennet_domain_congr_check_uint8_t(val, stride4))
        << "Generated value " << (int)val << " not in 4Z+2";
  }
}

TEST_F(LibBennet, CongrArbitrarySizeBudget) {
  bennet_set_size(2);
  auto stride4 = make_congr_u8(4, 0);  // 4Z+0 = {0, 4, 8, ..., 252}

  // With size budget 2, should only generate from first 3 elements: {0, 4, 8}
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_arbitrary_congr_uint8_t(stride4);
    EXPECT_TRUE(bennet_domain_congr_check_uint8_t(val, stride4));
    EXPECT_LE(val, 8) << "Size budget should limit generation";
  }
}

TEST_F(LibBennet, CongrArbitraryTop) {
  bennet_set_size(20);
  auto top = bennet_domain_congr_top_uint8_t();

  // Top generates all values
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_arbitrary_congr_uint8_t(top);
    EXPECT_TRUE(bennet_domain_congr_check_uint8_t(val, top));
  }
}

// =============================================================================
// Arithmetic Operation Tests
// =============================================================================

TEST_F(LibBennet, CongrAdd) {
  // 4Z+1 + 0Z+3 = gcd(4,0)Z + (1+3) = 4Z + 4 → xi: gcd(4,256)=4, 4 mod 4=0 → 4Z+0
  auto s4 = make_congr_u8(4, 1);
  auto c3 = make_congr_u8(0, 3);
  auto r = bennet_domain_congr_add_uint8_t(s4, c3);
  EXPECT_EQ(r->modulus, 4);
  EXPECT_EQ(r->residue, 0);
}

TEST_F(LibBennet, CongrSub) {
  // 4Z+3 - 0Z+1 = gcd(4,0)Z + (3-1) = 4Z + 2
  auto s4 = make_congr_u8(4, 3);
  auto c1 = make_congr_u8(0, 1);
  auto r = bennet_domain_congr_sub_uint8_t(s4, c1);
  EXPECT_EQ(r->modulus, 4);
  EXPECT_EQ(r->residue, 2);
}

TEST_F(LibBennet, CongrMul) {
  // Singleton mul: 0Z+3 * 0Z+5 = 0Z+15
  auto c3 = make_congr_u8(0, 3);
  auto c5 = make_congr_u8(0, 5);
  auto r = bennet_domain_congr_mul_uint8_t(c3, c5);
  EXPECT_EQ(r->modulus, 0);
  EXPECT_EQ(r->residue, 15);

  // 4Z+3 * 0Z+2: ac=0, ad=8, bc=0. gcd(0,8,0)=8 → 8Z+6
  auto s4 = make_congr_u8(4, 3);
  auto c2 = make_congr_u8(0, 2);
  auto r2 = bennet_domain_congr_mul_uint8_t(s4, c2);
  EXPECT_EQ(r2->modulus, 8);
  EXPECT_EQ(r2->residue, 6);
}

TEST_F(LibBennet, CongrDiv) {
  // 8Z+6 / 0Z+2: 2|8 and 2|6 → 4Z+3
  auto s8 = make_congr_u8(8, 6);
  auto c2 = make_congr_u8(0, 2);
  auto r = bennet_domain_congr_div_uint8_t(s8, c2);
  EXPECT_EQ(r->modulus, 4);
  EXPECT_EQ(r->residue, 3);

  // Division by zero → bottom
  auto c0 = make_congr_u8(0, 0);
  auto r2 = bennet_domain_congr_div_uint8_t(s8, c0);
  EXPECT_TRUE(bennet_domain_congr_is_bottom_uint8_t(r2));
}

TEST_F(LibBennet, CongrMod) {
  // 8Z+6 % 0Z+4: gcd(8,4)=4, 6 mod 4 = 2 → 4Z+2
  auto s8 = make_congr_u8(8, 6);
  auto c4 = make_congr_u8(0, 4);
  auto r = bennet_domain_congr_mod_uint8_t(s8, c4);
  EXPECT_EQ(r->modulus, 4);
  EXPECT_EQ(r->residue, 2);
}

// =============================================================================
// Interval Conversion Tests
// =============================================================================

TEST_F(LibBennet, CongrToInterval) {
  // Singleton 5 → [5, 5]
  auto c5 = make_congr_u8(0, 5);
  uint8_t lo, hi;
  EXPECT_TRUE(bennet_domain_congr_to_interval_uint8_t(c5, &lo, &hi));
  EXPECT_EQ(lo, 5);
  EXPECT_EQ(hi, 5);

  // 4Z+2 = {2, 6, 10, ..., 254} → [2, 254]
  auto s4 = make_congr_u8(4, 2);
  EXPECT_TRUE(bennet_domain_congr_to_interval_uint8_t(s4, &lo, &hi));
  EXPECT_EQ(lo, 2);
  EXPECT_EQ(hi, 254);

  // Top → no interval
  auto top = bennet_domain_congr_top_uint8_t();
  EXPECT_FALSE(bennet_domain_congr_to_interval_uint8_t(top, &lo, &hi));

  // Bottom → no interval
  auto bottom = bennet_domain_congr_bottom_uint8_t();
  EXPECT_FALSE(bennet_domain_congr_to_interval_uint8_t(bottom, &lo, &hi));
}

TEST_F(LibBennet, CongrOfInterval) {
  // Single value [5, 5] → singleton 5
  auto single = bennet_domain_congr_of_interval_uint8_t(5, 5);
  EXPECT_EQ(single->modulus, 0);
  EXPECT_EQ(single->residue, 5);

  // General interval → top (congruence can't represent intervals)
  auto range = bennet_domain_congr_of_interval_uint8_t(3, 7);
  EXPECT_TRUE(bennet_domain_congr_is_top_uint8_t(range));

  // Empty interval → bottom
  auto empty = bennet_domain_congr_of_interval_uint8_t(7, 3);
  EXPECT_TRUE(bennet_domain_congr_is_bottom_uint8_t(empty));
}

// =============================================================================
// Bitwise Operation Tests
// =============================================================================

TEST_F(LibBennet, CongrBitwiseAnd) {
  // Singleton AND: 0Z+7 & 0Z+5 = 0Z+5
  auto c7 = make_congr_u8(0, 7);
  auto c5 = make_congr_u8(0, 5);
  auto r = bennet_domain_congr_and_uint8_t(c7, c5);
  EXPECT_EQ(r->modulus, 0);
  EXPECT_EQ(r->residue, 5);

  // AND with zero → zero
  auto c0 = make_congr_u8(0, 0);
  auto s4 = make_congr_u8(4, 3);
  auto r2 = bennet_domain_congr_and_uint8_t(s4, c0);
  EXPECT_EQ(r2->modulus, 0);
  EXPECT_EQ(r2->residue, 0);
}

TEST_F(LibBennet, CongrBitwiseOr) {
  // Singleton OR: 0Z+5 | 0Z+3 = 0Z+7
  auto c5 = make_congr_u8(0, 5);
  auto c3 = make_congr_u8(0, 3);
  auto r = bennet_domain_congr_or_uint8_t(c5, c3);
  EXPECT_EQ(r->modulus, 0);
  EXPECT_EQ(r->residue, 7);
}

TEST_F(LibBennet, CongrBitwiseXor) {
  // Singleton XOR: 0Z+5 ^ 0Z+3 = 0Z+6
  auto c5 = make_congr_u8(0, 5);
  auto c3 = make_congr_u8(0, 3);
  auto r = bennet_domain_congr_xor_uint8_t(c5, c3);
  EXPECT_EQ(r->modulus, 0);
  EXPECT_EQ(r->residue, 6);
}

// =============================================================================
// Shift Operation Tests
// =============================================================================

TEST_F(LibBennet, CongrShiftLeft) {
  // 4Z+3 << 0Z+1 = gcd(8, 256)Z + ((6) mod 256) = 8Z+6
  auto s4 = make_congr_u8(4, 3);
  auto c1 = make_congr_u8(0, 1);
  auto r = bennet_domain_congr_shl_uint8_t(s4, c1);
  EXPECT_EQ(r->modulus, 8);
  EXPECT_EQ(r->residue, 6);
}

TEST_F(LibBennet, CongrShiftRight) {
  // 8Z+4 >> 0Z+2: 8 & 3 == 0 (cleanly divisible), → 2Z+1
  auto s8 = make_congr_u8(8, 4);
  auto c2 = make_congr_u8(0, 2);
  auto r = bennet_domain_congr_lshr_uint8_t(s8, c2);
  EXPECT_EQ(r->modulus, 2);
  EXPECT_EQ(r->residue, 1);

  // Non-divisible shift → top
  auto s4 = make_congr_u8(4, 3);
  auto c3 = make_congr_u8(0, 3);
  auto r2 = bennet_domain_congr_lshr_uint8_t(s4, c3);
  EXPECT_TRUE(bennet_domain_congr_is_top_uint8_t(r2));
}

// =============================================================================
// Equality and Copy Tests
// =============================================================================

TEST_F(LibBennet, CongrEqual) {
  auto s4a = make_congr_u8(4, 1);
  auto s4b = make_congr_u8(4, 1);
  auto s4c = make_congr_u8(4, 2);

  EXPECT_TRUE(bennet_domain_congr_equal_uint8_t(s4a, s4b));
  EXPECT_FALSE(bennet_domain_congr_equal_uint8_t(s4a, s4c));

  auto top1 = bennet_domain_congr_top_uint8_t();
  auto top2 = bennet_domain_congr_top_uint8_t();
  EXPECT_TRUE(bennet_domain_congr_equal_uint8_t(top1, top2));

  auto bot1 = bennet_domain_congr_bottom_uint8_t();
  auto bot2 = bennet_domain_congr_bottom_uint8_t();
  EXPECT_TRUE(bennet_domain_congr_equal_uint8_t(bot1, bot2));
}

TEST_F(LibBennet, CongrCopy) {
  auto orig = make_congr_u8(4, 3);
  auto copy = bennet_domain_congr_copy_uint8_t(orig);
  EXPECT_TRUE(bennet_domain_congr_equal_uint8_t(orig, copy));
  EXPECT_NE(orig, copy);  // Different pointers
}
