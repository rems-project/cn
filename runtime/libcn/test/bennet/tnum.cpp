/**
 * @file tnum.cpp
 * @brief Tests for the Tristate Number (tnum) abstract domain
 *
 * Tristate numbers track bit-level knowledge with (value, mask) pairs where:
 * - value = bits known to be 1
 * - mask = bits that are unknown (could be 0 or 1)
 * - Invariant: value & mask == 0
 *
 * Concretization: γ(v, m) = { c | c & ~m == v }
 */

#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/internals/domains/tnum.h>
#include <bennet/prelude.h>

// C++ helper functions to replace C macros that use GCC statement expressions
inline uint64_t arbitrary_tnum_u64(uint64_t value, uint64_t mask) {
  struct bennet_domain_tnum_uint64_t tmp = {
      .top = false, .bottom = false, .value = value, .mask = mask};
  return bennet_arbitrary_tnum_uint64_t(&tmp);
}

inline int64_t arbitrary_tnum_i64(int64_t value, int64_t mask) {
  struct bennet_domain_tnum_int64_t tmp = {
      .top = false, .bottom = false, .value = value, .mask = mask};
  return bennet_arbitrary_tnum_int64_t(&tmp);
}

TEST_F(LibBennet, ArbitraryTNumU64) {
  bennet_set_size(15);

  uint64_t value = 0xDEAD;
  uint64_t mask = 0x2102;

  uint64_t val = arbitrary_tnum_u64(value, mask);
  uint64_t min = val;
  uint64_t max = val;

  for (int i = 0; i < 1000; i++) {
    uint64_t val = arbitrary_tnum_u64(value, mask);

    EXPECT_EQ(val & ~mask, value);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, value);
  EXPECT_EQ(max, value | mask);
}

TEST_F(LibBennet, ArbitraryTNumI64) {
  bennet_set_size(15);
  int64_t val = arbitrary_tnum_i64(0xDEAD, 0x2102);
  int64_t min = val;
  int64_t max = val;

  int64_t value = 0xDEAD;
  int64_t mask = 0x2102;

  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_tnum_i64(value, mask);

    EXPECT_EQ(val & ~mask, value);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, value);
  EXPECT_EQ(max, value | mask);
}

// =============================================================================
// Helper functions for creating tnum domains
// =============================================================================

inline bennet_domain_tnum_uint8_t* make_tnum_u8(uint8_t value, uint8_t mask) {
  return bennet_domain_tnum_of_uint8_t(value, mask);
}

inline bennet_domain_tnum_int8_t* make_tnum_s8(int8_t value, int8_t mask) {
  return bennet_domain_tnum_of_int8_t(value, mask);
}

inline bennet_domain_tnum_uint16_t* make_tnum_u16(uint16_t value, uint16_t mask) {
  return bennet_domain_tnum_of_uint16_t(value, mask);
}

inline bennet_domain_tnum_uint32_t* make_tnum_u32(uint32_t value, uint32_t mask) {
  return bennet_domain_tnum_of_uint32_t(value, mask);
}

// =============================================================================
// Basic Creation Tests
// =============================================================================

TEST_F(LibBennet, TNumBottom) {
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(bottom));
  EXPECT_FALSE(bennet_domain_tnum_is_top_uint8_t(bottom));
}

TEST_F(LibBennet, TNumTop) {
  auto top = bennet_domain_tnum_top_uint8_t();
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(top));
  EXPECT_FALSE(bennet_domain_tnum_is_bottom_uint8_t(top));
}

TEST_F(LibBennet, TNumConst) {
  // Create constant 5 (all bits known)
  auto const_5 = make_tnum_u8(5, 0);
  EXPECT_FALSE(bennet_domain_tnum_is_bottom_uint8_t(const_5));
  EXPECT_FALSE(bennet_domain_tnum_is_top_uint8_t(const_5));
  EXPECT_EQ(const_5->value, 5);
  EXPECT_EQ(const_5->mask, 0);
}

TEST_F(LibBennet, TNumOfInterval) {
  // Single value interval [5, 5] should give constant 5
  auto single = bennet_domain_tnum_of_interval_uint8_t(5, 5);
  EXPECT_EQ(single->value, 5);
  EXPECT_EQ(single->mask, 0);

  // Range [4, 7] -> values are 100, 101, 110, 111
  // Common prefix is 1xx, so value=4 (100), mask=3 (011)
  auto range = bennet_domain_tnum_of_interval_uint8_t(4, 7);
  EXPECT_EQ(range->value, 4);
  EXPECT_EQ(range->mask, 3);

  // Empty range [7, 4] should give bottom
  auto empty = bennet_domain_tnum_of_interval_uint8_t(7, 4);
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(empty));
}

TEST_F(LibBennet, TNumOfTnum) {
  // Create tnum with value=5 (101), mask=2 (010)
  // After normalization: value=5 & ~2 = 5 (101)
  auto t = make_tnum_u8(5, 2);
  EXPECT_EQ(t->value, 5);
  EXPECT_EQ(t->mask, 2);
}

// =============================================================================
// Membership Tests
// =============================================================================

TEST_F(LibBennet, TNumMembership) {
  // Constant 5
  auto const_5 = make_tnum_u8(5, 0);
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(5, const_5));
  EXPECT_FALSE(bennet_domain_tnum_check_uint8_t(4, const_5));
  EXPECT_FALSE(bennet_domain_tnum_check_uint8_t(7, const_5));

  // Partial: value=4 (100), mask=3 (011) represents {4, 5, 6, 7}
  auto partial = make_tnum_u8(4, 3);
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(4, partial));
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(5, partial));
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(6, partial));
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(7, partial));
  EXPECT_FALSE(bennet_domain_tnum_check_uint8_t(3, partial));
  EXPECT_FALSE(bennet_domain_tnum_check_uint8_t(8, partial));

  // Top contains everything
  auto top = bennet_domain_tnum_top_uint8_t();
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(0, top));
  EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(255, top));

  // Bottom contains nothing
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  EXPECT_FALSE(bennet_domain_tnum_check_uint8_t(0, bottom));
  EXPECT_FALSE(bennet_domain_tnum_check_uint8_t(5, bottom));
}

// =============================================================================
// Lattice Ordering (leq) Tests
// =============================================================================

TEST_F(LibBennet, TNumLeq) {
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  auto top = bennet_domain_tnum_top_uint8_t();
  auto const_5 = make_tnum_u8(5, 0);
  auto partial = make_tnum_u8(4, 3);  // {4, 5, 6, 7}

  // Bottom is below everything
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(bottom, top));
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(bottom, const_5));
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(bottom, partial));

  // Top is above everything except bottom
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(const_5, top));
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(partial, top));
  EXPECT_FALSE(bennet_domain_tnum_leq_uint8_t(top, const_5));

  // Constants are ordered by refinement
  auto const_7 = make_tnum_u8(7, 0);
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(const_5, partial));  // 5 in {4..7}
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(const_7, partial));  // 7 in {4..7}
}

TEST_F(LibBennet, TNumLeqReflexive) {
  auto const_5 = make_tnum_u8(5, 0);
  auto partial = make_tnum_u8(4, 3);

  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(const_5, const_5));
  EXPECT_TRUE(bennet_domain_tnum_leq_uint8_t(partial, partial));
}

// =============================================================================
// Join Operation Tests
// =============================================================================

TEST_F(LibBennet, TNumJoin) {
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  auto top = bennet_domain_tnum_top_uint8_t();
  auto const_5 = make_tnum_u8(5, 0);
  auto const_6 = make_tnum_u8(6, 0);

  // Join with bottom
  auto join_bottom = bennet_domain_tnum_join_uint8_t(bottom, const_5);
  EXPECT_EQ(join_bottom->value, 5);
  EXPECT_EQ(join_bottom->mask, 0);

  // Join with top
  auto join_top = bennet_domain_tnum_join_uint8_t(top, const_5);
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(join_top));

  // Join of two different constants
  // 5 = 101, 6 = 110 -> differ in all three low bits
  auto join_5_6 = bennet_domain_tnum_join_uint8_t(const_5, const_6);
  EXPECT_FALSE(bennet_domain_tnum_is_bottom_uint8_t(join_5_6));
  // The join should have mask covering the differing bits
  EXPECT_NE(join_5_6->mask, 0);
}

TEST_F(LibBennet, TNumJoinCommutative) {
  auto const_5 = make_tnum_u8(5, 0);
  auto const_6 = make_tnum_u8(6, 0);

  auto join1 = bennet_domain_tnum_join_uint8_t(const_5, const_6);
  auto join2 = bennet_domain_tnum_join_uint8_t(const_6, const_5);

  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(join1, join2));
}

TEST_F(LibBennet, TNumJoinIdempotent) {
  auto partial = make_tnum_u8(4, 3);
  auto join_result = bennet_domain_tnum_join_uint8_t(partial, partial);
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(join_result, partial));
}

// =============================================================================
// Meet Operation Tests
// =============================================================================

TEST_F(LibBennet, TNumMeet) {
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  auto top = bennet_domain_tnum_top_uint8_t();
  auto const_5 = make_tnum_u8(5, 0);
  auto const_6 = make_tnum_u8(6, 0);

  // Meet with bottom
  auto meet_bottom = bennet_domain_tnum_meet_uint8_t(bottom, const_5);
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(meet_bottom));

  // Meet with top
  auto meet_top = bennet_domain_tnum_meet_uint8_t(top, const_5);
  EXPECT_EQ(meet_top->value, const_5->value);
  EXPECT_EQ(meet_top->mask, const_5->mask);

  // Meet of two conflicting constants
  auto meet_conflict = bennet_domain_tnum_meet_uint8_t(const_5, const_6);
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(meet_conflict));

  // Meet of compatible tnums
  auto partial = make_tnum_u8(4, 3);  // {4, 5, 6, 7}
  auto meet_partial = bennet_domain_tnum_meet_uint8_t(const_5, partial);
  EXPECT_EQ(meet_partial->value, 5);
  EXPECT_EQ(meet_partial->mask, 0);
}

TEST_F(LibBennet, TNumMeetCommutative) {
  auto const_5 = make_tnum_u8(5, 0);
  auto partial = make_tnum_u8(4, 3);

  auto meet1 = bennet_domain_tnum_meet_uint8_t(const_5, partial);
  auto meet2 = bennet_domain_tnum_meet_uint8_t(partial, const_5);

  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(meet1, meet2));
}

TEST_F(LibBennet, TNumMeetIdempotent) {
  auto partial = make_tnum_u8(4, 3);
  auto meet_result = bennet_domain_tnum_meet_uint8_t(partial, partial);
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(meet_result, partial));
}

// =============================================================================
// Equality Tests
// =============================================================================

TEST_F(LibBennet, TNumEquality) {
  // Bottom equals bottom
  auto bottom1 = bennet_domain_tnum_bottom_uint8_t();
  auto bottom2 = bennet_domain_tnum_bottom_uint8_t();
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(bottom1, bottom2));

  // Top equals top
  auto top1 = bennet_domain_tnum_top_uint8_t();
  auto top2 = bennet_domain_tnum_top_uint8_t();
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(top1, top2));

  // Top does not equal bottom
  EXPECT_FALSE(bennet_domain_tnum_equal_uint8_t(top1, bottom1));
  EXPECT_FALSE(bennet_domain_tnum_equal_uint8_t(bottom1, top1));

  // Same tnums are equal
  auto t1 = make_tnum_u8(5, 2);
  auto t2 = make_tnum_u8(5, 2);
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(t1, t2));

  // Different tnums are not equal
  auto t3 = make_tnum_u8(5, 3);
  EXPECT_FALSE(bennet_domain_tnum_equal_uint8_t(t1, t3));
}

// =============================================================================
// Bitwise AND Tests
// =============================================================================

TEST_F(LibBennet, TNumAnd) {
  // 5 = 101, 3 = 011 -> 5 & 3 = 001 = 1
  auto const_5 = make_tnum_u8(5, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_and_uint8_t(const_5, const_3);
  EXPECT_EQ(result->value, 1);
  EXPECT_EQ(result->mask, 0);

  // AND with unknown bits
  auto partial = make_tnum_u8(0, 0xff);  // all unknown
  auto and_partial = bennet_domain_tnum_and_uint8_t(const_5, partial);
  // AND with all-unknown should preserve known 0s from const_5
  EXPECT_FALSE(bennet_domain_tnum_is_top_uint8_t(and_partial));
}

TEST_F(LibBennet, TNumAndSoundness) {
  // Test that for constants, AND produces correct result
  for (int a = 0; a < 256; a += 17) {
    for (int b = 0; b < 256; b += 19) {
      auto t1 = make_tnum_u8((uint8_t)a, 0);
      auto t2 = make_tnum_u8((uint8_t)b, 0);
      auto result = bennet_domain_tnum_and_uint8_t(t1, t2);
      EXPECT_EQ(result->value, (uint8_t)(a & b));
      EXPECT_EQ(result->mask, 0);
    }
  }
}

// =============================================================================
// Bitwise OR Tests
// =============================================================================

TEST_F(LibBennet, TNumOr) {
  // 5 = 101, 3 = 011 -> 5 | 3 = 111 = 7
  auto const_5 = make_tnum_u8(5, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_or_uint8_t(const_5, const_3);
  EXPECT_EQ(result->value, 7);
  EXPECT_EQ(result->mask, 0);
}

TEST_F(LibBennet, TNumOrSoundness) {
  // Test that for constants, OR produces correct result
  for (int a = 0; a < 256; a += 17) {
    for (int b = 0; b < 256; b += 19) {
      auto t1 = make_tnum_u8((uint8_t)a, 0);
      auto t2 = make_tnum_u8((uint8_t)b, 0);
      auto result = bennet_domain_tnum_or_uint8_t(t1, t2);
      EXPECT_EQ(result->value, (uint8_t)(a | b));
      EXPECT_EQ(result->mask, 0);
    }
  }
}

// =============================================================================
// Bitwise XOR Tests
// =============================================================================

TEST_F(LibBennet, TNumXor) {
  // 5 = 101, 3 = 011 -> 5 ^ 3 = 110 = 6
  auto const_5 = make_tnum_u8(5, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_xor_uint8_t(const_5, const_3);
  EXPECT_EQ(result->value, 6);
  EXPECT_EQ(result->mask, 0);

  // XOR with unknown bits
  auto partial = make_tnum_u8(1, 2);  // value=1 (001), mask=2 (010)
  auto xor_partial = bennet_domain_tnum_xor_uint8_t(const_5, partial);
  // Unknown bits remain unknown
  EXPECT_EQ(xor_partial->mask, 2);
}

TEST_F(LibBennet, TNumXorSoundness) {
  // Test that for constants, XOR produces correct result
  for (int a = 0; a < 256; a += 17) {
    for (int b = 0; b < 256; b += 19) {
      auto t1 = make_tnum_u8((uint8_t)a, 0);
      auto t2 = make_tnum_u8((uint8_t)b, 0);
      auto result = bennet_domain_tnum_xor_uint8_t(t1, t2);
      EXPECT_EQ(result->value, (uint8_t)(a ^ b));
      EXPECT_EQ(result->mask, 0);
    }
  }
}

// =============================================================================
// Bitwise NOT Tests
// =============================================================================

TEST_F(LibBennet, TNumNot) {
  // 5 = 00000101 -> ~5 = 11111010 = 250
  auto const_5 = make_tnum_u8(5, 0);
  auto result = bennet_domain_tnum_not_uint8_t(const_5);
  EXPECT_EQ(result->value, 250);
  EXPECT_EQ(result->mask, 0);
}

// =============================================================================
// Addition Tests
// =============================================================================

TEST_F(LibBennet, TNumAdd) {
  // 5 + 3 = 8
  auto const_5 = make_tnum_u8(5, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_add_uint8_t(const_5, const_3);
  EXPECT_EQ(result->value, 8);
  EXPECT_EQ(result->mask, 0);

  // Addition with unknown bits
  auto partial = make_tnum_u8(4, 3);  // {4, 5, 6, 7}
  auto add_partial = bennet_domain_tnum_add_uint8_t(const_5, partial);
  // 5 + {4..7} = {9..12}
  EXPECT_NE(add_partial->mask, 0);  // Should have unknown bits
}

TEST_F(LibBennet, TNumAddSoundness) {
  // Test that for constants, ADD produces correct result
  for (int a = 0; a < 128; a += 11) {
    for (int b = 0; b < 128; b += 13) {
      auto t1 = make_tnum_u8((uint8_t)a, 0);
      auto t2 = make_tnum_u8((uint8_t)b, 0);
      auto result = bennet_domain_tnum_add_uint8_t(t1, t2);
      EXPECT_EQ(result->value, (uint8_t)((a + b) & 0xff));
      EXPECT_EQ(result->mask, 0);
    }
  }
}

// =============================================================================
// Subtraction Tests
// =============================================================================

TEST_F(LibBennet, TNumSub) {
  // 8 - 3 = 5
  auto const_8 = make_tnum_u8(8, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_sub_uint8_t(const_8, const_3);
  EXPECT_EQ(result->value, 5);
  EXPECT_EQ(result->mask, 0);
}

// =============================================================================
// Left Shift Tests
// =============================================================================

TEST_F(LibBennet, TNumShl) {
  // 5 << 2 = 20
  auto const_5 = make_tnum_u8(5, 0);
  auto shift_2 = make_tnum_u8(2, 0);
  auto result = bennet_domain_tnum_shl_uint8_t(const_5, shift_2);
  EXPECT_EQ(result->value, 20);
  EXPECT_EQ(result->mask, 0);

  // Shift with unknown bits in value
  auto partial = make_tnum_u8(4, 3);  // value=4, mask=3
  auto shl_partial = bennet_domain_tnum_shl_uint8_t(partial, shift_2);
  // Mask should be shifted too
  EXPECT_EQ(shl_partial->mask, 12);   // 3 << 2 = 12
  EXPECT_EQ(shl_partial->value, 16);  // 4 << 2 = 16

  // Non-constant shift should return top
  auto shift_var = make_tnum_u8(1, 1);  // unknown shift
  auto shl_var = bennet_domain_tnum_shl_uint8_t(const_5, shift_var);
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(shl_var));
}

// =============================================================================
// Right Shift Tests
// =============================================================================

TEST_F(LibBennet, TNumLshr) {
  // 20 >> 2 = 5
  auto const_20 = make_tnum_u8(20, 0);
  auto shift_2 = make_tnum_u8(2, 0);
  auto result = bennet_domain_tnum_lshr_uint8_t(const_20, shift_2);
  EXPECT_EQ(result->value, 5);
  EXPECT_EQ(result->mask, 0);

  // Non-constant shift should return top
  auto shift_var = make_tnum_u8(1, 1);
  auto lshr_var = bennet_domain_tnum_lshr_uint8_t(const_20, shift_var);
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(lshr_var));
}

// =============================================================================
// Multiplication Tests
// =============================================================================

TEST_F(LibBennet, TNumMul) {
  // 5 * 3 = 15
  auto const_5 = make_tnum_u8(5, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_mul_uint8_t(const_5, const_3);
  EXPECT_EQ(result->value, 15);
  EXPECT_EQ(result->mask, 0);

  // Multiplication by 0
  auto const_0 = make_tnum_u8(0, 0);
  auto mul_0 = bennet_domain_tnum_mul_uint8_t(const_5, const_0);
  EXPECT_EQ(mul_0->value, 0);
  EXPECT_EQ(mul_0->mask, 0);

  // Multiplication with unknown should return top
  auto partial = make_tnum_u8(4, 3);
  auto mul_partial = bennet_domain_tnum_mul_uint8_t(const_5, partial);
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(mul_partial));
}

// =============================================================================
// Division Tests
// =============================================================================

TEST_F(LibBennet, TNumDiv) {
  // 15 / 3 = 5
  auto const_15 = make_tnum_u8(15, 0);
  auto const_3 = make_tnum_u8(3, 0);
  auto result = bennet_domain_tnum_div_uint8_t(const_15, const_3);
  EXPECT_EQ(result->value, 5);
  EXPECT_EQ(result->mask, 0);

  // Division by 0
  auto const_0 = make_tnum_u8(0, 0);
  auto div_0 = bennet_domain_tnum_div_uint8_t(const_15, const_0);
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(div_0));

  // Division with unknown divisor should return top
  auto partial = make_tnum_u8(4, 3);
  auto div_partial = bennet_domain_tnum_div_uint8_t(const_15, partial);
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(div_partial));
}

// =============================================================================
// Modulo Tests
// =============================================================================

TEST_F(LibBennet, TNumMod) {
  // 17 % 5 = 2
  auto const_17 = make_tnum_u8(17, 0);
  auto const_5 = make_tnum_u8(5, 0);
  auto result = bennet_domain_tnum_mod_uint8_t(const_17, const_5);
  EXPECT_EQ(result->value, 2);
  EXPECT_EQ(result->mask, 0);

  // Modulo by 0
  auto const_0 = make_tnum_u8(0, 0);
  auto mod_0 = bennet_domain_tnum_mod_uint8_t(const_17, const_0);
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(mod_0));
}

// =============================================================================
// Copy Tests
// =============================================================================

TEST_F(LibBennet, TNumCopy) {
  auto original = make_tnum_u8(5, 2);
  auto copy = bennet_domain_tnum_copy_uint8_t(original);

  // Copy should equal original
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(original, copy));

  // But they should be different pointers
  EXPECT_NE(original, copy);

  // Test copying top and bottom
  auto top = bennet_domain_tnum_top_uint8_t();
  auto top_copy = bennet_domain_tnum_copy_uint8_t(top);
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(top, top_copy));

  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  auto bottom_copy = bennet_domain_tnum_copy_uint8_t(bottom);
  EXPECT_TRUE(bennet_domain_tnum_equal_uint8_t(bottom, bottom_copy));
}

// =============================================================================
// Absorbing Element Tests
// =============================================================================

TEST_F(LibBennet, TNumBottomAbsorbingForMeet) {
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  auto partial = make_tnum_u8(5, 2);

  auto result = bennet_domain_tnum_meet_uint8_t(partial, bottom);
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(result));
}

TEST_F(LibBennet, TNumTopAbsorbingForJoin) {
  auto top = bennet_domain_tnum_top_uint8_t();
  auto partial = make_tnum_u8(5, 2);

  auto result = bennet_domain_tnum_join_uint8_t(partial, top);
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint8_t(result));
}

// =============================================================================
// 16-bit and 32-bit Tests
// =============================================================================

TEST_F(LibBennet, TNum16BitBasic) {
  auto bottom = bennet_domain_tnum_bottom_uint16_t();
  auto top = bennet_domain_tnum_top_uint16_t();
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint16_t(bottom));
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint16_t(top));

  auto const_100 = make_tnum_u16(100, 0);
  EXPECT_EQ(const_100->value, 100);
  EXPECT_EQ(const_100->mask, 0);

  // Test AND
  auto const_0xf0f0 = make_tnum_u16(0xf0f0, 0);
  auto const_0x0ff0 = make_tnum_u16(0x0ff0, 0);
  auto and_result = bennet_domain_tnum_and_uint16_t(const_0xf0f0, const_0x0ff0);
  EXPECT_EQ(and_result->value, 0x00f0);
  EXPECT_EQ(and_result->mask, 0);
}

TEST_F(LibBennet, TNum32BitBasic) {
  auto bottom = bennet_domain_tnum_bottom_uint32_t();
  auto top = bennet_domain_tnum_top_uint32_t();
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint32_t(bottom));
  EXPECT_TRUE(bennet_domain_tnum_is_top_uint32_t(top));

  auto const_1000000 = make_tnum_u32(1000000, 0);
  EXPECT_EQ(const_1000000->value, 1000000U);
  EXPECT_EQ(const_1000000->mask, 0U);

  // Test multiplication
  auto const_1000 = make_tnum_u32(1000, 0);
  auto mul_result = bennet_domain_tnum_mul_uint32_t(const_1000, const_1000);
  EXPECT_EQ(mul_result->value, 1000000U);
  EXPECT_EQ(mul_result->mask, 0U);
}

// =============================================================================
// Signed Type Tests
// =============================================================================

TEST_F(LibBennet, TNumSignedBasic) {
  auto bottom = bennet_domain_tnum_bottom_int8_t();
  auto top = bennet_domain_tnum_top_int8_t();
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_int8_t(bottom));
  EXPECT_TRUE(bennet_domain_tnum_is_top_int8_t(top));

  // Create constant -5 (treated as bit pattern 251)
  auto const_neg5 = make_tnum_s8(-5, 0);
  EXPECT_FALSE(bennet_domain_tnum_is_bottom_int8_t(const_neg5));
  EXPECT_FALSE(bennet_domain_tnum_is_top_int8_t(const_neg5));
}

// =============================================================================
// Arbitrary Generation Tests
// =============================================================================

TEST_F(LibBennet, TNumArbitraryConstant) {
  auto const_42 = make_tnum_u8(42, 0);
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_domain_tnum_arbitrary_uint8_t(const_42);
    EXPECT_EQ(val, 42);
  }
}

TEST_F(LibBennet, TNumArbitraryPartial) {
  // value=4 (100), mask=3 (011) represents {4, 5, 6, 7}
  auto partial = make_tnum_u8(4, 3);
  bennet_set_size(15);
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_domain_tnum_arbitrary_uint8_t(partial);
    EXPECT_GE(val, 4);
    EXPECT_LE(val, 7);
    EXPECT_TRUE(bennet_domain_tnum_check_uint8_t(val, partial));
  }
}

TEST_F(LibBennet, TNumArbitraryTop) {
  auto top = bennet_domain_tnum_top_uint8_t();
  bennet_set_size(15);
  // Should generate various values
  uint8_t min = 255, max = 0;
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_domain_tnum_arbitrary_uint8_t(top);
    if (val < min)
      min = val;
    if (val > max)
      max = val;
  }
  // With top and enough iterations, we should see some variation
  EXPECT_LT(min, max);
}

// =============================================================================
// Bottom Propagation Tests
// =============================================================================

// =============================================================================
// Tagged Domain Transformer Tests
// =============================================================================

#include <bennet/internals/domain.h>
#include <cn-smt/terms.h>

// Helper: Create a tagged tnum domain for uint8_t
inline bennet_tagged_domain make_tagged_tnum_u8(uint8_t value, uint8_t mask) {
  auto* dom = bennet_domain_tnum_of_uint8_t(value, mask);
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(false, 8);
  return bennet_tagged_domain_create(type, dom);
}

// Helper: Create a tagged tnum top domain for uint8_t
inline bennet_tagged_domain make_tagged_tnum_u8_top() {
  auto* dom = bennet_domain_tnum_top_uint8_t();
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(false, 8);
  return bennet_tagged_domain_create(type, dom);
}

// Helper: Extract tnum from tagged domain
inline bennet_domain_tnum_uint8_t* extract_tnum_u8(bennet_tagged_domain* td) {
  return (bennet_domain_tnum_uint8_t*)td->domain;
}

TEST_F(LibBennet, TNumTransformForwardConst) {
  // Constant 42 -> tnum(42, 0)
  cn_term* term = cn_smt_bits(false, 8, 42);
  bennet_absint_state* state = bennet_absint_state_create();

  bennet_tagged_domain result = bennet_tnum_transform_forward(term, state);
  auto* dom = extract_tnum_u8(&result);
  EXPECT_FALSE(dom->top);
  EXPECT_FALSE(dom->bottom);
  EXPECT_EQ(dom->value, 42);
  EXPECT_EQ(dom->mask, 0);
}

TEST_F(LibBennet, TNumTransformForwardSym) {
  // Symbol with tnum domain -> same domain returned
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* term = cn_smt_sym_string("x", bt);
  bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};

  bennet_absint_state* state = bennet_absint_state_create();
  bennet_tagged_domain td = make_tagged_tnum_u8(0x0A, 0x05);  // value=10, mask=5
  state = bennet_absint_state_set_tnum(state, sym, td);

  bennet_tagged_domain result = bennet_tnum_transform_forward(term, state);
  auto* dom = extract_tnum_u8(&result);
  EXPECT_EQ(dom->value, 0x0A);
  EXPECT_EQ(dom->mask, 0x05);
}

TEST_F(LibBennet, TNumTransformForwardAnd) {
  // AND of two tnums: (value=5 (101), mask=0) AND (value=3 (011), mask=0) = (value=1, mask=0)
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* left_sym = cn_smt_sym_string("a", bt);
  cn_term* right_sym = cn_smt_sym_string("b", bt);
  cn_term* and_term = cn_smt_bw_and(left_sym, right_sym);

  bennet_absint_sym a_sym = {
      .name = left_sym->data.sym.name, .id = left_sym->data.sym.id};
  bennet_absint_sym b_sym = {
      .name = right_sym->data.sym.name, .id = right_sym->data.sym.id};

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, a_sym, make_tagged_tnum_u8(5, 0));
  state = bennet_absint_state_set_tnum(state, b_sym, make_tagged_tnum_u8(3, 0));

  bennet_tagged_domain result = bennet_tnum_transform_forward(and_term, state);
  auto* dom = extract_tnum_u8(&result);
  EXPECT_EQ(dom->value, 1);  // 101 & 011 = 001
  EXPECT_EQ(dom->mask, 0);
}

TEST_F(LibBennet, TNumTransformForwardOr) {
  // OR of two constant tnums: 5 | 3 = 7
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* left_sym = cn_smt_sym_string("a", bt);
  cn_term* right_sym = cn_smt_sym_string("b", bt);
  cn_term* or_term = cn_smt_bw_or(left_sym, right_sym);

  bennet_absint_sym a_sym = {
      .name = left_sym->data.sym.name, .id = left_sym->data.sym.id};
  bennet_absint_sym b_sym = {
      .name = right_sym->data.sym.name, .id = right_sym->data.sym.id};

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, a_sym, make_tagged_tnum_u8(5, 0));
  state = bennet_absint_state_set_tnum(state, b_sym, make_tagged_tnum_u8(3, 0));

  bennet_tagged_domain result = bennet_tnum_transform_forward(or_term, state);
  auto* dom = extract_tnum_u8(&result);
  EXPECT_EQ(dom->value, 7);  // 101 | 011 = 111
  EXPECT_EQ(dom->mask, 0);
}

TEST_F(LibBennet, TNumTransformForwardAdd) {
  // ADD of two constant tnums: 5 + 3 = 8
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* left_sym = cn_smt_sym_string("a", bt);
  cn_term* right_sym = cn_smt_sym_string("b", bt);
  cn_term* add_term = cn_smt_add(left_sym, right_sym);

  bennet_absint_sym a_sym = {
      .name = left_sym->data.sym.name, .id = left_sym->data.sym.id};
  bennet_absint_sym b_sym = {
      .name = right_sym->data.sym.name, .id = right_sym->data.sym.id};

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, a_sym, make_tagged_tnum_u8(5, 0));
  state = bennet_absint_state_set_tnum(state, b_sym, make_tagged_tnum_u8(3, 0));

  bennet_tagged_domain result = bennet_tnum_transform_forward(add_term, state);
  auto* dom = extract_tnum_u8(&result);
  EXPECT_EQ(dom->value, 8);
  EXPECT_EQ(dom->mask, 0);
}

TEST_F(LibBennet, TNumTransformForwardAddWithUnknown) {
  // ADD with unknown bits: (value=4, mask=3) + (value=0, mask=0) constant 0
  // {4,5,6,7} + 0 = {4,5,6,7}
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* left_sym = cn_smt_sym_string("a", bt);
  cn_term* right_sym = cn_smt_sym_string("b", bt);
  cn_term* add_term = cn_smt_add(left_sym, right_sym);

  bennet_absint_sym a_sym = {
      .name = left_sym->data.sym.name, .id = left_sym->data.sym.id};
  bennet_absint_sym b_sym = {
      .name = right_sym->data.sym.name, .id = right_sym->data.sym.id};

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, a_sym, make_tagged_tnum_u8(4, 3));
  state = bennet_absint_state_set_tnum(state, b_sym, make_tagged_tnum_u8(0, 0));

  bennet_tagged_domain result = bennet_tnum_transform_forward(add_term, state);
  auto* dom = extract_tnum_u8(&result);
  EXPECT_EQ(dom->value, 4);
  EXPECT_EQ(dom->mask, 3);
}

TEST_F(LibBennet, TNumTransformForwardArrayShift) {
  // base tnum + elem_size * index tnum
  // base = constant 100, elem_size = 4, index = constant 2
  // result = 100 + 4 * 2 = 108
  cn_base_type loc_type = cn_base_type_simple(CN_BASE_LOC);
  cn_base_type u64_type = cn_base_type_bits(false, 64);
  cn_term* base_sym = cn_smt_sym_string("base", loc_type);
  cn_term* index_sym = cn_smt_sym_string("idx", u64_type);
  cn_term* shift_term = cn_smt_array_shift(base_sym, 4, index_sym);

  bennet_absint_sym base_s = {
      .name = base_sym->data.sym.name, .id = base_sym->data.sym.id};
  bennet_absint_sym idx_s = {
      .name = index_sym->data.sym.name, .id = index_sym->data.sym.id};

  /* Create tagged domains for 64-bit unsigned */
  auto* base_dom = bennet_domain_tnum_of_uint64_t(100, 0);
  cn_base_type* base_bt = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *base_bt = cn_base_type_simple(CN_BASE_LOC);
  bennet_tagged_domain base_td = bennet_tagged_domain_create(base_bt, base_dom);

  auto* idx_dom = bennet_domain_tnum_of_uint64_t(2, 0);
  cn_base_type* idx_bt = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *idx_bt = cn_base_type_bits(false, 64);
  bennet_tagged_domain idx_td = bennet_tagged_domain_create(idx_bt, idx_dom);

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, base_s, base_td);
  state = bennet_absint_state_set_tnum(state, idx_s, idx_td);

  bennet_tagged_domain result = bennet_tnum_transform_forward(shift_term, state);
  auto* res_dom = (bennet_domain_tnum_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->value, 108u);
  EXPECT_EQ(res_dom->mask, 0u);
}

TEST_F(LibBennet, TNumTransformForwardMemberShift) {
  // base tnum + offset
  // base = constant 200, offset = 16
  // result = 216
  cn_base_type loc_type = cn_base_type_simple(CN_BASE_LOC);
  cn_term* base_sym = cn_smt_sym_string("base", loc_type);
  cn_term* shift_term = cn_smt_member_shift(base_sym, 16);

  bennet_absint_sym base_s = {
      .name = base_sym->data.sym.name, .id = base_sym->data.sym.id};

  auto* base_dom = bennet_domain_tnum_of_uint64_t(200, 0);
  cn_base_type* base_bt = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *base_bt = cn_base_type_simple(CN_BASE_LOC);
  bennet_tagged_domain base_td = bennet_tagged_domain_create(base_bt, base_dom);

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, base_s, base_td);

  bennet_tagged_domain result = bennet_tnum_transform_forward(shift_term, state);
  auto* res_dom = (bennet_domain_tnum_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->value, 216u);
  EXPECT_EQ(res_dom->mask, 0u);
}

TEST_F(LibBennet, TNumTransformBackwardAssumeEq) {
  // x == 5 must be true -> refine x to constant 5
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* x_sym = cn_smt_sym_string("x", bt);
  cn_term* const_5 = cn_smt_bits(false, 8, 5);
  cn_term* eq_term = cn_smt_eq(x_sym, const_5);

  bennet_absint_sym x_s = {.name = x_sym->data.sym.name, .id = x_sym->data.sym.id};

  bennet_absint_state* state = bennet_absint_state_create();
  state = bennet_absint_state_set_tnum(state, x_s, make_tagged_tnum_u8_top());

  bennet_absint_state* refined =
      bennet_tnum_transform_backward_assume(eq_term, true, state);
  bennet_tagged_domain x_dom = bennet_absint_state_get_tnum(refined, x_s, &bt);
  auto* dom = extract_tnum_u8(&x_dom);
  EXPECT_EQ(dom->value, 5);
  EXPECT_EQ(dom->mask, 0);
}

// =============================================================================
// Bottom Propagation Tests
// =============================================================================

TEST_F(LibBennet, TNumBottomPropagation) {
  auto bottom = bennet_domain_tnum_bottom_uint8_t();
  auto const_5 = make_tnum_u8(5, 0);

  // All operations with bottom should return bottom
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_and_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_or_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_xor_uint8_t(bottom, const_5)));
  EXPECT_TRUE(
      bennet_domain_tnum_is_bottom_uint8_t(bennet_domain_tnum_not_uint8_t(bottom)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_add_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_sub_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_mul_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_div_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_mod_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_shl_uint8_t(bottom, const_5)));
  EXPECT_TRUE(bennet_domain_tnum_is_bottom_uint8_t(
      bennet_domain_tnum_lshr_uint8_t(bottom, const_5)));
}
