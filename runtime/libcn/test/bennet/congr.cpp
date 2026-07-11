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

#include "absint_test_utils.hpp"
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

// =============================================================================
// Abstract Transformer Tests (bennet_congr_transform_*)
// These use the cn_term walkers over states, mirroring the wint.cpp pattern:
// bump frame guard per test, functional state updates.
// =============================================================================

using absint_test::asym;
using absint_test::negate_term;
using absint_test::tagged_congr_loc;
using absint_test::tagged_congr_u16;
using absint_test::tagged_congr_u64;
using absint_test::tagged_congr_u8;
using absint_test::u8_const;
using absint_test::u8_sym;

namespace {

void expect_congr_u8(bennet_tagged_domain d, uint8_t modulus, uint8_t residue) {
  ASSERT_NE(d.domain, nullptr);
  auto* dom = (bennet_domain_congr_uint8_t*)d.domain;
  EXPECT_FALSE(dom->bottom);
  EXPECT_EQ(dom->modulus, modulus);
  EXPECT_EQ(dom->residue, residue);
}

void expect_congr_u64(bennet_tagged_domain d, uint64_t modulus, uint64_t residue) {
  ASSERT_NE(d.domain, nullptr);
  auto* dom = (bennet_domain_congr_uint64_t*)d.domain;
  EXPECT_FALSE(dom->bottom);
  EXPECT_EQ(dom->modulus, modulus);
  EXPECT_EQ(dom->residue, residue);
}

}  // namespace

// -----------------------------------------------------------------------------
// Forward transformer
// -----------------------------------------------------------------------------

TEST_F(LibBennet, CongrForwardConst) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();

  bennet_tagged_domain result = bennet_congr_transform_forward(u8_const(6), state);
  expect_congr_u8(result, 0, 6);  // singleton {6}

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardSymBound) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));

  bennet_tagged_domain result = bennet_congr_transform_forward(u8_sym(a), state);
  expect_congr_u8(result, 4, 1);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardSymUnboundTop) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");

  bennet_tagged_domain result = bennet_congr_transform_forward(u8_sym(a), state);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&result));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardAddStrides) {
  // (4Z+1) + (4Z+2) = 4Z+3
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(4, 2));

  bennet_tagged_domain result =
      bennet_congr_transform_forward(cn_smt_add(u8_sym(a), u8_sym(b)), state);
  expect_congr_u8(result, 4, 3);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardAddConstWraps) {
  // (8Z+3) + {5} = 8Z+0 (residue 3+5 wraps modulo the stride)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(8, 3));

  bennet_tagged_domain result =
      bennet_congr_transform_forward(cn_smt_add(u8_sym(a), u8_const(5)), state);
  expect_congr_u8(result, 8, 0);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardSubConstsWraps) {
  // {1} - {2} = {255} at u8 (truncation of the 64-bit generic result)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();

  bennet_tagged_domain result =
      bennet_congr_transform_forward(cn_smt_sub(u8_const(1), u8_const(2)), state);
  expect_congr_u8(result, 0, 255);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardMulByConst) {
  // (4Z+1) * {3} = 12Z+3, xi-normalized to 4Z+3
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));

  bennet_tagged_domain result =
      bennet_congr_transform_forward(cn_smt_mul(u8_sym(a), u8_const(3)), state);
  expect_congr_u8(result, 4, 3);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardMulStrides) {
  // (4Z+0) * (8Z+2): gcd(32, 8, 0) = 8 -> 8Z+0
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 0));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(8, 2));

  bennet_tagged_domain result =
      bennet_congr_transform_forward(cn_smt_mul(u8_sym(a), u8_sym(b)), state);
  expect_congr_u8(result, 8, 0);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardDiv) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(8, 4));

  // (8Z+4) / {4} = 2Z+1 (singleton divisor divides both stride and residue)
  bennet_tagged_domain r1 =
      bennet_congr_transform_forward(cn_smt_div(u8_sym(a), u8_const(4)), state);
  expect_congr_u8(r1, 2, 1);

  // (8Z+3) / {2}: divisor does not divide the residue -> top
  auto* state2 = bennet_absint_state_create();
  state2 = bennet_absint_state_set_congr(state2, asym(a), tagged_congr_u8(8, 3));
  bennet_tagged_domain r2 =
      bennet_congr_transform_forward(cn_smt_div(u8_sym(a), u8_const(2)), state2);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&r2));

  // Division by zero -> bottom
  bennet_tagged_domain r3 =
      bennet_congr_transform_forward(cn_smt_div(u8_sym(a), u8_const(0)), state);
  EXPECT_TRUE(bennet_tagged_domain_is_bottom_congr(&r3));

  bennet_absint_state_free(state);
  bennet_absint_state_free(state2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardMod) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(8, 3));

  // (8Z+3) % {4} = 4Z+3 (gcd(8,4)=4)
  bennet_tagged_domain r1 =
      bennet_congr_transform_forward(cn_smt_mod(u8_sym(a), u8_const(4)), state);
  expect_congr_u8(r1, 4, 3);

  // Modulo by zero -> bottom
  bennet_tagged_domain r2 =
      bennet_congr_transform_forward(cn_smt_mod(u8_sym(a), u8_const(0)), state);
  EXPECT_TRUE(bennet_tagged_domain_is_bottom_congr(&r2));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardShl) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(4, 0));

  // (4Z+1) << {2} = 16Z+4
  bennet_tagged_domain r1 =
      bennet_congr_transform_forward(cn_smt_shift_left(u8_sym(a), u8_const(2)), state);
  expect_congr_u8(r1, 16, 4);

  // Shift by a non-singleton amount -> top
  bennet_tagged_domain r2 =
      bennet_congr_transform_forward(cn_smt_shift_left(u8_sym(a), u8_sym(b)), state);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&r2));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardLshr) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(8, 2));

  // (8Z+2) >> {1} = 4Z+1 (exact: {2,10,18,...} >> 1 = {1,5,9,...})
  bennet_tagged_domain r1 =
      bennet_congr_transform_forward(cn_smt_shift_right(u8_sym(a), u8_const(1)), state);
  expect_congr_u8(r1, 4, 1);

  // (2Z+0) >> {2} loses the stride -> top
  auto* state2 = bennet_absint_state_create();
  state2 = bennet_absint_state_set_congr(state2, asym(a), tagged_congr_u8(2, 0));
  bennet_tagged_domain r2 =
      bennet_congr_transform_forward(cn_smt_shift_right(u8_sym(a), u8_const(2)), state2);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&r2));

  bennet_absint_state_free(state);
  bennet_absint_state_free(state2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardNegate) {
  // -(4Z+1) = 4Z+3 (-1 = 3 mod 4). No cn_smt_negate builder exists, so the
  // CN_UNOP_NEGATE node is hand-built.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));

  bennet_tagged_domain result =
      bennet_congr_transform_forward(negate_term(u8_sym(a)), state);
  expect_congr_u8(result, 4, 3);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardIteJoins) {
  // ite(_, {3}, {7}) = join({3}, {7}) = 4Z+3 (gcd(0, 0, |3-7|) = 4)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();

  cn_term* ite = cn_smt_ite(cn_smt_bool(true), u8_const(3), u8_const(7));
  bennet_tagged_domain result = bennet_congr_transform_forward(ite, state);
  expect_congr_u8(result, 4, 3);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardCastWiden) {
  // Congruence survives widening casts: u8 4Z+3 -> u16 4Z+3
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 3));

  cn_term* cast = cn_smt_cast(cn_base_type_bits(false, 16), u8_sym(a));
  bennet_tagged_domain result = bennet_congr_transform_forward(cast, state);
  ASSERT_NE(result.domain, nullptr);
  auto* dom = (bennet_domain_congr_uint16_t*)result.domain;
  EXPECT_FALSE(dom->bottom);
  EXPECT_EQ(dom->modulus, 4);
  EXPECT_EQ(dom->residue, 3);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardCastNarrowTruncates) {
  // u16 256Z+7 narrowed to u8: modulus 256 truncates to 0 -> singleton {7},
  // which is exactly gamma(256Z+7) mod 2^8.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_term* a16 = cn_smt_sym(a, cn_base_type_bits(false, 16));
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u16(256, 7));

  cn_term* cast = cn_smt_cast(cn_base_type_bits(false, 8), a16);
  bennet_tagged_domain result = bennet_congr_transform_forward(cast, state);
  expect_congr_u8(result, 0, 7);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardBwAndTop) {
  // Bitwise ops are not modeled by the congruence walker (default arm) -> top,
  // even though bennet_domain_congr_and_uint8_t exists as a direct domain op.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(4, 2));

  bennet_tagged_domain result =
      bennet_congr_transform_forward(cn_smt_bw_and(u8_sym(a), u8_sym(b)), state);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&result));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardArrayShift) {
  // p: 16Z+0, i: 4Z+1, elem_size 2
  // offset = i * 2 = 8Z+2; result = p + offset = 8Z+2
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");
  cn_term* p_term = cn_smt_sym(p, cn_base_type_simple(CN_BASE_LOC));
  cn_term* i_term = cn_smt_sym(i, cn_base_type_bits(false, 64));
  state = bennet_absint_state_set_congr(state, asym(p), tagged_congr_loc(16, 0));
  state = bennet_absint_state_set_congr(state, asym(i), tagged_congr_u64(4, 1));

  cn_term* shift = cn_smt_array_shift(p_term, 2, i_term);
  bennet_tagged_domain result = bennet_congr_transform_forward(shift, state);
  expect_congr_u64(result, 8, 2);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrForwardMemberShift) {
  // p: 8Z+0, offset 3 -> 8Z+3
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_term* p_term = cn_smt_sym(p, cn_base_type_simple(CN_BASE_LOC));
  state = bennet_absint_state_set_congr(state, asym(p), tagged_congr_loc(8, 0));

  cn_term* shift = cn_smt_member_shift(p_term, 3);
  bennet_tagged_domain result = bennet_congr_transform_forward(shift, state);
  expect_congr_u64(result, 8, 3);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// -----------------------------------------------------------------------------
// Backward transformer
// -----------------------------------------------------------------------------

TEST_F(LibBennet, CongrBackwardBottomOutputPropagates) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  auto* refined = bennet_congr_transform_backward(
      u8_sym(a), asym(a), bennet_tagged_domain_bottom_congr(&bt), state);
  EXPECT_TRUE(bennet_absint_state_is_bottom_congr(refined));
  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined, asym(a), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_bottom_congr(&a_dom));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardSymMeet) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  // a unbound (top): meet with 4Z+3 -> 4Z+3
  auto* state = bennet_absint_state_create();
  auto* refined =
      bennet_congr_transform_backward(u8_sym(a), asym(a), tagged_congr_u8(4, 3), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 3);

  // a: 2Z+0, output {5}: 5 is odd -> meet is bottom
  auto* state2 = bennet_absint_state_create();
  state2 = bennet_absint_state_set_congr(state2, asym(a), tagged_congr_u8(2, 0));
  auto* refined2 =
      bennet_congr_transform_backward(u8_sym(a), asym(a), tagged_congr_u8(0, 5), state2);
  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined2, asym(a), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_bottom_congr(&a_dom));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  bennet_absint_state_free(state2);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardOtherSymUntouched) {
  // Term mentions only b; refining target a must change nothing.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(4, 1));

  cn_term* term = cn_smt_add(u8_sym(b), u8_const(1));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(8, 0), state);

  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined, asym(a), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&a_dom));
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(b), &bt), 4, 1);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardAddInverts) {
  // out = a + {3} with out: 8Z+1  =>  a = out - {3} = 8Z+6
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* term = cn_smt_add(u8_sym(a), u8_const(3));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(8, 1), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 8, 6);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardSubLeftInverts) {
  // out = a - {3} with out: 4Z+1  =>  a = out + {3} = 4Z+0
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* term = cn_smt_sub(u8_sym(a), u8_const(3));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(4, 1), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 0);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardSubRightInverts) {
  // out = {10} - a with out: {3}  =>  a = {10} - out = {7}
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* term = cn_smt_sub(u8_const(10), u8_sym(a));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(0, 3), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 0, 7);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardMulSingletonInverts) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);

  // out = a * {4} with out: 8Z+4  =>  a = out / {4} = 2Z+1
  auto* state = bennet_absint_state_create();
  cn_term* term = cn_smt_mul(u8_sym(a), u8_const(4));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(8, 4), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 2, 1);

  // Multiplying by a non-singleton (b: 4Z+2) is not invertible -> a unchanged
  auto* state2 = bennet_absint_state_create();
  state2 = bennet_absint_state_set_congr(state2, asym(b), tagged_congr_u8(4, 2));
  cn_term* term2 = cn_smt_mul(u8_sym(a), u8_sym(b));
  auto* refined2 =
      bennet_congr_transform_backward(term2, asym(a), tagged_congr_u8(8, 4), state2);
  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined2, asym(a), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&a_dom));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  bennet_absint_state_free(state2);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardModGcd) {
  // out = a % {4} with out: {3}  =>  a in gcd(0,4)Z+3 = 4Z+3
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* term = cn_smt_mod(u8_sym(a), u8_const(4));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(0, 3), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 3);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardShlInverts) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  // out = a << {1} with out: 8Z+4  =>  a = out >> {1} = 4Z+2
  auto* state = bennet_absint_state_create();
  cn_term* term = cn_smt_shift_left(u8_sym(a), u8_const(1));
  auto* refined =
      bennet_congr_transform_backward(term, asym(a), tagged_congr_u8(8, 4), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 2);

  // SHIFT_RIGHT is never inverted -> a keeps its prior domain
  auto* state2 = bennet_absint_state_create();
  state2 = bennet_absint_state_set_congr(state2, asym(a), tagged_congr_u8(4, 1));
  cn_term* term2 = cn_smt_shift_right(u8_sym(a), u8_const(1));
  auto* refined2 =
      bennet_congr_transform_backward(term2, asym(a), tagged_congr_u8(0, 2), state2);
  expect_congr_u8(bennet_absint_state_get_congr(refined2, asym(a), &bt), 4, 1);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  bennet_absint_state_free(state2);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardComparisonNoRefine) {
  // Comparisons are deferred to backward_assume: state is unchanged.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* cmps[] = {
      cn_smt_eq(u8_sym(a), u8_const(5)),
      cn_smt_lt(u8_sym(a), u8_const(5)),
      cn_smt_le(u8_sym(a), u8_const(5)),
  };

  for (cn_term* cmp : cmps) {
    auto* state = bennet_absint_state_create();
    state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
    auto* refined =
        bennet_congr_transform_backward(cmp, asym(a), tagged_congr_u8(0, 1), state);
    expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 1);
    bennet_absint_state_free(state);
    bennet_absint_state_free(refined);
  }

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardCastPropagates) {
  // out = (u16)a with out: u16 8Z+2  =>  a: u8 8Z+2
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* cast = cn_smt_cast(cn_base_type_bits(false, 16), u8_sym(a));
  auto* refined =
      bennet_congr_transform_backward(cast, asym(a), tagged_congr_u16(8, 2), state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 8, 2);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardArrayShiftBaseAndIndex) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);
  cn_base_type bt_u64 = cn_base_type_bits(false, 64);
  cn_term* p_term = cn_smt_sym(p, bt_loc);
  cn_term* i_term = cn_smt_sym(i, bt_u64);
  cn_term* shift = cn_smt_array_shift(p_term, 4, i_term);

  // Base target: i: {2}, out: 16Z+8 => p = out - {8} = 16Z+0
  auto* state = bennet_absint_state_create();
  state = bennet_absint_state_set_congr(state, asym(i), tagged_congr_u64(0, 2));
  auto* refined =
      bennet_congr_transform_backward(shift, asym(p), tagged_congr_loc(16, 8), state);
  expect_congr_u64(bennet_absint_state_get_congr(refined, asym(p), &bt_loc), 16, 0);

  // Index target: p: {100}, out: {108} => i = ({108} - {100}) / 4 = {2}
  auto* state2 = bennet_absint_state_create();
  state2 = bennet_absint_state_set_congr(state2, asym(p), tagged_congr_loc(0, 100));
  auto* refined2 =
      bennet_congr_transform_backward(shift, asym(i), tagged_congr_loc(0, 108), state2);
  expect_congr_u64(bennet_absint_state_get_congr(refined2, asym(i), &bt_u64), 0, 2);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  bennet_absint_state_free(state2);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrBackwardMemberShift) {
  // out = member_shift(p, 5) with out: {105}  =>  p = {100}
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);
  cn_term* p_term = cn_smt_sym(p, bt_loc);

  cn_term* shift = cn_smt_member_shift(p_term, 5);
  auto* refined =
      bennet_congr_transform_backward(shift, asym(p), tagged_congr_loc(0, 105), state);
  expect_congr_u64(bennet_absint_state_get_congr(refined, asym(p), &bt_loc), 0, 100);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

// -----------------------------------------------------------------------------
// Backward assume
// -----------------------------------------------------------------------------

TEST_F(LibBennet, CongrAssumeEqTrueMeetsBothSyms) {
  // assume(a == b, true) with a: 4Z+1, b: 2Z+1: meet = 4Z+1, applied to both
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(2, 1));

  cn_term* eq = cn_smt_eq(u8_sym(a), u8_sym(b));
  auto* refined = bennet_congr_transform_backward_assume(eq, true, state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 1);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(b), &bt), 4, 1);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeEqTrueWithConst) {
  // assume(a == {9}, true) with a: 4Z+1 (9 = 1 mod 4) => a: {9}
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));

  cn_term* eq = cn_smt_eq(u8_sym(a), u8_const(9));
  auto* refined = bennet_congr_transform_backward_assume(eq, true, state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 0, 9);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeEqTrueUnsatBottomsAll) {
  // assume(a == b, true) with a: 4Z+1, b: 4Z+3: meet is bottom -> every sym
  // in the constraint goes to bottom. The stored bottoms are LOC-tagged
  // (congr.c), so only inspect them via the tagged is_bottom predicate.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(4, 3));

  cn_term* eq = cn_smt_eq(u8_sym(a), u8_sym(b));
  auto* refined = bennet_congr_transform_backward_assume(eq, true, state);
  EXPECT_TRUE(bennet_absint_state_is_bottom_congr(refined));
  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined, asym(a), &bt);
  bennet_tagged_domain b_dom = bennet_absint_state_get_congr(refined, asym(b), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_bottom_congr(&a_dom));
  EXPECT_TRUE(bennet_tagged_domain_is_bottom_congr(&b_dom));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeEqFalseNoop) {
  // a != b gives no congruence refinement
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);
  state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
  state = bennet_absint_state_set_congr(state, asym(b), tagged_congr_u8(4, 3));

  cn_term* eq = cn_smt_eq(u8_sym(a), u8_sym(b));
  auto* refined = bennet_congr_transform_backward_assume(eq, false, state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 1);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(b), &bt), 4, 3);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeAndTrueRefines) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* conj =
      cn_smt_and(cn_smt_eq(u8_sym(a), u8_const(5)), cn_smt_eq(u8_sym(b), u8_const(6)));

  // assume(a == 5 && b == 6, true) refines both syms
  auto* state = bennet_absint_state_create();
  auto* refined = bennet_congr_transform_backward_assume(conj, true, state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 0, 5);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(b), &bt), 0, 6);

  // assume(..., false) has no single implied fact -> no refinement
  auto* state2 = bennet_absint_state_create();
  auto* refined2 = bennet_congr_transform_backward_assume(conj, false, state2);
  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined2, asym(a), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&a_dom));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  bennet_absint_state_free(state2);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeOrFalseWithNotRefines) {
  // assume(!(a == 5) || !(b == 6), false): both disjuncts must be false,
  // NOT flips them to EQ-true -> a: {5}, b: {6}
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");
  cn_sym b = cn_sym_from_string("b");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* disj = cn_smt_or(cn_smt_not(cn_smt_eq(u8_sym(a), u8_const(5))),
      cn_smt_not(cn_smt_eq(u8_sym(b), u8_const(6))));
  auto* refined = bennet_congr_transform_backward_assume(disj, false, state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 0, 5);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(b), &bt), 0, 6);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeNotFlips) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);
  cn_term* neg = cn_smt_not(cn_smt_eq(u8_sym(a), u8_const(5)));

  // assume(!(a == 5), false) == assume(a == 5, true) -> a: {5}
  auto* state = bennet_absint_state_create();
  auto* refined = bennet_congr_transform_backward_assume(neg, false, state);
  expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 0, 5);

  // assume(!(a == 5), true) == assume(a != 5) -> no congruence refinement
  auto* state2 = bennet_absint_state_create();
  auto* refined2 = bennet_congr_transform_backward_assume(neg, true, state2);
  bennet_tagged_domain a_dom = bennet_absint_state_get_congr(refined2, asym(a), &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_top_congr(&a_dom));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  bennet_absint_state_free(state2);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, CongrAssumeLtLeNoop) {
  // Congruences cannot represent intervals: LT/LE refine nothing, either way.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym a = cn_sym_from_string("a");
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_term* cmps[] = {
      cn_smt_lt(u8_sym(a), u8_const(10)),
      cn_smt_le(u8_sym(a), u8_const(10)),
  };

  for (cn_term* cmp : cmps) {
    for (bool polarity : {true, false}) {
      auto* state = bennet_absint_state_create();
      state = bennet_absint_state_set_congr(state, asym(a), tagged_congr_u8(4, 1));
      auto* refined = bennet_congr_transform_backward_assume(cmp, polarity, state);
      expect_congr_u8(bennet_absint_state_get_congr(refined, asym(a), &bt), 4, 1);
      bennet_absint_state_free(state);
      bennet_absint_state_free(refined);
    }
  }

  cn_bump_free_after(frame);
}
