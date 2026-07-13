#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/prelude.h>

// C++ helper functions to replace C macros that use _Generic and GCC statement expressions
inline int64_t arbitrary_wint_i64(int64_t start, int64_t end) {
  struct bennet_domain_wint_int64_t tmp = {false, false, start, end};
  return bennet_arbitrary_wint_int64_t(&tmp);
}

inline uint64_t arbitrary_wint_u64(uint64_t start, uint64_t end) {
  struct bennet_domain_wint_uint64_t tmp = {false, false, start, end};
  return bennet_arbitrary_wint_uint64_t(&tmp);
}

TEST_F(LibBennet, ArbitraryWIntI64) {
  bennet_set_size(15);
  for (int i = 0; i < 100; i++) {
    int64_t val = arbitrary_wint_i64(-12, 53);
    EXPECT_LT(val, 17);
    EXPECT_GE(val, -12);
  }
}

TEST_F(LibBennet, OverflowArbitraryWIntU64_JustLower) {
  bennet_set_max_size(100);
  bennet_set_size(50);
  for (int i = 0; i < 1000; i++) {
    uint64_t val = arbitrary_wint_u64(UINT64_MAX - 100, 53);
    EXPECT_GE(val, 0);
    EXPECT_LT(val, 50);
  }
}

TEST_F(LibBennet, OverflowArbitraryWIntU64_AlsoHigh) {
  bennet_set_max_size(100);
  bennet_set_size(75);
  uint64_t excess = bennet_get_size() - 53;

  uint64_t val = arbitrary_wint_u64(UINT64_MAX - 100, 53);
  uint64_t min = val;
  uint64_t max = val;

  for (int i = 0; i < 1000; i++) {
    uint64_t val = arbitrary_wint_u64(UINT64_MAX - 100, 53);

    EXPECT_FALSE(53 < val && val < UINT64_MAX - 100);
    EXPECT_LT(val, UINT64_MAX - 100 + excess);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, 0);
  EXPECT_EQ(max, UINT64_MAX - 100 + excess - 1);
}

TEST_F(LibBennet, OverflowArbitraryWIntI64_RightOfZero) {
  bennet_set_max_size(100);
  bennet_set_size(50);

  int64_t val = arbitrary_wint_i64(INT64_MAX - 100, 53);
  int64_t min = val;
  int64_t max = val;

  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_wint_i64(INT64_MAX - 100, 53);
    EXPECT_LT(val, 50);
    EXPECT_GT(val, -50);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, -49);
  EXPECT_EQ(max, 49);
}

TEST_F(LibBennet, OverflowArbitraryWIntI64_LeftOfZero) {
  bennet_set_max_size(100);
  bennet_set_size(50);

  int64_t val = arbitrary_wint_i64(-53, INT64_MIN + 100);
  int64_t min = val;
  int64_t max = val;

  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_wint_i64(-53, INT64_MIN + 100);
    EXPECT_LT(val, 50);
    EXPECT_GT(val, -50);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, -49);
  EXPECT_EQ(max, 49);
}

TEST_F(LibBennet, OverflowArbitraryWIntI64_DisjointGTZero) {
  bennet_set_max_size(100);
  bennet_set_size(100);

  int64_t val = arbitrary_wint_i64(60, 20);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_wint_i64(60, 20);

    EXPECT_LT(val, 120);
    EXPECT_GT(val, -119);
    EXPECT_FALSE(20 < val && val < 60);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, -118);
  EXPECT_EQ(max, 119);
}

TEST_F(LibBennet, OverflowArbitraryWIntI64_DisjointLTZero) {
  bennet_set_max_size(100);
  bennet_set_size(100);

  int64_t val = arbitrary_wint_i64(-20, -60);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_wint_i64(-20, -60);

    EXPECT_LT(val, 120);
    EXPECT_GT(val, -119);
    EXPECT_FALSE(-60 < val && val < -20);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, -118);
  EXPECT_EQ(max, 119);
}

TEST_F(LibBennet, OverflowArbitraryWIntI64_DisjointLeanRight) {
  bennet_set_max_size(100);
  bennet_set_size(100);

  int64_t val = arbitrary_wint_i64(65, -75);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_wint_i64(65, -75);

    EXPECT_LT(val, 170);
    EXPECT_GT(val, -169);
    EXPECT_FALSE(-75 < val && val < 65);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, -168);
  EXPECT_EQ(max, 169);
}

TEST_F(LibBennet, OverflowArbitraryWIntI64_DisjointLeanLeft) {
  bennet_set_max_size(100);
  bennet_set_size(100);

  int64_t val = arbitrary_wint_i64(70, -70);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = arbitrary_wint_i64(70, -70);

    EXPECT_LT(val, 170);
    EXPECT_GT(val, -169);
    EXPECT_FALSE(-70 < val && val < 70);

    if (val < min) {
      min = val;
    }

    if (max < val) {
      max = val;
    }
  }

  EXPECT_EQ(min, -168);
  EXPECT_EQ(max, 169);
}

// =============================================================================
// Wrapped Interval Domain Tests
// Ported from tests/ounit/bennet/abstractDomains/wrappedInterval.ml
// =============================================================================

// Helper functions for creating wrapped interval domains
inline bennet_domain_wint_uint8_t* make_wint_u8(uint8_t start, uint8_t stop) {
  return bennet_domain_wint_of_uint8_t(start, stop);
}

inline bennet_domain_wint_int8_t* make_wint_s8(int8_t start, int8_t stop) {
  return bennet_domain_wint_of_int8_t(start, stop);
}

inline bennet_domain_wint_uint16_t* make_wint_u16(uint16_t start, uint16_t stop) {
  return bennet_domain_wint_of_uint16_t(start, stop);
}

// =============================================================================
// Basic Creation Tests
// =============================================================================

TEST_F(LibBennet, WIntBasicCreation) {
  // Test bottom
  auto bottom = bennet_domain_wint_bottom_uint8_t();
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint8_t(bottom));
  EXPECT_FALSE(bennet_domain_wint_is_top_uint8_t(bottom));

  // Test top
  auto top = bennet_domain_wint_top_uint8_t();
  EXPECT_TRUE(bennet_domain_wint_is_top_uint8_t(top));
  EXPECT_FALSE(bennet_domain_wint_is_bottom_uint8_t(top));

  // Test single value interval
  auto single = make_wint_u8(42, 42);
  EXPECT_FALSE(bennet_domain_wint_is_bottom_uint8_t(single));
  EXPECT_FALSE(bennet_domain_wint_is_top_uint8_t(single));
}

// =============================================================================
// Wrapped Membership Tests
// =============================================================================

TEST_F(LibBennet, WIntWrappedMembership) {
  // Normal interval [10, 20]
  auto normal = make_wint_u8(10, 20);
  EXPECT_TRUE(bennet_domain_wint_check_uint8_t(15, normal));  // 15 should be in [10,20]
  EXPECT_TRUE(bennet_domain_wint_check_uint8_t(10, normal));  // 10 should be in [10,20]
  EXPECT_TRUE(bennet_domain_wint_check_uint8_t(20, normal));  // 20 should be in [10,20]
  EXPECT_FALSE(
      bennet_domain_wint_check_uint8_t(5, normal));  // 5 should not be in [10,20]
  EXPECT_FALSE(
      bennet_domain_wint_check_uint8_t(25, normal));  // 25 should not be in [10,20]

  // Wrapped interval [250, 10] wraps around
  auto wrapped = make_wint_u8(250, 10);
  EXPECT_TRUE(
      bennet_domain_wint_check_uint8_t(5, wrapped));  // 5 should be in wrapped [250,10]
  EXPECT_TRUE(bennet_domain_wint_check_uint8_t(
      255, wrapped));  // 255 should be in wrapped [250,10]
  EXPECT_TRUE(bennet_domain_wint_check_uint8_t(
      250, wrapped));  // 250 should be in wrapped [250,10]
  EXPECT_TRUE(
      bennet_domain_wint_check_uint8_t(10, wrapped));  // 10 should be in wrapped [250,10]
  EXPECT_FALSE(bennet_domain_wint_check_uint8_t(
      100, wrapped));  // 100 should not be in wrapped [250,10]
  EXPECT_FALSE(bennet_domain_wint_check_uint8_t(
      200, wrapped));  // 200 should not be in wrapped [250,10]
}

// =============================================================================
// Lattice Ordering (leq) Tests
// =============================================================================

TEST_F(LibBennet, WIntLeqOrdering) {
  auto bottom = bennet_domain_wint_bottom_uint8_t();
  auto top = bennet_domain_wint_top_uint8_t();

  // Bottom is less than everything
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(bottom, bottom));
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(bottom, top));

  // Everything is less than top
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(top, top));

  // Top is not less than bottom
  EXPECT_FALSE(bennet_domain_wint_leq_uint8_t(top, bottom));

  // Normal containment
  auto small = make_wint_u8(10, 15);
  auto large = make_wint_u8(5, 20);
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(small, large));   // [10,15] <= [5,20]
  EXPECT_FALSE(bennet_domain_wint_leq_uint8_t(large, small));  // not [5,20] <= [10,15]

  // Wrapped intervals
  auto wrapped_small = make_wint_u8(250, 10);
  auto wrapped_large = make_wint_u8(240, 20);
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(wrapped_small, wrapped_large));
}

// =============================================================================
// Join Operation Tests
// =============================================================================

TEST_F(LibBennet, WIntJoinOperation) {
  auto bottom = bennet_domain_wint_bottom_uint8_t();
  auto top = bennet_domain_wint_top_uint8_t();
  auto wint = make_wint_u8(10, 20);

  // Join with bottom
  auto join_bottom = bennet_domain_wint_join_uint8_t(bottom, wint);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(join_bottom, wint));

  auto join_bottom_comm = bennet_domain_wint_join_uint8_t(wint, bottom);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(join_bottom_comm, wint));

  // Join with top
  auto join_top = bennet_domain_wint_join_uint8_t(top, wint);
  EXPECT_TRUE(bennet_domain_wint_is_top_uint8_t(join_top));

  auto join_top_comm = bennet_domain_wint_join_uint8_t(wint, top);
  EXPECT_TRUE(bennet_domain_wint_is_top_uint8_t(join_top_comm));

  // Join containment case
  auto small = make_wint_u8(10, 15);
  auto large = make_wint_u8(5, 20);
  auto join_contain = bennet_domain_wint_join_uint8_t(small, large);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(join_contain, large));

  auto join_contain_comm = bennet_domain_wint_join_uint8_t(large, small);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(join_contain_comm, large));
}

// =============================================================================
// Meet Operation Tests
// =============================================================================

TEST_F(LibBennet, WIntMeetOperation) {
  auto bottom = bennet_domain_wint_bottom_uint8_t();
  auto top = bennet_domain_wint_top_uint8_t();
  auto wint = make_wint_u8(10, 20);

  // Meet with bottom
  auto meet_bottom = bennet_domain_wint_meet_uint8_t(bottom, wint);
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint8_t(meet_bottom));

  auto meet_bottom_comm = bennet_domain_wint_meet_uint8_t(wint, bottom);
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint8_t(meet_bottom_comm));

  // Meet with top
  auto meet_top = bennet_domain_wint_meet_uint8_t(top, wint);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(meet_top, wint));

  auto meet_top_comm = bennet_domain_wint_meet_uint8_t(wint, top);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(meet_top_comm, wint));

  // Meet containment case
  auto small = make_wint_u8(10, 15);
  auto large = make_wint_u8(5, 20);
  auto meet_contain = bennet_domain_wint_meet_uint8_t(small, large);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(meet_contain, small));

  auto meet_contain_comm = bennet_domain_wint_meet_uint8_t(large, small);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(meet_contain_comm, small));
}

// =============================================================================
// Equality Tests
// =============================================================================

TEST_F(LibBennet, WIntEquality) {
  // Bottom equals bottom
  auto bottom1 = bennet_domain_wint_bottom_uint8_t();
  auto bottom2 = bennet_domain_wint_bottom_uint8_t();
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(bottom1, bottom2));

  // Top equals top
  auto top1 = bennet_domain_wint_top_uint8_t();
  auto top2 = bennet_domain_wint_top_uint8_t();
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(top1, top2));

  // Top does not equal bottom
  EXPECT_FALSE(bennet_domain_wint_equal_uint8_t(top1, bottom1));
  EXPECT_FALSE(bennet_domain_wint_equal_uint8_t(bottom1, top1));

  // Same intervals are equal
  auto int1 = make_wint_u8(10, 20);
  auto int2 = make_wint_u8(10, 20);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(int1, int2));

  // Different intervals are not equal
  auto int3 = make_wint_u8(10, 25);
  EXPECT_FALSE(bennet_domain_wint_equal_uint8_t(int1, int3));
}

// =============================================================================
// Signed Integer Tests
// =============================================================================

TEST_F(LibBennet, WIntSignedBasic) {
  // Test signed bottom
  auto bottom = bennet_domain_wint_bottom_int8_t();
  EXPECT_TRUE(bennet_domain_wint_is_bottom_int8_t(bottom));

  // Test signed top
  auto top = bennet_domain_wint_top_int8_t();
  EXPECT_TRUE(bennet_domain_wint_is_top_int8_t(top));

  // Test signed interval [-10, 10]
  auto interval = make_wint_s8(-10, 10);
  EXPECT_FALSE(bennet_domain_wint_is_bottom_int8_t(interval));
  EXPECT_FALSE(bennet_domain_wint_is_top_int8_t(interval));

  // Test membership for signed interval
  EXPECT_TRUE(bennet_domain_wint_check_int8_t(0, interval));
  EXPECT_TRUE(bennet_domain_wint_check_int8_t(-10, interval));
  EXPECT_TRUE(bennet_domain_wint_check_int8_t(10, interval));
  EXPECT_FALSE(bennet_domain_wint_check_int8_t(-20, interval));
  EXPECT_FALSE(bennet_domain_wint_check_int8_t(20, interval));
}

// =============================================================================
// 16-bit Tests
// =============================================================================

TEST_F(LibBennet, WInt16BitBasic) {
  // Test 16-bit bottom and top
  auto bottom = bennet_domain_wint_bottom_uint16_t();
  auto top = bennet_domain_wint_top_uint16_t();
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint16_t(bottom));
  EXPECT_TRUE(bennet_domain_wint_is_top_uint16_t(top));

  // Test 16-bit interval
  auto interval = make_wint_u16(100, 200);
  EXPECT_TRUE(bennet_domain_wint_check_uint16_t(150, interval));
  EXPECT_FALSE(bennet_domain_wint_check_uint16_t(50, interval));

  // Test ordering
  EXPECT_TRUE(bennet_domain_wint_leq_uint16_t(bottom, top));
  EXPECT_TRUE(bennet_domain_wint_leq_uint16_t(interval, top));
  EXPECT_TRUE(bennet_domain_wint_leq_uint16_t(bottom, interval));
}

// =============================================================================
// Copy Tests
// =============================================================================

TEST_F(LibBennet, WIntCopy) {
  auto original = make_wint_u8(10, 20);
  auto copy = bennet_domain_wint_copy_uint8_t(original);

  // Copy should equal original
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(original, copy));

  // But they should be different pointers
  EXPECT_NE(original, copy);

  // Test copying top and bottom
  auto top = bennet_domain_wint_top_uint8_t();
  auto top_copy = bennet_domain_wint_copy_uint8_t(top);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(top, top_copy));

  auto bottom = bennet_domain_wint_bottom_uint8_t();
  auto bottom_copy = bennet_domain_wint_copy_uint8_t(bottom);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(bottom, bottom_copy));
}

// =============================================================================
// Reflexivity and Idempotence Tests
// =============================================================================

TEST_F(LibBennet, WIntLeqReflexive) {
  // leq should be reflexive
  auto int1 = make_wint_u8(10, 20);
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(int1, int1));

  auto wrapped = make_wint_u8(250, 10);
  EXPECT_TRUE(bennet_domain_wint_leq_uint8_t(wrapped, wrapped));
}

TEST_F(LibBennet, WIntJoinIdempotent) {
  // join should be idempotent
  auto interval = make_wint_u8(10, 20);
  auto join_result = bennet_domain_wint_join_uint8_t(interval, interval);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(join_result, interval));
}

TEST_F(LibBennet, WIntMeetIdempotent) {
  // meet should be idempotent
  auto interval = make_wint_u8(10, 20);
  auto meet_result = bennet_domain_wint_meet_uint8_t(interval, interval);
  EXPECT_TRUE(bennet_domain_wint_equal_uint8_t(meet_result, interval));
}

// =============================================================================
// Absorbing Element Tests
// =============================================================================

TEST_F(LibBennet, WIntBottomAbsorbingForMeet) {
  // Bottom should be absorbing for meet
  auto bottom = bennet_domain_wint_bottom_uint8_t();
  auto interval = make_wint_u8(10, 20);

  auto result = bennet_domain_wint_meet_uint8_t(interval, bottom);
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint8_t(result));
}

TEST_F(LibBennet, WIntTopAbsorbingForJoin) {
  // Top should be absorbing for join
  auto top = bennet_domain_wint_top_uint8_t();
  auto interval = make_wint_u8(10, 20);

  auto result = bennet_domain_wint_join_uint8_t(interval, top);
  EXPECT_TRUE(bennet_domain_wint_is_top_uint8_t(result));
}

// =============================================================================
// Bitwise and Shift Operation Tests
// Ported from tests/ounit/bennet/abstractDomains/wrappedInterval.ml
// These tests use the abstract transformer API
// =============================================================================

#include <bennet/internals/domains/wint.h>
#include <cn-executable/bump_alloc.h>
#include <cn-smt/terms.h>

// Helper to create a tagged domain from a uint8_t wint domain
inline bennet_tagged_domain make_tagged_wint_u8(uint8_t start, uint8_t end) {
  auto* dom = bennet_domain_wint_of_uint8_t(start, end);
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(false, 8);
  return bennet_tagged_domain_create(type, dom);
}

// Helper to create a tagged domain from a int8_t wint domain
inline bennet_tagged_domain make_tagged_wint_s8(int8_t start, int8_t end) {
  auto* dom = bennet_domain_wint_of_int8_t(start, end);
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(true, 8);
  return bennet_tagged_domain_create(type, dom);
}

// Helper to create a tagged domain from a uint16_t wint domain
inline bennet_tagged_domain make_tagged_wint_u16(uint16_t start, uint16_t end) {
  auto* dom = bennet_domain_wint_of_uint16_t(start, end);
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(false, 16);
  return bennet_tagged_domain_create(type, dom);
}

// Helper to get the uint8 interval bounds from a tagged domain
inline void get_wint_u8_bounds(bennet_tagged_domain* d, uint8_t* start, uint8_t* end) {
  auto* dom = (bennet_domain_wint_uint8_t*)d->domain;
  *start = dom->start;
  *end = dom->end;
}

// Helper to check if tagged domain is top
inline bool is_tagged_top_u8(bennet_tagged_domain* d) {
  return bennet_tagged_domain_is_top_wint(d);
}

// =============================================================================
// Bitwise AND Tests
// =============================================================================

TEST_F(LibBennet, WIntBitwiseAnd) {
  // Test [2,3] & [9,10] = [0,2]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(2, 3);
  auto dom_b = make_tagged_wint_u8(9, 10);

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* and_term = cn_smt_bw_and(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(and_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 0);
  EXPECT_EQ(end, 2);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Bitwise OR Tests
// =============================================================================

TEST_F(LibBennet, WIntBitwiseOr) {
  // Test [2,3] | [9,10] = [10,11]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(2, 3);
  auto dom_b = make_tagged_wint_u8(9, 10);

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* or_term = cn_smt_bw_or(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(or_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 10);
  EXPECT_EQ(end, 11);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Bitwise XOR Tests
// =============================================================================

TEST_F(LibBennet, WIntBitwiseXor) {
  // Test [2,3] ^ [9,10]
  // Exact values: 2^9=11, 2^10=8, 3^9=10, 3^10=9 -> exact range [8,11]
  // Uses De Morgan's law: a^b = (a & ~b) | (~a & b) for precise bounds
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(2, 3);
  auto dom_b = make_tagged_wint_u8(9, 10);

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* xor_term = cn_smt_bw_xor(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(xor_term, state);

  // Check result matches precise XOR bounds: [8, 11]
  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 8);
  EXPECT_EQ(end, 11);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Left Shift Tests
// =============================================================================

TEST_F(LibBennet, WIntLeftShiftBasic) {
  // Test [2,3] << 1 = [4,6]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(2, 3);
  auto dom_b = make_tagged_wint_u8(1, 1);  // Constant shift amount

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* shift_term = cn_smt_shift_left(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 4);
  EXPECT_EQ(end, 6);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntLeftShiftZero) {
  // Test [10,20] << 0 = [10,20]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(10, 20);
  auto dom_b = make_tagged_wint_u8(0, 0);  // Shift by 0

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* shift_term = cn_smt_shift_left(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 10);
  EXPECT_EQ(end, 20);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Right Shift Tests
// =============================================================================

TEST_F(LibBennet, WIntRightShiftBasic) {
  // Test [8,12] >> 1 = [4,6]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(8, 12);
  auto dom_b = make_tagged_wint_u8(1, 1);  // Constant shift amount

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* shift_term = cn_smt_shift_right(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 4);
  EXPECT_EQ(end, 6);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Excessive Shift Tests
// =============================================================================

TEST_F(LibBennet, WIntShiftExcessive) {
  // Test shift by 8 (width) produces top
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(10, 20);
  auto dom_b = make_tagged_wint_u8(8, 8);  // Shift by width

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* shift_term = cn_smt_shift_left(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  EXPECT_TRUE(is_tagged_top_u8(&result));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Non-Constant Shift Tests
// =============================================================================

TEST_F(LibBennet, WIntNonConstantShift) {
  // Test variable shift [1,3] produces top
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(10, 20);
  auto dom_b = make_tagged_wint_u8(1, 3);  // Non-constant shift

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* shift_term = cn_smt_shift_left(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  EXPECT_TRUE(is_tagged_top_u8(&result));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Arithmetic Operations via Abstract Transformer
// =============================================================================

TEST_F(LibBennet, WIntAddViaTransformer) {
  // Test [10,20] + [5,15] = [15,35]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(10, 20);
  auto dom_b = make_tagged_wint_u8(5, 15);

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* add_term = cn_smt_add(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(add_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 15);
  EXPECT_EQ(end, 35);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntSubViaTransformer) {
  // Test [20,30] - [5,10] = [10,25]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 8);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u8(20, 30);
  auto dom_b = make_tagged_wint_u8(5, 10);

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* sub_term = cn_smt_sub(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(sub_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 10);
  EXPECT_EQ(end, 25);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// 16-bit Shift Tests
// =============================================================================

// =============================================================================
// 64-bit Regression Tests
// These test Bug #1: (uint64_t)1 << 64 is UB, which caused wint_normalize_unsigned,
// wint_member, wint_cardinality, and wint_get_min/wint_get_max to return wrong
// results for width=64 types (uint64_t, uintptr_t, int64_t).
// =============================================================================

// Helper to create uint64_t wint domains
inline bennet_domain_wint_uint64_t* make_wint_u64(uint64_t start, uint64_t stop) {
  return bennet_domain_wint_of_uint64_t(start, stop);
}

inline bennet_domain_wint_int64_t* make_wint_s64(int64_t start, int64_t stop) {
  return bennet_domain_wint_of_int64_t(start, stop);
}

TEST_F(LibBennet, WInt64BitBasicCreation) {
  // Top and bottom for uint64_t
  auto top = bennet_domain_wint_top_uint64_t();
  EXPECT_TRUE(bennet_domain_wint_is_top_uint64_t(top));
  EXPECT_FALSE(bennet_domain_wint_is_bottom_uint64_t(top));

  auto bottom = bennet_domain_wint_bottom_uint64_t();
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint64_t(bottom));
  EXPECT_FALSE(bennet_domain_wint_is_top_uint64_t(bottom));

  // Top and bottom for int64_t
  auto top_s = bennet_domain_wint_top_int64_t();
  EXPECT_TRUE(bennet_domain_wint_is_top_int64_t(top_s));

  auto bottom_s = bennet_domain_wint_bottom_int64_t();
  EXPECT_TRUE(bennet_domain_wint_is_bottom_int64_t(bottom_s));
}

TEST_F(LibBennet, WInt64BitMembership) {
  // Regression: wint_member was broken for width=64 due to (1<<64) UB
  auto interval = make_wint_u64(10, 100);
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(10, interval));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(50, interval));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(100, interval));
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(5, interval));
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(101, interval));

  // Test with values near extremes
  auto near_max = make_wint_u64(UINT64_MAX - 10, UINT64_MAX);
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(UINT64_MAX, near_max));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(UINT64_MAX - 5, near_max));
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(0, near_max));
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(UINT64_MAX - 11, near_max));

  // Test wrapping interval [UINT64_MAX-5, 5] for uint64
  auto wrapped = make_wint_u64(UINT64_MAX - 5, 5);
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(0, wrapped));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(UINT64_MAX, wrapped));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(5, wrapped));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(UINT64_MAX - 5, wrapped));
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(100, wrapped));
}

TEST_F(LibBennet, WInt64BitSignedMembership) {
  auto interval = make_wint_s64(-100, 100);
  EXPECT_TRUE(bennet_domain_wint_check_int64_t(0, interval));
  EXPECT_TRUE(bennet_domain_wint_check_int64_t(-100, interval));
  EXPECT_TRUE(bennet_domain_wint_check_int64_t(100, interval));
  EXPECT_FALSE(bennet_domain_wint_check_int64_t(-101, interval));
  EXPECT_FALSE(bennet_domain_wint_check_int64_t(101, interval));

  // Extremes
  auto extremes = make_wint_s64(INT64_MIN, INT64_MAX);
  EXPECT_TRUE(bennet_domain_wint_check_int64_t(0, extremes));
  EXPECT_TRUE(bennet_domain_wint_check_int64_t(INT64_MIN, extremes));
  EXPECT_TRUE(bennet_domain_wint_check_int64_t(INT64_MAX, extremes));
}

TEST_F(LibBennet, WInt64BitMeet) {
  // Meet with top should return the other interval
  auto a = make_wint_u64(10, 50);
  auto top = bennet_domain_wint_top_uint64_t();
  auto meet_top = bennet_domain_wint_meet_uint64_t(a, top);
  EXPECT_TRUE(bennet_domain_wint_equal_uint64_t(meet_top, a));

  // Meet of containing intervals: [10, 50] meet [5, 70] = [10, 50]
  auto large = make_wint_u64(5, 70);
  auto meet_contain = bennet_domain_wint_meet_uint64_t(a, large);
  EXPECT_TRUE(bennet_domain_wint_equal_uint64_t(meet_contain, a));

  // Meet with bottom = bottom
  auto bottom = bennet_domain_wint_bottom_uint64_t();
  auto meet_bot = bennet_domain_wint_meet_uint64_t(a, bottom);
  EXPECT_TRUE(bennet_domain_wint_is_bottom_uint64_t(meet_bot));
}

TEST_F(LibBennet, WInt64BitJoin) {
  // Join of two 64-bit intervals
  auto a = make_wint_u64(10, 20);
  auto b = make_wint_u64(15, 30);
  auto join = bennet_domain_wint_join_uint64_t(a, b);
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(10, join));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(30, join));

  // Join with bottom
  auto bottom = bennet_domain_wint_bottom_uint64_t();
  auto join_bottom = bennet_domain_wint_join_uint64_t(a, bottom);
  EXPECT_TRUE(bennet_domain_wint_equal_uint64_t(join_bottom, a));
}

TEST_F(LibBennet, WInt64BitArbitrary) {
  // Regression: arbitrary was broken for width=64 due to normalization UB
  bennet_set_max_size(100);
  bennet_set_size(50);
  for (int i = 0; i < 100; i++) {
    uint64_t val = arbitrary_wint_u64(10, 100);
    EXPECT_GE(val, 10u);
    EXPECT_LE(val, 100u);
  }
}

TEST_F(LibBennet, WInt64BitArbitraryNearMax) {
  // Regression: ensure arbitrary works near UINT64_MAX
  bennet_set_max_size(100);
  bennet_set_size(50);
  for (int i = 0; i < 100; i++) {
    uint64_t val = arbitrary_wint_u64(UINT64_MAX - 50, UINT64_MAX);
    EXPECT_GE(val, UINT64_MAX - 50);
  }
}

// =============================================================================
// Backward Assume Regression Tests
// These test Bug #2 (empty interval check using signed comparison) and
// Bug #3 (is_top not cleared after EQ refinement).
// The original failure: backward_assume(EQ(n, NULL), false) on a 64-bit
// unsigned top interval incorrectly marked the refined interval as bottom
// or left is_top=true, causing the pointer generator to ignore bounds.
// =============================================================================

// Helper to create a tagged domain for 64-bit unsigned (like uintptr_t)
inline bennet_tagged_domain make_tagged_wint_u64(uint64_t start, uint64_t end) {
  auto* dom = bennet_domain_wint_of_uint64_t(start, end);
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(false, 64);
  return bennet_tagged_domain_create(type, dom);
}

inline bennet_tagged_domain make_tagged_wint_u64_top() {
  auto* dom = bennet_domain_wint_top_uint64_t();
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_bits(false, 64);
  return bennet_tagged_domain_create(type, dom);
}

TEST_F(LibBennet, WIntBackwardAssumeNeqZero64Bit) {
  // Regression test for Bug #2 and #3:
  // backward_assume(EQ(n, 0), false) on top interval [0, UINT64_MAX]
  // should produce [1, UINT64_MAX], NOT bottom.
  // Bug #2: signed comparison (1 > -1) made [1, UINT64_MAX] look empty
  // Bug #3: is_top stayed true after narrowing start from 0 to 1
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 64);

  cn_sym sym_n = cn_sym_from_string("n");
  auto dom_n = make_tagged_wint_u64_top();
  state = bennet_absint_state_set_wint(state, {sym_n.name, sym_n.id}, dom_n);

  // Create term: EQ(n, 0)
  cn_term* term_n = cn_smt_sym(sym_n, bt);
  cn_term* term_zero = cn_smt_bits(false, 64, 0);
  cn_term* eq_term = cn_smt_eq(term_n, term_zero);

  // backward_assume(EQ(n, 0), false) means "n != 0"
  auto* refined_state = bennet_wint_transform_backward_assume(eq_term, false, state);

  // Get the refined domain for n
  bennet_tagged_domain refined_n =
      bennet_absint_state_get_wint(refined_state, {sym_n.name, sym_n.id}, &bt);

  // It should NOT be bottom
  EXPECT_FALSE(bennet_tagged_domain_is_bottom_wint(&refined_n));
  // It should NOT be top (Bug #3: is_top was not cleared)
  EXPECT_FALSE(bennet_tagged_domain_is_top_wint(&refined_n));

  // 0 should NOT be a member (we required n != 0)
  auto* refined_dom = (bennet_domain_wint_uint64_t*)refined_n.domain;
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(0, refined_dom));

  // 1 should be a member
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(1, refined_dom));
  // UINT64_MAX should be a member
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(UINT64_MAX, refined_dom));

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntBackwardAssumeNeqOnNarrowInterval64Bit) {
  // backward_assume(EQ(n, 5), false) on [5, 10]
  // should produce [6, 10] (start boundary removed)
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 64);

  cn_sym sym_n = cn_sym_from_string("n");
  auto dom_n = make_tagged_wint_u64(5, 10);
  state = bennet_absint_state_set_wint(state, {sym_n.name, sym_n.id}, dom_n);

  cn_term* term_n = cn_smt_sym(sym_n, bt);
  cn_term* term_five = cn_smt_bits(false, 64, 5);
  cn_term* eq_term = cn_smt_eq(term_n, term_five);

  auto* refined_state = bennet_wint_transform_backward_assume(eq_term, false, state);

  bennet_tagged_domain refined_n =
      bennet_absint_state_get_wint(refined_state, {sym_n.name, sym_n.id}, &bt);

  EXPECT_FALSE(bennet_tagged_domain_is_bottom_wint(&refined_n));

  auto* refined_dom = (bennet_domain_wint_uint64_t*)refined_n.domain;
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(5, refined_dom));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(6, refined_dom));
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(10, refined_dom));

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

// =============================================================================
// 16-bit Shift Tests
// =============================================================================

TEST_F(LibBennet, WIntShift16Bit) {
  // Test [100,200] << 2 = [400,800]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt = cn_base_type_bits(false, 16);

  cn_sym sym_a = cn_sym_from_string("a");
  cn_sym sym_b = cn_sym_from_string("b");

  auto dom_a = make_tagged_wint_u16(100, 200);
  // Create shift amount domain
  auto* shift_dom = bennet_domain_wint_of_uint16_t(2, 2);
  cn_base_type* shift_type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *shift_type = cn_base_type_bits(false, 16);
  auto dom_b = bennet_tagged_domain_create(shift_type, shift_dom);

  state = bennet_absint_state_set_wint(state, {sym_a.name, sym_a.id}, dom_a);
  state = bennet_absint_state_set_wint(state, {sym_b.name, sym_b.id}, dom_b);

  cn_term* term_a = cn_smt_sym(sym_a, bt);
  cn_term* term_b = cn_smt_sym(sym_b, bt);
  cn_term* shift_term = cn_smt_shift_left(term_a, term_b);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  auto* res_dom = (bennet_domain_wint_uint16_t*)result.domain;
  EXPECT_EQ(res_dom->start, 400);
  EXPECT_EQ(res_dom->end, 800);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// CN_TERM_CAST forward transformer tests
// =============================================================================

TEST_F(LibBennet, WIntCastSameWidth_U8ToU8) {
  // Same-width cast: uint8 [10,20] → uint8 [10,20]
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_u8(10, 20);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_u8);
  cn_term* cast_term = cn_smt_cast(bt_u8, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 10);
  EXPECT_EQ(end, 20);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastZeroExtend_U8ToU16) {
  // Zero extension: uint8 [10,20] → uint16 [10,20]
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_base_type bt_u16 = cn_base_type_bits(false, 16);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_u8(10, 20);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_u8);
  cn_term* cast_term = cn_smt_cast(bt_u16, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  auto* res_dom = (bennet_domain_wint_uint16_t*)result.domain;
  EXPECT_EQ(res_dom->start, 10);
  EXPECT_EQ(res_dom->end, 20);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastZeroExtend_U8ToU64) {
  // Zero extension: uint8 [100,200] → uint64 [100,200]
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_base_type bt_u64 = cn_base_type_bits(false, 64);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_u8(100, 200);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_u8);
  cn_term* cast_term = cn_smt_cast(bt_u64, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  auto* res_dom = (bennet_domain_wint_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (uint64_t)100);
  EXPECT_EQ(res_dom->end, (uint64_t)200);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastSignExtend_S8ToS64_Positive) {
  // Sign extension: int8 [10,20] → int64 [10,20] (positive, sign bit not set)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_s8 = cn_base_type_bits(true, 8);
  cn_base_type bt_s64 = cn_base_type_bits(true, 64);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_s8(10, 20);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_s8);
  cn_term* cast_term = cn_smt_cast(bt_s64, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  auto* res_dom = (bennet_domain_wint_int64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (int64_t)10);
  EXPECT_EQ(res_dom->end, (int64_t)20);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastSignExtend_S8ToS64_Negative) {
  // Sign extension: int8 [-5,-1] → int64 [-5,-1] (negative, sign bit set → extend with 1s)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_s8 = cn_base_type_bits(true, 8);
  cn_base_type bt_s64 = cn_base_type_bits(true, 64);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_s8(-5, -1);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_s8);
  cn_term* cast_term = cn_smt_cast(bt_s64, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  auto* res_dom = (bennet_domain_wint_int64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (int64_t)-5);
  EXPECT_EQ(res_dom->end, (int64_t)-1);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastTruncate_U16ToU8_Narrow) {
  // Truncation: uint16 [10,20] → uint8 [10,20]  (fits, no wrapping)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_u16 = cn_base_type_bits(false, 16);
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_u16(10, 20);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_u16);
  cn_term* cast_term = cn_smt_cast(bt_u8, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  uint8_t start, end;
  get_wint_u8_bounds(&result, &start, &end);
  EXPECT_EQ(start, 10);
  EXPECT_EQ(end, 20);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastTruncate_U16ToU8_Wide) {
  // Truncation: uint16 [0,1000] → uint8: cardinality (1001) >= 256, so top
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_u16 = cn_base_type_bits(false, 16);
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  auto dom_x = make_tagged_wint_u16(0, 1000);
  state = bennet_absint_state_set_wint(state, {sym_x.name, sym_x.id}, dom_x);

  cn_term* term_x = cn_smt_sym(sym_x, bt_u16);
  cn_term* cast_term = cn_smt_cast(bt_u8, term_x);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  EXPECT_TRUE(is_tagged_top_u8(&result));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntCastPtrToU64_PreservesInterval) {
  // Pointer → u64 cast: LOC [0x1000,0x2000] → u64 [0x1000,0x2000]
  // This is the key case for the slab_free stuck constraint
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc;
  bt_loc.tag = CN_BASE_LOC;
  cn_base_type bt_u64 = cn_base_type_bits(false, 64);
  cn_sym sym_ptr = cn_sym_from_string("ptr");

  // Create a LOC domain [0x1000, 0x2000]
  auto* loc_dom = bennet_domain_wint_of_uint64_t(0x1000, 0x2000);
  cn_base_type* loc_type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *loc_type = bt_loc;
  auto dom_ptr = bennet_tagged_domain_create(loc_type, loc_dom);
  state = bennet_absint_state_set_wint(state, {sym_ptr.name, sym_ptr.id}, dom_ptr);

  cn_term* term_ptr = cn_smt_sym(sym_ptr, bt_loc);
  cn_term* cast_term = cn_smt_cast(bt_u64, term_ptr);
  bennet_tagged_domain result = bennet_wint_transform_forward(cast_term, state);

  auto* res_dom = (bennet_domain_wint_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (uint64_t)0x1000);
  EXPECT_EQ(res_dom->end, (uint64_t)0x2000);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// =============================================================================
// Array Shift and Member Shift Forward/Backward Transformer Tests
// =============================================================================

// Helper to create a tagged LOC domain
inline bennet_tagged_domain make_tagged_wint_loc(uint64_t start, uint64_t end) {
  auto* dom = bennet_domain_wint_of_uint64_t(start, end);
  cn_base_type* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = cn_base_type_simple(CN_BASE_LOC);
  return bennet_tagged_domain_create(type, dom);
}

TEST_F(LibBennet, WIntForwardArrayShift) {
  // base=[100,200], index=[0,3], elem_size=4
  // result = base + index*4 = [100, 200+3*4] = [100, 212]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);
  cn_base_type bt_u64 = cn_base_type_bits(false, 64);

  cn_sym sym_base = cn_sym_from_string("base");
  cn_sym sym_idx = cn_sym_from_string("idx");

  auto dom_base = make_tagged_wint_loc(100, 200);
  auto dom_idx = make_tagged_wint_u64(0, 3);

  state = bennet_absint_state_set_wint(state, {sym_base.name, sym_base.id}, dom_base);
  state = bennet_absint_state_set_wint(state, {sym_idx.name, sym_idx.id}, dom_idx);

  cn_term* term_base = cn_smt_sym(sym_base, bt_loc);
  cn_term* term_idx = cn_smt_sym(sym_idx, bt_u64);
  cn_term* shift_term = cn_smt_array_shift(term_base, 4, term_idx);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  auto* res_dom = (bennet_domain_wint_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (uint64_t)100);
  EXPECT_EQ(res_dom->end, (uint64_t)212);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntForwardMemberShift) {
  // base=[1000,2000], offset=8
  // result = base + 8 = [1008, 2008]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);

  cn_sym sym_base = cn_sym_from_string("base");
  auto dom_base = make_tagged_wint_loc(1000, 2000);
  state = bennet_absint_state_set_wint(state, {sym_base.name, sym_base.id}, dom_base);

  cn_term* term_base = cn_smt_sym(sym_base, bt_loc);
  cn_term* shift_term = cn_smt_member_shift(term_base, 8);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  auto* res_dom = (bennet_domain_wint_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (uint64_t)1008);
  EXPECT_EQ(res_dom->end, (uint64_t)2008);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntForwardArrayShiftZeroIndex) {
  // base=[100,200], index=[0,0], elem_size=4
  // result = base + 0*4 = [100, 200]
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);
  cn_base_type bt_u64 = cn_base_type_bits(false, 64);

  cn_sym sym_base = cn_sym_from_string("base");
  cn_sym sym_idx = cn_sym_from_string("idx");

  auto dom_base = make_tagged_wint_loc(100, 200);
  auto dom_idx = make_tagged_wint_u64(0, 0);

  state = bennet_absint_state_set_wint(state, {sym_base.name, sym_base.id}, dom_base);
  state = bennet_absint_state_set_wint(state, {sym_idx.name, sym_idx.id}, dom_idx);

  cn_term* term_base = cn_smt_sym(sym_base, bt_loc);
  cn_term* term_idx = cn_smt_sym(sym_idx, bt_u64);
  cn_term* shift_term = cn_smt_array_shift(term_base, 4, term_idx);

  bennet_tagged_domain result = bennet_wint_transform_forward(shift_term, state);

  auto* res_dom = (bennet_domain_wint_uint64_t*)result.domain;
  EXPECT_EQ(res_dom->start, (uint64_t)100);
  EXPECT_EQ(res_dom->end, (uint64_t)200);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntBackwardArrayShift) {
  // Backward: target sym is in base position
  // array_shift(base, 4, idx) with output domain [200, 300]
  // Should propagate to base
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);
  cn_base_type bt_u64 = cn_base_type_bits(false, 64);

  cn_sym sym_base = cn_sym_from_string("base");
  cn_sym sym_idx = cn_sym_from_string("idx");

  auto dom_base = make_tagged_wint_loc(0, 1000);
  auto dom_idx = make_tagged_wint_u64(0, 10);

  state = bennet_absint_state_set_wint(state, {sym_base.name, sym_base.id}, dom_base);
  state = bennet_absint_state_set_wint(state, {sym_idx.name, sym_idx.id}, dom_idx);

  cn_term* term_base = cn_smt_sym(sym_base, bt_loc);
  cn_term* term_idx = cn_smt_sym(sym_idx, bt_u64);
  cn_term* shift_term = cn_smt_array_shift(term_base, 4, term_idx);

  auto output_dom = make_tagged_wint_loc(200, 300);

  auto* refined_state = bennet_wint_transform_backward(
      shift_term, {sym_base.name, sym_base.id}, output_dom, state);

  // The backward transform should propagate to the base symbol
  // and produce a refined state (not bottom)
  EXPECT_FALSE(bennet_absint_state_is_bottom_wint(refined_state));

  bennet_tagged_domain refined_base =
      bennet_absint_state_get_wint(refined_state, {sym_base.name, sym_base.id}, &bt_loc);
  EXPECT_FALSE(bennet_tagged_domain_is_bottom_wint(&refined_base));

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntBackwardArrayShiftIndexFallbackStops) {
  // Backward: target sym is a NARROW (u8) index and the base has top bounds,
  // so the guarded inversion cannot run. The fallback used to push the
  // un-narrowed LOC-width output into the index subtree, and the meet with
  // the u8 index binding tripped wint_generic_meet's equal-width assert.
  // Sound behavior: no index refinement at all.
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);

  cn_sym sym_base = cn_sym_from_string("base");
  cn_sym sym_idx = cn_sym_from_string("idx");

  // Only the index is bound (u8 [0,10]); the base stays top.
  state = bennet_absint_state_set_wint(
      state, {sym_idx.name, sym_idx.id}, make_tagged_wint_u8(0, 10));

  cn_term* term_base = cn_smt_sym(sym_base, bt_loc);
  cn_term* term_idx = cn_smt_sym(sym_idx, bt_u8);
  cn_term* shift_term = cn_smt_array_shift(term_base, 4, term_idx);

  auto output_dom = make_tagged_wint_loc(200, 300);

  auto* refined_state = bennet_wint_transform_backward(
      shift_term, {sym_idx.name, sym_idx.id}, output_dom, state);

  EXPECT_FALSE(bennet_absint_state_is_bottom_wint(refined_state));

  // The index binding is untouched: still u8 [0,10].
  bennet_tagged_domain refined_idx =
      bennet_absint_state_get_wint(refined_state, {sym_idx.name, sym_idx.id}, &bt_u8);
  auto* idx_dom = (bennet_domain_wint_uint8_t*)refined_idx.domain;
  EXPECT_EQ(idx_dom->start, (uint8_t)0);
  EXPECT_EQ(idx_dom->end, (uint8_t)10);

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

// =============================================================================
// State functional contract: set/meet/backward return fresh states and leave
// the input state readable. The walkers' ITE cases pass the same state to
// both branch recursions and rely on this.
// =============================================================================

TEST_F(LibBennet, WIntStateSetShadowsWithoutMutating) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  cn_base_type bt = cn_base_type_bits(false, 64);
  cn_sym sym_x = cn_sym_from_string("x");
  bennet_absint_sym x = {sym_x.name, sym_x.id};

  auto* s0 = bennet_absint_state_create();
  auto* s1 = bennet_absint_state_set_wint(s0, x, make_tagged_wint_u64(0, 100));
  auto* s2 = bennet_absint_state_set_wint(s1, x, make_tagged_wint_u64(5, 7));

  bennet_tagged_domain from_s2 = bennet_absint_state_get_wint(s2, x, &bt);
  auto* d2 = (bennet_domain_wint_uint64_t*)from_s2.domain;
  EXPECT_EQ(d2->start, (uint64_t)5);
  EXPECT_EQ(d2->end, (uint64_t)7);

  // The older state still reads its own binding.
  bennet_tagged_domain from_s1 = bennet_absint_state_get_wint(s1, x, &bt);
  auto* d1 = (bennet_domain_wint_uint64_t*)from_s1.domain;
  EXPECT_EQ(d1->start, (uint64_t)0);
  EXPECT_EQ(d1->end, (uint64_t)100);

  // And the empty state still has no binding (top).
  bennet_tagged_domain from_s0 = bennet_absint_state_get_wint(s0, x, &bt);
  EXPECT_TRUE(bennet_tagged_domain_is_top_wint(&from_s0));

  bennet_absint_state_free(s2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntStatePersistsAcrossBackwardAssume) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  cn_base_type bt = cn_base_type_bits(false, 64);
  cn_sym sym_x = cn_sym_from_string("x");
  bennet_absint_sym x = {sym_x.name, sym_x.id};

  auto* state = bennet_absint_state_create();
  state = bennet_absint_state_set_wint(state, x, make_tagged_wint_u64(0, 100));

  // assume(EQ(x, 5), true) refines x in the returned state only
  cn_term* term_x = cn_smt_sym(sym_x, bt);
  cn_term* term_five = cn_smt_bits(false, 64, 5);
  cn_term* eq_term = cn_smt_eq(term_x, term_five);

  auto* refined = bennet_wint_transform_backward_assume(eq_term, true, state);

  bennet_tagged_domain refined_x = bennet_absint_state_get_wint(refined, x, &bt);
  auto* rd = (bennet_domain_wint_uint64_t*)refined_x.domain;
  EXPECT_TRUE(bennet_domain_wint_check_uint64_t(5, rd));
  EXPECT_FALSE(bennet_domain_wint_check_uint64_t(50, rd));

  // The input state still reads the pre-refinement interval.
  bennet_tagged_domain original_x = bennet_absint_state_get_wint(state, x, &bt);
  auto* od = (bennet_domain_wint_uint64_t*)original_x.domain;
  EXPECT_EQ(od->start, (uint64_t)0);
  EXPECT_EQ(od->end, (uint64_t)100);

  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntBackwardMemberShift) {
  // Backward: target sym is in base position
  // member_shift(base, 8) with output domain [1008, 2008]
  // Should propagate to base
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_loc = cn_base_type_simple(CN_BASE_LOC);

  cn_sym sym_base = cn_sym_from_string("base");
  auto dom_base = make_tagged_wint_loc(0, 5000);
  state = bennet_absint_state_set_wint(state, {sym_base.name, sym_base.id}, dom_base);

  cn_term* term_base = cn_smt_sym(sym_base, bt_loc);
  cn_term* shift_term = cn_smt_member_shift(term_base, 8);

  auto output_dom = make_tagged_wint_loc(1008, 2008);

  auto* refined_state = bennet_wint_transform_backward(
      shift_term, {sym_base.name, sym_base.id}, output_dom, state);

  // The backward transform should propagate to the base symbol
  // and produce a refined state (not bottom)
  EXPECT_FALSE(bennet_absint_state_is_bottom_wint(refined_state));

  bennet_tagged_domain refined_base =
      bennet_absint_state_get_wint(refined_state, {sym_base.name, sym_base.id}, &bt_loc);
  EXPECT_FALSE(bennet_tagged_domain_is_bottom_wint(&refined_base));

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

// There is no cn_smt_negate builder; hand-construct the CN_UNOP_NEGATE node.
inline cn_term* make_negate_term(cn_term* operand) {
  cn_term* t = cn_term_alloc(CN_TERM_UNOP, operand->base_type);
  t->data.unop.op = CN_UNOP_NEGATE;
  t->data.unop.operand = operand;
  return t;
}

TEST_F(LibBennet, WIntBackwardNegateInverts) {
  // out = -x in [10,20] (u8)  =>  x in the wrapped negation [236,246].
  // The legacy backward-unop default pushed [10,20] into x unchanged.
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");

  cn_term* neg_term = make_negate_term(cn_smt_sym(sym_x, bt_u8));

  auto* refined_state = bennet_wint_transform_backward(
      neg_term, {sym_x.name, sym_x.id}, make_tagged_wint_u8(10, 20), state);

  bennet_tagged_domain refined_x =
      bennet_absint_state_get_wint(refined_state, {sym_x.name, sym_x.id}, &bt_u8);
  uint8_t start, end;
  get_wint_u8_bounds(&refined_x, &start, &end);
  EXPECT_EQ(start, (uint8_t)236);
  EXPECT_EQ(end, (uint8_t)246);

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntBackwardComplInverts) {
  // out = ~x == 0xF0 (u8)  =>  x == 0x0F (COMPL is self-inverse).
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");

  cn_term* compl_term = cn_smt_bw_compl(cn_smt_sym(sym_x, bt_u8));

  auto* refined_state = bennet_wint_transform_backward(
      compl_term, {sym_x.name, sym_x.id}, make_tagged_wint_u8(0xF0, 0xF0), state);

  bennet_tagged_domain refined_x =
      bennet_absint_state_get_wint(refined_state, {sym_x.name, sym_x.id}, &bt_u8);
  uint8_t start, end;
  get_wint_u8_bounds(&refined_x, &start, &end);
  EXPECT_EQ(start, (uint8_t)0x0F);
  EXPECT_EQ(end, (uint8_t)0x0F);

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntAssumeOrTrueJoinsHull) {
  // x==3 || x==7 assumed true (formerly a gap: wint's AND/OR handling was
  // unreachable dead code): the branch refinements {3} and {7} join to the
  // hull [3,7].
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  cn_term* x = cn_smt_sym(sym_x, bt_u8);

  cn_term* cond = cn_smt_or(
      cn_smt_eq(x, cn_smt_bits(false, 8, 3)), cn_smt_eq(x, cn_smt_bits(false, 8, 7)));

  auto* refined =
      bennet_wint_transform_backward_assume(cond, true, bennet_absint_state_create());

  bennet_tagged_domain rx =
      bennet_absint_state_get_wint(refined, {sym_x.name, sym_x.id}, &bt_u8);
  uint8_t start, end;
  get_wint_u8_bounds(&rx, &start, &end);
  EXPECT_EQ(start, (uint8_t)3);
  EXPECT_EQ(end, (uint8_t)7);

  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntAssumeAndTrueThreads) {
  // 5 < x && x < 10 assumed true: the conjuncts thread left to right,
  // landing x in [6,9]. (First wint AND coverage; recursion was gated off
  // until the join-rule fix.)
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  cn_term* x = cn_smt_sym(sym_x, bt_u8);

  cn_term* cond = cn_smt_and(
      cn_smt_lt(cn_smt_bits(false, 8, 5), x), cn_smt_lt(x, cn_smt_bits(false, 8, 10)));

  auto* refined =
      bennet_wint_transform_backward_assume(cond, true, bennet_absint_state_create());

  bennet_tagged_domain rx =
      bennet_absint_state_get_wint(refined, {sym_x.name, sym_x.id}, &bt_u8);
  uint8_t start, end;
  get_wint_u8_bounds(&rx, &start, &end);
  EXPECT_EQ(start, (uint8_t)6);
  EXPECT_EQ(end, (uint8_t)9);

  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntAssumeFuelTwoTightensSecondSym) {
  // assume(x == y && x < 10, true): pass 1 threads the conjuncts - x==y
  // meets two tops (no information), then x<10 refines x to [0,9]. A second
  // local iteration re-runs the EQ with the refined x and meets it into y.
  // At the default fuel of 1, y stays top.
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  cn_sym sym_y = cn_sym_from_string("y");
  cn_term* x = cn_smt_sym(sym_x, bt_u8);
  cn_term* y = cn_smt_sym(sym_y, bt_u8);

  cn_term* cond = cn_smt_and(cn_smt_eq(x, y), cn_smt_lt(x, cn_smt_bits(false, 8, 10)));

  bennet_set_dynamic_local_iterations(1);
  auto* refined1 =
      bennet_wint_transform_backward_assume(cond, true, bennet_absint_state_create());
  bennet_tagged_domain y1 =
      bennet_absint_state_get_wint(refined1, {sym_y.name, sym_y.id}, &bt_u8);
  EXPECT_TRUE(is_tagged_top_u8(&y1));

  bennet_set_dynamic_local_iterations(2);
  auto* refined2 =
      bennet_wint_transform_backward_assume(cond, true, bennet_absint_state_create());
  bennet_tagged_domain y2 =
      bennet_absint_state_get_wint(refined2, {sym_y.name, sym_y.id}, &bt_u8);
  uint8_t start, end;
  get_wint_u8_bounds(&y2, &start, &end);
  EXPECT_EQ(start, (uint8_t)0);
  EXPECT_EQ(end, (uint8_t)9);

  bennet_set_dynamic_local_iterations(1);
  bennet_absint_state_free(refined1);
  bennet_absint_state_free(refined2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntAssumeAndFalseJoins) {
  // !(x < 100 && x < 50): at least one conjunct is false, so x is in the
  // join of [100,255] and [50,255] = [50,255].
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  cn_term* x = cn_smt_sym(sym_x, bt_u8);

  cn_term* cond = cn_smt_and(
      cn_smt_lt(x, cn_smt_bits(false, 8, 100)), cn_smt_lt(x, cn_smt_bits(false, 8, 50)));

  auto* refined =
      bennet_wint_transform_backward_assume(cond, false, bennet_absint_state_create());

  bennet_tagged_domain rx =
      bennet_absint_state_get_wint(refined, {sym_x.name, sym_x.id}, &bt_u8);
  uint8_t start, end;
  get_wint_u8_bounds(&rx, &start, &end);
  EXPECT_EQ(start, (uint8_t)50);
  EXPECT_EQ(end, (uint8_t)255);

  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, WIntBackwardAddTopSideStops) {
  // out = x + y in [5,5] with y unconstrained puts no constraint on x: the
  // legacy fallback pushed [5,5] into x (unsound; e.g. x=2,y=3 satisfies).
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  auto* state = bennet_absint_state_create();
  cn_base_type bt_u8 = cn_base_type_bits(false, 8);
  cn_sym sym_x = cn_sym_from_string("x");
  cn_sym sym_y = cn_sym_from_string("y");

  cn_term* add_term = cn_smt_add(cn_smt_sym(sym_x, bt_u8), cn_smt_sym(sym_y, bt_u8));

  auto* refined_state = bennet_wint_transform_backward(
      add_term, {sym_x.name, sym_x.id}, make_tagged_wint_u8(5, 5), state);

  bennet_tagged_domain refined_x =
      bennet_absint_state_get_wint(refined_state, {sym_x.name, sym_x.id}, &bt_u8);
  EXPECT_TRUE(is_tagged_top_u8(&refined_x));

  bennet_absint_state_free(refined_state);
  cn_bump_free_after(frame);
}
