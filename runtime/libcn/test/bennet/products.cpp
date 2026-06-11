/**
 * @file products.cpp
 * @brief Tests for combined product domain arbitraries
 *
 * Tests the specialized combined arbitrary generators for:
 * - congr x ownership: congruence + ownership
 * - congr x wint: congruence + wrapped interval
 * - congr x ownership x wint: congruence + ownership + wrapped interval
 * - ownership x tnum: ownership + tristate number
 *
 * Each combined arbitrary must produce values satisfying all domains
 * simultaneously.
 */

#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/internals/domains/congr.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/products.h>
#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/prelude.h>

// =============================================================================
// congr_ownership tests
// =============================================================================

TEST_F(LibBennet, CongrOwnershipArbitrary_TopOwnership) {
  // Ownership top (before=0, after=0): delegate to congr arbitrary
  // Congr: 4Z+1 (values 1, 5, 9, 13, ...)
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uintptr_t(4, 1);
    auto own = bennet_domain_ownership_top_uintptr_t();

    uintptr_t val = bennet_domain_congr_ownership_arbitrary_uintptr_t(congr, own);

    EXPECT_TRUE(bennet_domain_congr_check_uintptr_t(val, congr))
        << "val=" << val << " should satisfy congr 4Z+1";
  }
}

TEST_F(LibBennet, CongrOwnershipArbitrary_TopCongr) {
  // Congr top: any value works, just needs valid allocation
  // Ownership: before=4, after=4 (8 bytes total)
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_top_uintptr_t();
    auto own = bennet_domain_ownership_of_uintptr_t(4, 4);

    uintptr_t val = bennet_domain_congr_ownership_arbitrary_uintptr_t(congr, own);

    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(4,4)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, CongrOwnershipArbitrary_BothConstrained) {
  // Congr: 8Z+0 (values divisible by 8)
  // Ownership: before=8, after=8 (16 bytes total)
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uintptr_t(8, 0);
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);

    uintptr_t val = bennet_domain_congr_ownership_arbitrary_uintptr_t(congr, own);

    EXPECT_TRUE(bennet_domain_congr_check_uintptr_t(val, congr))
        << "val=" << val << " should satisfy congr 8Z+0";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(8,8)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, CongrOwnershipArbitrary_BothConstrained_Residue) {
  // Congr: 16Z+3 (values 3, 19, 35, ...)
  // Ownership: before=4, after=4
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uintptr_t(16, 3);
    auto own = bennet_domain_ownership_of_uintptr_t(4, 4);

    uintptr_t val = bennet_domain_congr_ownership_arbitrary_uintptr_t(congr, own);

    EXPECT_TRUE(bennet_domain_congr_check_uintptr_t(val, congr))
        << "val=" << val << " should satisfy congr 16Z+3";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(4,4)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, CongrOwnershipReduce) {
  // Ownership: before=4, after=4
  // Congr: top initially
  auto congr = bennet_domain_congr_top_uintptr_t();
  auto own = bennet_domain_ownership_of_uintptr_t(4, 4);

  bennet_domain_congr_ownership_reduce_uintptr_t(congr, own);

  // After reduce, congr should not be bottom (allocation is feasible)
  EXPECT_FALSE(congr->bottom);
}

// =============================================================================
// ownership_tnum tests
// =============================================================================

TEST_F(LibBennet, OwnershipTnumArbitrary_TopOwnership) {
  // Ownership top: delegate to tnum arbitrary
  // Tnum: value=0xF0, mask=0x0F (high nibble fixed, low nibble free)
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_top_uintptr_t();
    auto tnum = bennet_domain_tnum_of_uintptr_t(0xF0, 0x0F);

    uintptr_t val = bennet_domain_ownership_tnum_arbitrary_uintptr_t(own, tnum);

    EXPECT_TRUE(bennet_domain_tnum_check_uintptr_t(val, tnum))
        << "val=0x" << std::hex << val << " should satisfy tnum(0xF0, 0x0F)";
  }
}

TEST_F(LibBennet, OwnershipTnumArbitrary_TopTnum) {
  // Tnum top: any value works, just needs valid allocation
  // Ownership: before=4, after=4
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_of_uintptr_t(4, 4);
    auto tnum = bennet_domain_tnum_top_uintptr_t();

    uintptr_t val = bennet_domain_ownership_tnum_arbitrary_uintptr_t(own, tnum);

    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(4,4)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, OwnershipTnumArbitrary_BothConstrained) {
  // Tnum: value=0, mask=~0 (all bits unknown = top-like but not flagged top)
  // This is effectively unconstrained on bit patterns but goes through the
  // meet path. Ownership: before=8, after=8
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
    auto tnum = bennet_domain_tnum_of_uintptr_t(0, UINTPTR_MAX);

    uintptr_t val = bennet_domain_ownership_tnum_arbitrary_uintptr_t(own, tnum);

    EXPECT_TRUE(bennet_domain_tnum_check_uintptr_t(val, tnum))
        << "val=0x" << std::hex << val << " should satisfy tnum";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << std::dec << val << " should satisfy ownership(8,8)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, OwnershipTnumArbitrary_BothConstrained_FixedBits) {
  // Tnum with low 3 bits fixed to 0 (8-byte aligned), high bits free.
  // Ownership: before=8, after=8 ensures ptr = base+8 is 8-byte aligned
  // (since base is max_align_t-aligned).
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
    auto tnum = bennet_domain_tnum_of_uintptr_t(0, ~(uintptr_t)0x7);

    uintptr_t val = bennet_domain_ownership_tnum_arbitrary_uintptr_t(own, tnum);

    EXPECT_TRUE(bennet_domain_tnum_check_uintptr_t(val, tnum))
        << "val=0x" << std::hex << val << " should be 8-byte aligned";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << std::dec << val << " should satisfy ownership(8,8)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, OwnershipTnumReduce) {
  // Ownership: before=4, after=4
  // Tnum: top initially
  auto own = bennet_domain_ownership_of_uintptr_t(4, 4);
  auto tnum = bennet_domain_tnum_top_uintptr_t();

  bennet_domain_ownership_tnum_reduce_uintptr_t(own, tnum);

  // After reduce, tnum should not be bottom (allocation is feasible)
  EXPECT_FALSE(tnum->bottom);
}

// =============================================================================
// congr_wint tests
// =============================================================================

TEST_F(LibBennet, CongrWintArbitrary_TopCongr) {
  // Congr top: delegate to wint arbitrary
  // Wint: [10, 50]
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_top_uint32_t();
    auto wint = bennet_domain_wint_of_uint32_t(10, 50);

    uint32_t val = bennet_domain_congr_wint_arbitrary_uint32_t(congr, wint);

    EXPECT_TRUE(bennet_domain_wint_check_uint32_t(val, wint))
        << "val=" << val << " should be in [10, 50]";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_TopWint) {
  // Wint top: delegate to congr arbitrary
  // Congr: 4Z+1 (values 1, 5, 9, 13, ...)
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uint32_t(4, 1);
    auto wint = bennet_domain_wint_top_uint32_t();

    uint32_t val = bennet_domain_congr_wint_arbitrary_uint32_t(congr, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uint32_t(val, congr))
        << "val=" << val << " should satisfy congr 4Z+1";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_BothConstrained_Unsigned) {
  // Congr: 8Z+0 (multiples of 8)
  // Wint: [0, 100]
  // Valid: 0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uint32_t(8, 0);
    auto wint = bennet_domain_wint_of_uint32_t(0, 100);

    uint32_t val = bennet_domain_congr_wint_arbitrary_uint32_t(congr, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uint32_t(val, congr))
        << "val=" << val << " should satisfy congr 8Z+0";
    EXPECT_TRUE(bennet_domain_wint_check_uint32_t(val, wint))
        << "val=" << val << " should be in [0, 100]";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_BothConstrained_Residue) {
  // Congr: 16Z+3 (values 3, 19, 35, ...)
  // Wint: [10, 60]
  // Valid: 19, 35, 51
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uint32_t(16, 3);
    auto wint = bennet_domain_wint_of_uint32_t(10, 60);

    uint32_t val = bennet_domain_congr_wint_arbitrary_uint32_t(congr, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uint32_t(val, congr))
        << "val=" << val << " should satisfy congr 16Z+3";
    EXPECT_TRUE(bennet_domain_wint_check_uint32_t(val, wint))
        << "val=" << val << " should be in [10, 60]";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_Singleton) {
  // Congr: singleton {42} (modulus=0, residue=42)
  // Wint: [40, 50]
  for (int i = 0; i < 100; i++) {
    auto congr = bennet_domain_congr_of_uint32_t(0, 42);
    auto wint = bennet_domain_wint_of_uint32_t(40, 50);

    uint32_t val = bennet_domain_congr_wint_arbitrary_uint32_t(congr, wint);

    EXPECT_EQ(val, 42u) << "singleton congr {42} intersected with [40,50] must be 42";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_Signed_CrossesZero) {
  // Congr: 4Z+1 (values ..., -7, -3, 1, 5, 9, ...)
  // Wint: [-10, 10]
  // Valid (signed): -7, -3, 1, 5, 9
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_int32_t(4, 1);
    auto wint = bennet_domain_wint_of_int32_t(-10, 10);

    int32_t val = bennet_domain_congr_wint_arbitrary_int32_t(congr, wint);

    EXPECT_TRUE(bennet_domain_congr_check_int32_t(val, congr))
        << "val=" << val << " should satisfy congr 4Z+1";
    EXPECT_TRUE(bennet_domain_wint_check_int32_t(val, wint))
        << "val=" << val << " should be in [-10, 10]";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_Signed_AllNegative) {
  // Congr: 4Z+1 (values ..., -7, -3, ...)
  // Wint: [-20, -1]
  // Valid: -19, -15, -11, -7, -3
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_int32_t(4, 1);
    auto wint = bennet_domain_wint_of_int32_t(-20, -1);

    int32_t val = bennet_domain_congr_wint_arbitrary_int32_t(congr, wint);

    EXPECT_TRUE(bennet_domain_congr_check_int32_t(val, congr))
        << "val=" << val << " should satisfy congr 4Z+1";
    EXPECT_TRUE(bennet_domain_wint_check_int32_t(val, wint))
        << "val=" << val << " should be in [-20, -1]";
  }
}

TEST_F(LibBennet, CongrWintArbitrary_UintptrNullBias) {
  // Congr and wint both allow 0 -> should sometimes generate NULL
  // Congr: 4Z+0 (multiples of 4, includes 0)
  // Wint: [0, 1000]
  int null_count = 0;
  for (int i = 0; i < 10000; i++) {
    auto congr = bennet_domain_congr_of_uintptr_t(4, 0);
    auto wint = bennet_domain_wint_of_uintptr_t(0, 1000);

    uintptr_t val = bennet_domain_congr_wint_arbitrary_uintptr_t(congr, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uintptr_t(val, congr));
    EXPECT_TRUE(bennet_domain_wint_check_uintptr_t(val, wint));

    if (val == 0)
      null_count++;
  }
  // Should generate at least some NULLs
  EXPECT_GT(null_count, 0) << "Should generate NULL sometimes for pointer types";
}

TEST_F(LibBennet, CongrWintReduce_NarrowsCongr) {
  // Wint: [0, 15]
  // Congr: top initially
  auto congr = bennet_domain_congr_top_uint32_t();
  auto wint = bennet_domain_wint_of_uint32_t(0, 15);

  bennet_domain_congr_wint_reduce_uint32_t(congr, wint);

  // After reduce, congr should not be bottom
  EXPECT_FALSE(congr->bottom);
}

TEST_F(LibBennet, CongrWintReduce_EmptyIntersection) {
  // Congr: singleton {42}
  // Wint: [0, 10] (doesn't contain 42)
  auto congr = bennet_domain_congr_of_uint32_t(0, 42);
  auto wint = bennet_domain_wint_of_uint32_t(0, 10);

  bennet_domain_congr_wint_reduce_uint32_t(congr, wint);

  // Should detect empty intersection
  EXPECT_TRUE(congr->bottom);
  EXPECT_TRUE(wint->bottom);
}

TEST_F(LibBennet, CongrWintReduce_NarrowsWint) {
  // Congr: singleton {5}
  // Wint: [0, 100]
  auto congr = bennet_domain_congr_of_uint32_t(0, 5);
  auto wint = bennet_domain_wint_of_uint32_t(0, 100);

  bennet_domain_congr_wint_reduce_uint32_t(congr, wint);

  // After reduce with singleton congr, wint should narrow
  EXPECT_FALSE(wint->bottom);
  EXPECT_FALSE(congr->bottom);
}

// =============================================================================
// congr_ownership_wint tests
// =============================================================================

TEST_F(LibBennet, CongrOwnershipWintArbitrary_TopOwnership) {
  // Ownership top: delegate to congr_wint
  // Congr: 4Z+1, Wint: [0, 100]
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uint32_t(4, 1);
    auto own = bennet_domain_ownership_top_uint32_t();
    auto wint = bennet_domain_wint_of_uint32_t(0, 100);

    uint32_t val =
        bennet_domain_congr_ownership_wint_arbitrary_uint32_t(congr, own, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uint32_t(val, congr))
        << "val=" << val << " should satisfy congr 4Z+1";
    EXPECT_TRUE(bennet_domain_wint_check_uint32_t(val, wint))
        << "val=" << val << " should be in [0, 100]";
  }
}

TEST_F(LibBennet, CongrOwnershipWintArbitrary_AllTop) {
  // All three top: should just allocate
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_top_uintptr_t();
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
    auto wint = bennet_domain_wint_top_uintptr_t();

    uintptr_t val =
        bennet_domain_congr_ownership_wint_arbitrary_uintptr_t(congr, own, wint);

    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(8,8)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, CongrOwnershipWintArbitrary_AllConstrained) {
  // Congr: 8Z+0 (multiples of 8)
  // Ownership: before=8, after=8 (16 bytes total)
  // Wint: constrains pointer range
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_of_uintptr_t(8, 0);
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
    auto wint = bennet_domain_wint_top_uintptr_t();

    uintptr_t val =
        bennet_domain_congr_ownership_wint_arbitrary_uintptr_t(congr, own, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uintptr_t(val, congr))
        << "val=" << val << " should satisfy congr 8Z+0";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(8,8)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, CongrOwnershipWintArbitrary_OwnershipTopNullBias) {
  // Ownership top + congr/wint allow 0 -> should sometimes generate NULL
  int null_count = 0;
  for (int i = 0; i < 10000; i++) {
    auto congr = bennet_domain_congr_of_uintptr_t(4, 0);
    auto own = bennet_domain_ownership_top_uintptr_t();
    auto wint = bennet_domain_wint_of_uintptr_t(0, 1000);

    uintptr_t val =
        bennet_domain_congr_ownership_wint_arbitrary_uintptr_t(congr, own, wint);

    EXPECT_TRUE(bennet_domain_congr_check_uintptr_t(val, congr));
    EXPECT_TRUE(bennet_domain_wint_check_uintptr_t(val, wint));

    if (val == 0)
      null_count++;
  }
  EXPECT_GT(null_count, 0) << "Should generate NULL sometimes for pointer types";
}

TEST_F(LibBennet, CongrOwnershipWintReduce) {
  // Ownership: before=4, after=4
  // Congr: top, Wint: top
  auto congr = bennet_domain_congr_top_uintptr_t();
  auto own = bennet_domain_ownership_of_uintptr_t(4, 4);
  auto wint = bennet_domain_wint_top_uintptr_t();

  bennet_domain_congr_ownership_wint_reduce_uintptr_t(congr, own, wint);

  // After reduce, none should be bottom (allocation is feasible)
  EXPECT_FALSE(congr->bottom);
  EXPECT_FALSE(wint->bottom);
}

TEST_F(LibBennet, CongrOwnershipWintReduce_OwnershipTop) {
  // Ownership top: delegate to congr_wint reduce
  // Congr: singleton {42}, Wint: [0, 100]
  auto congr = bennet_domain_congr_of_uint32_t(0, 42);
  auto own = bennet_domain_ownership_top_uint32_t();
  auto wint = bennet_domain_wint_of_uint32_t(0, 100);

  bennet_domain_congr_ownership_wint_reduce_uint32_t(congr, own, wint);

  EXPECT_FALSE(congr->bottom);
  EXPECT_FALSE(wint->bottom);
}
