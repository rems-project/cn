/**
 * @file products.cpp
 * @brief Tests for combined product domain arbitraries
 *
 * Tests the specialized combined arbitrary generators for:
 * - ownership x wint: ownership + wrapped interval
 * - congr x ownership: congruence + ownership
 * - congr x wint: congruence + wrapped interval
 * - congr x ownership x wint: congruence + ownership + wrapped interval
 * - ownership x tnum: ownership + tristate number
 *
 * Each combined arbitrary must produce values satisfying all domains
 * simultaneously.
 *
 * The ProductPin_* tests additionally pin exact RNG-derived output sequences
 * for a fixed seed, so that refactors of the samplers cannot silently change
 * the consumed RNG stream (bennet's RNG is SplitMix64 + Lemire — pure integer
 * ops, so the golden literals are platform-independent on 64-bit targets).
 * Alloc-path pins record offsets relative to bennet_rand_alloc_min_ptr()
 * because absolute buffer addresses differ per run.
 */

#include "harness.hpp"
#include <gtest/gtest.h>

#include <cstddef>
#include <cstdio>

#include <bennet/internals/domains/congr.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/products.h>
#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/prelude.h>
#include <bennet/state/rand_alloc.h>

#define PRODUCT_PIN_SEED 0xC0FFEEULL

// On mismatch, dump the actual sequence so new goldens can be pasted in.
#define PRODUCT_PIN_DUMP(got, n, tail)                                                   \
  do {                                                                                   \
    if (::testing::Test::HasNonfatalFailure()) {                                         \
      printf("PIN ACTUALS (%s):\n",                                                      \
          ::testing::UnitTest::GetInstance()->current_test_info()->name());              \
      for (size_t pin_i = 0; pin_i < (n); pin_i++) {                                     \
        printf("  0x%llxULL,\n", (unsigned long long)(got)[pin_i]);                      \
      }                                                                                  \
      printf("  tail: 0x%llxULL\n", (unsigned long long)(tail));                         \
    }                                                                                    \
  } while (0)

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

TEST_F(LibBennet, CongrOwnershipWintArbitrary_TopCongrBoundedWint_Aligned) {
  // Congr top + wint narrowed (the shape the triple reduce produces: wint
  // gets the effective allocation range): must allocate within the range,
  // not draw byte-granular values - generated pointers must keep allocator
  // alignment. Regression test for the misaligned-struct-pointer defect
  // exposed when this sampler became reachable.
  uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();
  for (int i = 0; i < 1000; i++) {
    auto congr = bennet_domain_congr_top_uintptr_t();
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
    auto wint = bennet_domain_wint_of_uintptr_t(min_ptr + 64, min_ptr + 65536);

    uintptr_t val =
        bennet_domain_congr_ownership_wint_arbitrary_uintptr_t(congr, own, wint);

    EXPECT_TRUE(bennet_domain_wint_check_uintptr_t(val, wint))
        << "val=" << val << " should be within the narrowed wint";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(8,8)";
    EXPECT_EQ((val - 8) % alignof(std::max_align_t), 0u)
        << "val=" << val << " allocation base must keep allocator alignment";

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

// =============================================================================
// ownership_wint tests
// =============================================================================

TEST_F(LibBennet, OwnershipWintArbitrary_TopOwnership) {
  // Ownership top (before=0, after=0): delegate to wint arbitrary
  // Wint: [10, 50]
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_top_uint32_t();
    auto wint = bennet_domain_wint_of_uint32_t(10, 50);

    uint32_t val = bennet_domain_ownership_wint_arbitrary_uint32_t(own, wint);

    EXPECT_TRUE(bennet_domain_wint_check_uint32_t(val, wint))
        << "val=" << val << " should be in [10, 50]";
  }
}

TEST_F(LibBennet, OwnershipWintArbitrary_AllocTopWint) {
  // Wint top: any value works, just needs valid allocation
  // Ownership: before=8, after=8 (16 bytes total)
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
    auto wint = bennet_domain_wint_top_uintptr_t();

    uintptr_t val = bennet_domain_ownership_wint_arbitrary_uintptr_t(own, wint);

    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(8,8)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, OwnershipWintArbitrary_AllocBoundedWint) {
  // Wint constrains the pointer range within the allocation buffer.
  // Ownership: before=4, after=4
  uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();
  for (int i = 0; i < 1000; i++) {
    auto own = bennet_domain_ownership_of_uintptr_t(4, 4);
    auto wint = bennet_domain_wint_of_uintptr_t(min_ptr + 64, min_ptr + 4096);

    uintptr_t val = bennet_domain_ownership_wint_arbitrary_uintptr_t(own, wint);

    EXPECT_TRUE(bennet_domain_wint_check_uintptr_t(val, wint))
        << "val=" << val << " should be in [min+64, min+4096]";
    EXPECT_TRUE(bennet_domain_ownership_check_uintptr_t(val, own))
        << "val=" << val << " should satisfy ownership(4,4)";

    bennet_rand_alloc_free_all();
  }
}

TEST_F(LibBennet, OwnershipWintArbitrary_UintptrNullBias) {
  // Ownership top + wint allows 0 -> should sometimes generate NULL
  // Wint: [0, 1000]
  int null_count = 0;
  for (int i = 0; i < 10000; i++) {
    auto own = bennet_domain_ownership_top_uintptr_t();
    auto wint = bennet_domain_wint_of_uintptr_t(0, 1000);

    uintptr_t val = bennet_domain_ownership_wint_arbitrary_uintptr_t(own, wint);

    EXPECT_TRUE(bennet_domain_wint_check_uintptr_t(val, wint));

    if (val == 0)
      null_count++;
  }
  EXPECT_GT(null_count, 0) << "Should generate NULL sometimes for pointer types";
}

// =============================================================================
// RNG-sequence pins (fixed seed -> exact output sequence)
// =============================================================================

TEST_F(LibBennet, ProductPin_OwnershipWint_U64_Values) {
  // Ownership top, wint [10, 50] at uint64_t: pure-RNG value path.
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  auto own = bennet_domain_ownership_top_uint64_t();
  auto wint = bennet_domain_wint_of_uint64_t(10, 50);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = bennet_domain_ownership_wint_arbitrary_uint64_t(own, wint);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0x18ULL, 0x1bULL, 0x15ULL, 0xaULL, 0xeULL, 0x11ULL, 0x1bULL, 0x1dULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x1a4c7945ef3e2887ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_OwnershipWint_Uintptr_BiasPath) {
  // Ownership top, wint [0, 1000] at uintptr_t: NULL-bias draw interleaves
  // with the wint draw.
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  auto own = bennet_domain_ownership_top_uintptr_t();
  auto wint = bennet_domain_wint_of_uintptr_t(0, 1000);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = (uint64_t)bennet_domain_ownership_wint_arbitrary_uintptr_t(own, wint);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0x11ULL, 0x0ULL, 0x7ULL, 0x13ULL, 0x6ULL, 0x10ULL, 0xdULL, 0x13ULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x2dd35b22825e9e21ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_OwnershipWint_Uintptr_AllocOffsets) {
  // Ownership(8,8), wint top at uintptr_t: bennet_alloc path. Pin buffer
  // offsets, not absolute addresses.
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();
  auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
  auto wint = bennet_domain_wint_top_uintptr_t();

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] =
        (uint64_t)(bennet_domain_ownership_wint_arbitrary_uintptr_t(own, wint) - min_ptr);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {0x31a758ULL,
      0x16a9a38ULL,
      0xdb9a28ULL,
      0xdcf2d8ULL,
      0xa3af28ULL,
      0x13433f8ULL,
      0x6f84b0ULL,
      0x1460fa0ULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x1a4c7945ef3e2887ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_CongrOwnership_Uintptr_AllocOffsets) {
  // Congr 8Z+0, ownership(8,8) at uintptr_t: both-constrained path draws the
  // index via bennet_range_uint64_t.
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();
  auto congr = bennet_domain_congr_of_uintptr_t(8, 0);
  auto own = bennet_domain_ownership_of_uintptr_t(8, 8);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = (uint64_t)(bennet_domain_congr_ownership_arbitrary_uintptr_t(congr, own) -
                        min_ptr);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0x58ULL, 0xa0ULL, 0x8ULL, 0x58ULL, 0x28ULL, 0x60ULL, 0x20ULL, 0x70ULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x1a4c7945ef3e2887ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_CongrOwnership_U32_NarrowAlloc) {
  // Congr 4Z+1, ownership(4,4) at uint32_t: narrow-type path allocates
  // (uint64 draw) then draws the congr value (uint32 draw) — pins the
  // interleaving.
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  auto congr = bennet_domain_congr_of_uint32_t(4, 1);
  auto own = bennet_domain_ownership_of_uint32_t(4, 4);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = (uint64_t)bennet_domain_congr_ownership_arbitrary_uint32_t(congr, own);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0x41ULL, 0x19ULL, 0x11ULL, 0x9ULL, 0x41ULL, 0x1ULL, 0x1ULL, 0xdULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x2dd35b22825e9e21ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_OwnershipTnum_Uintptr_RetryLoop) {
  // Ownership(8,8), tnum requiring 8-byte alignment at uintptr_t: pins the
  // draw count of the allocate-check-retry loop via offsets.
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();
  auto own = bennet_domain_ownership_of_uintptr_t(8, 8);
  auto tnum = bennet_domain_tnum_of_uintptr_t(0, ~(uintptr_t)0x7);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] =
        (uint64_t)(bennet_domain_ownership_tnum_arbitrary_uintptr_t(own, tnum) - min_ptr);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {0x31a758ULL,
      0x16a9a38ULL,
      0xdb9a28ULL,
      0xdcf2d8ULL,
      0xa3af28ULL,
      0x13433f8ULL,
      0x6f84b0ULL,
      0x1460fa0ULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x1a4c7945ef3e2887ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_OwnershipTnum_Uintptr_BiasPath) {
  // Ownership top, tnum all-bits-unknown at uintptr_t: NULL-bias draw then
  // tnum arbitrary. An all-bits-unknown tnum is an implicit top, so the tnum
  // arbitrary falls back to the default sized sampler (extrema skew intact,
  // sentinels like UINT_MAX stay reachable).
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  auto own = bennet_domain_ownership_top_uintptr_t();
  auto tnum = bennet_domain_tnum_of_uintptr_t(0, ~(uintptr_t)0);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = (uint64_t)bennet_domain_ownership_tnum_arbitrary_uintptr_t(own, tnum);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0xbULL, 0x7ULL, 0x3ULL, 0x10ULL, 0x1ULL, 0x10ULL, 0x10ULL, 0x4ULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0xb6e2c223523178d0ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_CongrWint_Uintptr_BiasPath) {
  // Congr 4Z+0, wint [0, 1000] at uintptr_t: both admit 0, so the NULL-bias
  // draw interleaves with the joint index draw. Goldens captured after unifying
  // the index draw on bennet_arbitrary_wint_of (the sampler is
  // unreachable from generated code until the sampler wiring lands).
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  auto congr = bennet_domain_congr_of_uintptr_t(4, 0);
  auto wint = bennet_domain_wint_of_uintptr_t(0, 1000);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = (uint64_t)bennet_domain_congr_wint_arbitrary_uintptr_t(congr, wint);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0x44ULL, 0x0ULL, 0x1cULL, 0x4cULL, 0x18ULL, 0x40ULL, 0x34ULL, 0x4cULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x2dd35b22825e9e21ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}

TEST_F(LibBennet, ProductPin_CongrWint_Uintptr_ConstrainedIndex) {
  // Congr 8Z+0, wint [16, 4096] at uintptr_t: 0 is outside wint, so no bias
  // draw - pure joint index draw (post-unification goldens).
  bennet_srand(PRODUCT_PIN_SEED);
  bennet_set_size(20);
  bennet_rand_alloc_free_all();

  auto congr = bennet_domain_congr_of_uintptr_t(8, 0);
  auto wint = bennet_domain_wint_of_uintptr_t(16, 4096);

  uint64_t got[8];
  for (int i = 0; i < 8; i++) {
    got[i] = (uint64_t)bennet_domain_congr_wint_arbitrary_uintptr_t(congr, wint);
  }
  uint64_t tail = bennet_rand();

  const uint64_t want[8] = {
      0x80ULL, 0x98ULL, 0x68ULL, 0x10ULL, 0x30ULL, 0x48ULL, 0x98ULL, 0xa8ULL};
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], want[i]) << "index " << i;
  }
  EXPECT_EQ(tail, 0x1a4c7945ef3e2887ULL) << "stream-position tail probe";
  PRODUCT_PIN_DUMP(got, 8, tail);
}
