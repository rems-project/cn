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
