#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/prelude.h>

TEST_F(LibBennet, ArbitraryWIntI64) {
  bennet_set_size(15);
  for (int i = 0; i < 100; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, -12, 53);
    EXPECT_LT(val, 17);
    EXPECT_GE(val, -12);
  }
}

TEST_F(LibBennet, OverflowArbitraryWIntU64_JustLower) {
  bennet_set_max_size(100);
  bennet_set_size(50);
  for (int i = 0; i < 1000; i++) {
    uint64_t val = bennet_arbitrary_wint_of(uint64_t, UINT64_MAX - 100, 53);
    EXPECT_GE(val, 0);
    EXPECT_LT(val, 50);
  }
}

TEST_F(LibBennet, OverflowArbitraryWIntU64_AlsoHigh) {
  bennet_set_max_size(100);
  bennet_set_size(75);
  uint64_t excess = bennet_get_size() - 53;

  uint64_t val = bennet_arbitrary_wint_of(uint64_t, UINT64_MAX - 100, 53);
  uint64_t min = val;
  uint64_t max = val;

  for (int i = 0; i < 1000; i++) {
    uint64_t val = bennet_arbitrary_wint_of(uint64_t, UINT64_MAX - 100, 53);

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

  int64_t val = bennet_arbitrary_wint_of(int64_t, INT64_MAX - 100, 53);
  int64_t min = val;
  int64_t max = val;

  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, INT64_MAX - 100, 53);
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

  int64_t val = bennet_arbitrary_wint_of(int64_t, -53, INT64_MIN + 100);
  int64_t min = val;
  int64_t max = val;

  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, -53, INT64_MIN + 100);
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

  int64_t val = bennet_arbitrary_wint_of(int64_t, 60, 20);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, 60, 20);

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

  int64_t val = bennet_arbitrary_wint_of(int64_t, -20, -60);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, -20, -60);

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

  int64_t val = bennet_arbitrary_wint_of(int64_t, 65, -75);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, 65, -75);

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

  int64_t val = bennet_arbitrary_wint_of(int64_t, 70, -70);
  int64_t min = val;
  int64_t max = val;
  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_arbitrary_wint_of(int64_t, 70, -70);

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
