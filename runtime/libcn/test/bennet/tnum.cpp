#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/prelude.h>

// C++ helper functions to replace C macros that use GCC statement expressions
inline uint64_t arbitrary_tnum_u64(uint64_t value, uint64_t mask) {
  struct bennet_domain_tnum_uint64_t tmp = {value, mask};
  return bennet_arbitrary_tnum_uint64_t(&tmp);
}

inline int64_t arbitrary_tnum_i64(int64_t value, int64_t mask) {
  struct bennet_domain_tnum_int64_t tmp = {value, mask};
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
