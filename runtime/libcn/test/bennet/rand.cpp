#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/prelude.h>

TEST_F(LibBennet, UniformU8) {
  for (int i = 0; i < 100; i++) {
    uint8_t val = bennet_uniform_uint8_t(100);
    EXPECT_LT(val, 100);
  }
}

TEST_F(LibBennet, UniformU16) {
  for (int i = 0; i < 100; i++) {
    uint16_t val = bennet_uniform_uint16_t(100);
    EXPECT_LT(val, 100);
  }
}

TEST_F(LibBennet, UniformU32) {
  for (int i = 0; i < 100; i++) {
    uint32_t val = bennet_uniform_uint32_t(100);
    EXPECT_LT(val, 100);
  }
}

TEST_F(LibBennet, UniformU64) {
  for (int i = 0; i < 100; i++) {
    uint64_t val = bennet_uniform_uint64_t(100);
    EXPECT_LT(val, 100);
  }
}

TEST_F(LibBennet, UniformI8) {
  for (int i = 0; i < 100; i++) {
    int8_t val = bennet_uniform_int8_t(100);
    EXPECT_LT(val, 100);
    EXPECT_GT(val, -100);
  }
}

TEST_F(LibBennet, UniformI16) {
  for (int i = 0; i < 100; i++) {
    int16_t val = bennet_uniform_int16_t(100);
    EXPECT_LT(val, 100);
    EXPECT_GT(val, -100);
  }
}

TEST_F(LibBennet, UniformI32) {
  for (int i = 0; i < 100; i++) {
    int32_t val = bennet_uniform_int32_t(100);
    EXPECT_LT(val, 100);
    EXPECT_GT(val, -100);
  }
}

TEST_F(LibBennet, UniformI64) {
  for (int i = 0; i < 100; i++) {
    int64_t val = bennet_uniform_int64_t(100);
    EXPECT_LT(val, 100);
    EXPECT_GT(val, -100);
  }
}

TEST_F(LibBennet, RangeI64) {
  for (int i = 0; i < 100; i++) {
    int64_t val = bennet_range_int64_t(-12, 53);
    EXPECT_LE(val, 53);
    EXPECT_GE(val, -12);
  }
}

TEST_F(LibBennet, OverflowRangeU64) {
  for (int i = 0; i < 1000; i++) {
    uint64_t val = bennet_range_uint64_t(UINT64_MAX - 100, 53);
    EXPECT_FALSE(53 < val && val < UINT64_MAX - 100);
  }
}

TEST_F(LibBennet, OverflowRangeI64) {
  for (int i = 0; i < 1000; i++) {
    int64_t val = bennet_range_int64_t(INT64_MAX - 100, 53);
    EXPECT_FALSE(53 < val && val < INT64_MAX - 100);
  }
}
