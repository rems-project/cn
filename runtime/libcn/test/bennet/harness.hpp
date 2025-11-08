#include <gtest/gtest.h>

class LibBennet : public ::testing::Test {
 protected:
  // Setup code that's called before each test
  void SetUp() override;

  // Cleanup code that's called after each test
  void TearDown() override;
};
