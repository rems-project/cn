#include <cn-autoannot/focus_ctx.h>
#include <gtest/gtest.h>

class LibAutoAnnot : public ::testing::Test {
 protected:
  void SetUp() override;
};

TEST(LibAutoAnnot, BasicOperations) {
  initialise_focus_context();
  push_focus_context();
  insert_focus(100);

  ASSERT_EQ(check_focus(100), 1) << "Focus 100 should be found in current context.";

  pop_focus_context();

  ASSERT_EQ(check_focus(100), 0) << "Focus 100 should NOT be found after pop.";
}
