#include <cn-autoannot/auto_annot.h>
#include <gtest/gtest.h>

class LibAutoAnnot : public ::testing::Test {
 protected:
  void SetUp() override;
};

TEST(LibAutoAnnot, BasicOperations) {
  initialise_focus_context();
  push_focus_context();

  /*
  let p = 0xcafe000;
  take X = each(u64 i; i < 4) { RW<u64>(p) };
  */
  insert_iter_res(0xcafe0000, 0, 4, 8, "unsigned long");
  /* focus RW<u64>, 1u64; */
  insert_focus(1, "unsigned long");

  ASSERT_EQ(needs_focus(0xcafe0000, 8), 1) << "Lack of focus for p[0]";
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 0) << "Focused";
  ASSERT_EQ(needs_focus(0xcafe1000, 8), 0)
      << "No appropriate resource, so we don't need focus";

  clear_focus();
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 1) << "Cleared";

  insert_focus(1, "unsigned long");
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 0) << "Focused";

  push_focus_context();
  insert_iter_res(0xcafe0000, 0, 4, 8, "unsigned long");
  insert_iter_res(0x10000000, 1, 4, 8, "unsigned long");
  insert_iter_res(0x20000000, 0, 4, 4, "unsigned int");
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 1) << "No focus in the current level";
  ASSERT_EQ(needs_focus(0xcafe0000, 8), 1) << "No focus in the current level";

  insert_focus(1, "unsigned long");
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 0) << "Just focused";
  ASSERT_EQ(needs_focus(0x10000008, 8), 0) << "Just focused";
  ASSERT_EQ(needs_focus(0x20000004, 4), 1) << "Type mismatch";
  ASSERT_EQ(needs_focus(0x10000000, 8), 0) << "Out of the iter_res";
  insert_focus(1, "unsigned int");
  ASSERT_EQ(needs_focus(0x20000004, 4), 0) << "Just focused";

  pop_focus_context();

  // Check if it remembers the context
  ASSERT_EQ(needs_focus(0xcafe0000, 8), 1) << "Lack of focus for p[0]";
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 0) << "Focus has already been annotated";

  pop_focus_context();
  ASSERT_EQ(needs_focus(0xcafe0000, 8), 0) << "No need for focus";
  ASSERT_EQ(needs_focus(0xcafe0008, 8), 0) << "No need for focus";
}
