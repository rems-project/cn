#include <gtest/gtest.h>

#include <bennet/utils/vector.h>
#include <cn-smt/context.h>
#include <cn-smt/terms.h>

// Generate vector implementation for the test
BENNET_VECTOR_IMPL(cn_term_ptr)

class ContextTest : public ::testing::Test {
 protected:
  void SetUp() override {
    ctx = cn_context_create();
    ASSERT_NE(ctx, nullptr);
  }

  void TearDown() override {
    cn_context_destroy(ctx);
  }

  cn_constraint_context* ctx;
};

// Test basic context creation and destruction
TEST_F(ContextTest, CreateDestroy) {
  EXPECT_EQ(cn_context_resource_count(ctx), 0);
  EXPECT_EQ(cn_context_logical_count(ctx), 0);
  EXPECT_EQ(cn_context_first_resource(ctx), nullptr);
  EXPECT_EQ(cn_context_first_logical(ctx), nullptr);
}

// Test predicate resource constraints
TEST_F(ContextTest, AddPredicateConstraint) {
  // Create some test terms
  cn_term* pointer = cn_smt_pointer(123);

  // Create predicate constraint
  cn_resource_constraint* constraint =
      cn_resource_constraint_create_predicate(pointer, 4, 4);
  ASSERT_NE(constraint, nullptr);

  // Add to context
  cn_context_add_resource_constraint(ctx, constraint);
  EXPECT_EQ(cn_context_resource_count(ctx), 1);

  // Check constraint properties
  const cn_resource_constraint* first = cn_context_first_resource(ctx);
  ASSERT_NE(first, nullptr);
  EXPECT_EQ(first->bytes, 4);
  EXPECT_EQ(first->pointer, pointer);
}

// Test logical constraints (term)
TEST_F(ContextTest, AddLogicalTermConstraint) {
  // Create test term
  cn_term* term = cn_smt_bool(false);

  // Create logical constraint
  cn_logical_constraint* constraint = cn_logical_constraint_create_term(term);
  ASSERT_NE(constraint, nullptr);

  // Add to context
  cn_context_add_logical_constraint(ctx, constraint);
  EXPECT_EQ(cn_context_logical_count(ctx), 1);

  // Check constraint properties
  const cn_logical_constraint* first = cn_context_first_logical(ctx);
  ASSERT_NE(first, nullptr);
  EXPECT_EQ(first->type, CN_LOGICAL_TERM);
  EXPECT_EQ(first->data.term, term);
}

// Test logical constraints (forall)
TEST_F(ContextTest, AddLogicalForallConstraint) {
  // Create test term
  cn_term* body = cn_smt_bool(true);

  // Create forall constraint
  cn_sym var_name = {"x", 2};
  cn_logical_constraint* constraint = cn_logical_constraint_create_forall(
      var_name, cn_base_type_simple(CN_BASE_INTEGER), body);
  ASSERT_NE(constraint, nullptr);

  // Add to context
  cn_context_add_logical_constraint(ctx, constraint);
  EXPECT_EQ(cn_context_logical_count(ctx), 1);

  // Check constraint properties
  const cn_logical_constraint* first = cn_context_first_logical(ctx);
  ASSERT_NE(first, nullptr);
  EXPECT_EQ(first->type, CN_LOGICAL_FORALL);
  EXPECT_STREQ(first->data.forall.var_name.name, "x");
  EXPECT_TRUE(cn_base_type_is(first->data.forall.var_type, CN_BASE_INTEGER));
  EXPECT_EQ(first->data.forall.body, body);
}

// Test multiple constraints
TEST_F(ContextTest, MultipleConstraints) {
  // Add multiple resource constraints
  for (int i = 0; i < 3; i++) {
    cn_term* pointer = cn_smt_pointer(i);
    cn_resource_constraint* constraint =
        cn_resource_constraint_create_predicate(pointer, 8, 8);
    cn_context_add_resource_constraint(ctx, constraint);
  }

  // Add multiple logical constraints
  for (int i = 0; i < 2; i++) {
    cn_term* term = cn_smt_z(i + 100);
    cn_logical_constraint* constraint = cn_logical_constraint_create_term(term);
    cn_context_add_logical_constraint(ctx, constraint);
  }

  EXPECT_EQ(cn_context_resource_count(ctx), 3);
  EXPECT_EQ(cn_context_logical_count(ctx), 2);

  // Verify constraints are linked properly
  const cn_resource_constraint* resource = cn_context_first_resource(ctx);
  int resource_count = 0;
  while (resource) {
    resource_count++;
    resource = resource->next;
  }
  EXPECT_EQ(resource_count, 3);

  const cn_logical_constraint* logical = cn_context_first_logical(ctx);
  int logical_count = 0;
  while (logical) {
    logical_count++;
    logical = logical->next;
  }
  EXPECT_EQ(logical_count, 2);
}

// Test context clear
TEST_F(ContextTest, ContextClear) {
  // Add some constraints
  cn_term* pointer = cn_smt_pointer(999);
  cn_resource_constraint* resource_constraint =
      cn_resource_constraint_create_predicate(pointer, 8, 8);
  cn_context_add_resource_constraint(ctx, resource_constraint);

  cn_term* term = cn_smt_z(888);
  cn_logical_constraint* logical_constraint = cn_logical_constraint_create_term(term);
  cn_context_add_logical_constraint(ctx, logical_constraint);

  EXPECT_EQ(cn_context_resource_count(ctx), 1);
  EXPECT_EQ(cn_context_logical_count(ctx), 1);

  // Clear context
  cn_context_clear(ctx);

  EXPECT_EQ(cn_context_resource_count(ctx), 0);
  EXPECT_EQ(cn_context_logical_count(ctx), 0);
  EXPECT_EQ(cn_context_first_resource(ctx), nullptr);
  EXPECT_EQ(cn_context_first_logical(ctx), nullptr);
}

// Test print summary (mostly for coverage)
TEST_F(ContextTest, PrintSummary) {
  // Add a few constraints
  cn_term* pointer = cn_smt_pointer(123);
  cn_resource_constraint* resource_constraint =
      cn_resource_constraint_create_predicate(pointer, 8, 8);
  cn_context_add_resource_constraint(ctx, resource_constraint);

  cn_term* term = cn_smt_bool(true);
  cn_logical_constraint* logical_constraint = cn_logical_constraint_create_term(term);
  cn_context_add_logical_constraint(ctx, logical_constraint);

  // This should not crash (output goes to stdout)
  cn_context_print_summary(ctx);
}
