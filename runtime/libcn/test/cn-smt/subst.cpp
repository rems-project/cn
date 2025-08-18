#include <gtest/gtest.h>

#include <cn-smt/subst.h>
#include <cn-smt/terms.h>

// Generate vector and hash table implementations for tests
BENNET_VECTOR_IMPL(cn_term_ptr)
BENNET_HASH_TABLE_IMPL(const_char_ptr, cn_term_ptr)

class CnSmtSubstTest : public ::testing::Test {
 protected:
  void SetUp() override {
    reset_cn_sym_counter();
  }
};

// Test basic symbol substitution
TEST_F(CnSmtSubstTest, BasicSymbolSubstitution) {
  // Create a symbol and a constant
  cn_term* symbol = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* constant = cn_smt_z(42);

  ASSERT_NE(symbol, nullptr);
  ASSERT_NE(constant, nullptr);

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Add substitution x -> 42
  cn_add_substitution(subst_table, symbol->data.sym.id, constant);

  // Perform substitution
  cn_term* result = cn_subst_term(symbol, subst_table);

  // Result should be a constant with value 42
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(result->base_type, CN_BASE_INTEGER));
  EXPECT_EQ(result->data.const_val.type, CN_CONST_Z);
  EXPECT_EQ(result->data.const_val.data.z, 42);

  // Result should be a different object (copy)
  EXPECT_NE(result, constant);

  cn_free_subst_table(subst_table);
}

// Test symbol not in substitution table
TEST_F(CnSmtSubstTest, SymbolNotInTable) {
  cn_term* symbol = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  ASSERT_NE(symbol, nullptr);

  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Don't add symbol to table
  cn_term* result = cn_subst_term(symbol, subst_table);

  // Should return original symbol unchanged
  EXPECT_EQ(result, symbol);

  cn_free_subst_table(subst_table);
}

// Test constant term (no substitution needed)
TEST_F(CnSmtSubstTest, ConstantTerm) {
  cn_term* constant = cn_smt_z(100);
  ASSERT_NE(constant, nullptr);

  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  cn_term* result = cn_subst_term(constant, subst_table);

  // Should return original constant unchanged
  EXPECT_EQ(result, constant);

  cn_free_subst_table(subst_table);
}

// Test substitution in binary operation
TEST_F(CnSmtSubstTest, BinaryOperationSubstitution) {
  // Create symbols and constants
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y_sym = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* x_const = cn_smt_z(10);
  cn_term* y_const = cn_smt_z(20);

  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(y_sym, nullptr);
  ASSERT_NE(x_const, nullptr);
  ASSERT_NE(y_const, nullptr);

  // Create binary operation: x + y
  cn_term* add_expr = cn_smt_add(x_sym, y_sym);
  ASSERT_NE(add_expr, nullptr);

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Add substitutions x -> 10, y -> 20
  cn_add_substitution(subst_table, x_sym->data.sym.id, x_const);
  cn_add_substitution(subst_table, y_sym->data.sym.id, y_const);

  // Perform substitution
  cn_term* result = cn_subst_term(add_expr, subst_table);

  // Result should be a new binary operation with constants
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_BINOP);
  EXPECT_EQ(result->data.binop.op, CN_BINOP_ADD);

  // Left operand should be constant 10
  ASSERT_NE(result->data.binop.left, nullptr);
  EXPECT_EQ(result->data.binop.left->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.binop.left->data.const_val.data.z, 10);

  // Right operand should be constant 20
  ASSERT_NE(result->data.binop.right, nullptr);
  EXPECT_EQ(result->data.binop.right->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.binop.right->data.const_val.data.z, 20);

  // Result should be different from original
  EXPECT_NE(result, add_expr);

  cn_free_subst_table(subst_table);
}

// Test partial substitution in binary operation
TEST_F(CnSmtSubstTest, PartialSubstitution) {
  // Create symbols and constant
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y_sym = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* x_const = cn_smt_z(15);

  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(y_sym, nullptr);
  ASSERT_NE(x_const, nullptr);

  // Create binary operation: x + y
  cn_term* add_expr = cn_smt_add(x_sym, y_sym);
  ASSERT_NE(add_expr, nullptr);

  // Create substitution table with only x -> 15
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);
  cn_add_substitution(subst_table, x_sym->data.sym.id, x_const);

  // Perform substitution
  cn_term* result = cn_subst_term(add_expr, subst_table);

  // Result should be a new binary operation
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_BINOP);
  EXPECT_EQ(result->data.binop.op, CN_BINOP_ADD);

  // Left operand should be constant 15
  ASSERT_NE(result->data.binop.left, nullptr);
  EXPECT_EQ(result->data.binop.left->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.binop.left->data.const_val.data.z, 15);

  // Right operand should still be the original symbol y
  EXPECT_EQ(result->data.binop.right, y_sym);

  cn_free_subst_table(subst_table);
}

// Test substitution in unary operation
TEST_F(CnSmtSubstTest, UnaryOperationSubstitution) {
  // Create symbol and constant
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_BOOL));
  cn_term* x_const = cn_smt_bool(true);

  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(x_const, nullptr);

  // Create unary operation: NOT x
  cn_term* not_expr = cn_smt_not(x_sym);
  ASSERT_NE(not_expr, nullptr);

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);
  cn_add_substitution(subst_table, x_sym->data.sym.id, x_const);

  // Perform substitution
  cn_term* result = cn_subst_term(not_expr, subst_table);

  // Result should be a new unary operation with constant
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_UNOP);
  EXPECT_EQ(result->data.unop.op, CN_UNOP_NOT);

  // Operand should be constant true
  ASSERT_NE(result->data.unop.operand, nullptr);
  EXPECT_EQ(result->data.unop.operand->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.unop.operand->data.const_val.data.boolean, true);

  cn_free_subst_table(subst_table);
}

// Test substitution in if-then-else
TEST_F(CnSmtSubstTest, IteSubstitution) {
  // Create symbols and constants
  cn_term* cond_sym = cn_smt_sym_string("cond", cn_base_type_simple(CN_BASE_BOOL));
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y_sym = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* cond_const = cn_smt_bool(true);
  cn_term* x_const = cn_smt_z(10);

  ASSERT_NE(cond_sym, nullptr);
  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(y_sym, nullptr);
  ASSERT_NE(cond_const, nullptr);
  ASSERT_NE(x_const, nullptr);

  // Create ITE: if cond then x else y
  cn_term* ite_expr = cn_smt_ite(cond_sym, x_sym, y_sym);
  ASSERT_NE(ite_expr, nullptr);

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);
  cn_add_substitution(subst_table, cond_sym->data.sym.id, cond_const);
  cn_add_substitution(subst_table, x_sym->data.sym.id, x_const);
  // Don't substitute y

  // Perform substitution
  cn_term* result = cn_subst_term(ite_expr, subst_table);

  // Result should be new ITE with substituted parts
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_ITE);

  // Condition should be constant true
  ASSERT_NE(result->data.ite.cond, nullptr);
  EXPECT_EQ(result->data.ite.cond->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.ite.cond->data.const_val.data.boolean, true);

  // Then branch should be constant 10
  ASSERT_NE(result->data.ite.then_term, nullptr);
  EXPECT_EQ(result->data.ite.then_term->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.ite.then_term->data.const_val.data.z, 10);

  // Else branch should still be original symbol
  EXPECT_EQ(result->data.ite.else_term, y_sym);

  cn_free_subst_table(subst_table);
}

// Test no substitution returns original
TEST_F(CnSmtSubstTest, NoSubstitutionReturnsOriginal) {
  // Create expression with no symbols to substitute
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y_sym = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* add_expr = cn_smt_add(x_sym, y_sym);

  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(y_sym, nullptr);
  ASSERT_NE(add_expr, nullptr);

  // Create empty substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Perform substitution
  cn_term* result = cn_subst_term(add_expr, subst_table);

  // Should return original expression unchanged
  EXPECT_EQ(result, add_expr);

  cn_free_subst_table(subst_table);
}

// Test different symbol types
TEST_F(CnSmtSubstTest, DifferentSymbolTypes) {
  // Create symbols of different types
  cn_term* bool_sym = cn_smt_sym_string("b", cn_base_type_simple(CN_BASE_BOOL));
  cn_term* int_sym = cn_smt_sym_string("i", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* bool_const = cn_smt_bool(false);
  cn_term* int_const = cn_smt_z(999);

  ASSERT_NE(bool_sym, nullptr);
  ASSERT_NE(int_sym, nullptr);
  ASSERT_NE(bool_const, nullptr);
  ASSERT_NE(int_const, nullptr);

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);
  cn_add_substitution(subst_table, bool_sym->data.sym.id, bool_const);
  cn_add_substitution(subst_table, int_sym->data.sym.id, int_const);

  // Test bool substitution
  cn_term* bool_result = cn_subst_term(bool_sym, subst_table);
  ASSERT_NE(bool_result, nullptr);
  EXPECT_EQ(bool_result->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(bool_result->base_type, CN_BASE_BOOL));
  EXPECT_EQ(bool_result->data.const_val.data.boolean, false);

  // Test int substitution
  cn_term* int_result = cn_subst_term(int_sym, subst_table);
  ASSERT_NE(int_result, nullptr);
  EXPECT_EQ(int_result->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(int_result->base_type, CN_BASE_INTEGER));
  EXPECT_EQ(int_result->data.const_val.data.z, 999);

  cn_free_subst_table(subst_table);
}

// Test substitution in EACHI term
TEST_F(CnSmtSubstTest, EachISubstitution) {
  // Create symbols and constants
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y_sym = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* x_const = cn_smt_z(42);

  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(y_sym, nullptr);
  ASSERT_NE(x_const, nullptr);

  // Create an EACHI term: ∀i∈[0,10]. x + y
  cn_term* body_expr = cn_smt_add(x_sym, y_sym);
  cn_term* eachi_term = (cn_term*)malloc(sizeof(cn_term));
  eachi_term->type = CN_TERM_EACHI;
  eachi_term->base_type = cn_base_type_simple(CN_BASE_INTEGER);
  eachi_term->data.eachi.start = 0;
  eachi_term->data.eachi.var_name = "i";
  eachi_term->data.eachi.var_type = cn_base_type_simple(CN_BASE_INTEGER);
  eachi_term->data.eachi.end = 10;
  eachi_term->data.eachi.body = body_expr;

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Add substitution x -> 42
  cn_add_substitution(subst_table, x_sym->data.sym.id, x_const);

  // Perform substitution
  cn_term* result = cn_subst_term(eachi_term, subst_table);

  // Result should be a new EACHI term with substituted body
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_EACHI);
  EXPECT_NE(result, eachi_term);  // Should be a new term

  // Check that the body has been substituted
  ASSERT_NE(result->data.eachi.body, nullptr);
  EXPECT_EQ(result->data.eachi.body->type, CN_TERM_BINOP);
  EXPECT_EQ(result->data.eachi.body->data.binop.op, CN_BINOP_ADD);

  // Left operand should be constant 42
  ASSERT_NE(result->data.eachi.body->data.binop.left, nullptr);
  EXPECT_EQ(result->data.eachi.body->data.binop.left->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.eachi.body->data.binop.left->data.const_val.data.z, 42);

  // Right operand should still be the original symbol y
  EXPECT_EQ(result->data.eachi.body->data.binop.right, y_sym);

  free(eachi_term);
  cn_free_subst_table(subst_table);
}

// Test substitution in STRUCT term
TEST_F(CnSmtSubstTest, StructSubstitution) {
  // Create symbols and constants
  cn_term* x_sym = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y_sym = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* x_const = cn_smt_z(100);

  ASSERT_NE(x_sym, nullptr);
  ASSERT_NE(y_sym, nullptr);
  ASSERT_NE(x_const, nullptr);

  // Create a STRUCT term with symbols as members
  cn_term* struct_term = (cn_term*)malloc(sizeof(cn_term));
  struct_term->type = CN_TERM_STRUCT;
  struct_term->base_type = cn_base_type_simple(CN_BASE_STRUCT);
  struct_term->data.struct_val.tag = "test_tag";
  bennet_hash_table_init(const_char_ptr, cn_term_ptr)(
      &struct_term->data.struct_val.members,
      bennet_hash_const_char_ptr,
      bennet_eq_const_char_ptr);
  bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
      &struct_term->data.struct_val.members, "field1", x_sym);
  bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
      &struct_term->data.struct_val.members, "field2", y_sym);

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Add substitution x -> 100
  cn_add_substitution(subst_table, x_sym->data.sym.id, x_const);

  // Perform substitution
  cn_term* result = cn_subst_term(struct_term, subst_table);

  // Result should be a new STRUCT term with substituted members
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_STRUCT);
  EXPECT_NE(result, struct_term);  // Should be a new term

  // Check that field1 has been substituted
  bennet_optional(cn_term_ptr) field1_result = bennet_hash_table_get(
      const_char_ptr, cn_term_ptr)(&result->data.struct_val.members, "field1");
  ASSERT_TRUE(bennet_optional_is_some(field1_result));
  cn_term* field1_value = bennet_optional_unwrap(field1_result);
  EXPECT_EQ(field1_value->type, CN_TERM_CONST);
  EXPECT_EQ(field1_value->data.const_val.data.z, 100);

  // Check that field2 is still the original symbol
  bennet_optional(cn_term_ptr) field2_result = bennet_hash_table_get(
      const_char_ptr, cn_term_ptr)(&result->data.struct_val.members, "field2");
  ASSERT_TRUE(bennet_optional_is_some(field2_result));
  cn_term* field2_value = bennet_optional_unwrap(field2_result);
  EXPECT_EQ(field2_value, y_sym);

  bennet_hash_table_free(const_char_ptr, cn_term_ptr)(
      &struct_term->data.struct_val.members);
  free(struct_term);
  cn_free_subst_table(subst_table);
}

// Test substitution in ARRAY_SHIFT term
TEST_F(CnSmtSubstTest, ArrayShiftSubstitution) {
  // Create symbols and constants
  cn_term* base_sym = cn_smt_sym_string("base", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* index_sym = cn_smt_sym_string("index", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* base_const = cn_smt_z(100);
  cn_term* index_const = cn_smt_z(5);

  ASSERT_NE(base_sym, nullptr);
  ASSERT_NE(index_sym, nullptr);
  ASSERT_NE(base_const, nullptr);
  ASSERT_NE(index_const, nullptr);

  // Create an ARRAY_SHIFT term
  cn_term* array_shift_term = (cn_term*)malloc(sizeof(cn_term));
  array_shift_term->type = CN_TERM_ARRAY_SHIFT;
  array_shift_term->base_type = cn_base_type_simple(CN_BASE_INTEGER);
  array_shift_term->data.array_shift.base = base_sym;
  array_shift_term->data.array_shift.element_size = 8;
  array_shift_term->data.array_shift.index = index_sym;

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Add substitutions
  cn_add_substitution(subst_table, base_sym->data.sym.id, base_const);
  cn_add_substitution(subst_table, index_sym->data.sym.id, index_const);

  // Perform substitution
  cn_term* result = cn_subst_term(array_shift_term, subst_table);

  // Check that the result is a new ARRAY_SHIFT term with substituted parts
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_ARRAY_SHIFT);
  EXPECT_NE(result, array_shift_term);  // Should be a new term

  // Check substituted base
  ASSERT_NE(result->data.array_shift.base, nullptr);
  EXPECT_EQ(result->data.array_shift.base->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.array_shift.base->data.const_val.data.z, 100);

  // Check substituted index
  ASSERT_NE(result->data.array_shift.index, nullptr);
  EXPECT_EQ(result->data.array_shift.index->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.array_shift.index->data.const_val.data.z, 5);

  free(array_shift_term);
  cn_free_subst_table(subst_table);
}

// Test substitution in MAP_SET term
TEST_F(CnSmtSubstTest, MapSetSubstitution) {
  // Create symbols and constants
  cn_term* map_sym = cn_smt_sym_string("map", cn_base_type_simple(CN_BASE_MAP));
  cn_term* key_sym = cn_smt_sym_string("key", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* value_sym = cn_smt_sym_string("value", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* map_const = cn_smt_z(100);  // Simplified for test
  cn_term* key_const = cn_smt_z(5);
  cn_term* value_const = cn_smt_z(42);

  ASSERT_NE(map_sym, nullptr);
  ASSERT_NE(key_sym, nullptr);
  ASSERT_NE(value_sym, nullptr);
  ASSERT_NE(map_const, nullptr);
  ASSERT_NE(key_const, nullptr);
  ASSERT_NE(value_const, nullptr);

  // Create a MAP_SET term
  cn_term* map_set_term = (cn_term*)malloc(sizeof(cn_term));
  map_set_term->type = CN_TERM_MAP_SET;
  map_set_term->base_type = cn_base_type_simple(CN_BASE_MAP);
  map_set_term->data.map_set.map = map_sym;
  map_set_term->data.map_set.key = key_sym;
  map_set_term->data.map_set.value = value_sym;

  // Create substitution table
  auto* subst_table = cn_create_subst_table();
  ASSERT_NE(subst_table, nullptr);

  // Add substitutions
  cn_add_substitution(subst_table, map_sym->data.sym.id, map_const);
  cn_add_substitution(subst_table, key_sym->data.sym.id, key_const);
  cn_add_substitution(subst_table, value_sym->data.sym.id, value_const);

  // Perform substitution
  cn_term* result = cn_subst_term(map_set_term, subst_table);

  // Result should be a new MAP_SET term with substituted parts
  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_MAP_SET);
  EXPECT_NE(result, map_set_term);  // Should be a new term

  // Check substituted map
  ASSERT_NE(result->data.map_set.map, nullptr);
  EXPECT_EQ(result->data.map_set.map->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.map_set.map->data.const_val.data.z, 100);

  // Check substituted key
  ASSERT_NE(result->data.map_set.key, nullptr);
  EXPECT_EQ(result->data.map_set.key->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.map_set.key->data.const_val.data.z, 5);

  // Check substituted value
  ASSERT_NE(result->data.map_set.value, nullptr);
  EXPECT_EQ(result->data.map_set.value->type, CN_TERM_CONST);
  EXPECT_EQ(result->data.map_set.value->data.const_val.data.z, 42);

  free(map_set_term);
  cn_free_subst_table(subst_table);
}
