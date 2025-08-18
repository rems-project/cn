#include <gtest/gtest.h>

#include <cn-smt/terms.h>

// Generate vector implementations for tests
BENNET_VECTOR_IMPL(cn_term_ptr)

class CnSmtTest : public ::testing::Test {};

// Test basic constant constructors
TEST_F(CnSmtTest, BasicConstants) {
  // Test integer constant
  cn_term* z_term = cn_smt_z(42);
  ASSERT_NE(z_term, nullptr);
  EXPECT_EQ(z_term->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(z_term->base_type, CN_BASE_INTEGER));
  EXPECT_EQ(z_term->data.const_val.type, CN_CONST_Z);
  EXPECT_EQ(z_term->data.const_val.data.z, 42);

  // Test boolean constants
  cn_term* bool_true = cn_smt_bool(true);
  ASSERT_NE(bool_true, nullptr);
  EXPECT_EQ(bool_true->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(bool_true->base_type, CN_BASE_BOOL));
  EXPECT_EQ(bool_true->data.const_val.type, CN_CONST_BOOL);
  EXPECT_EQ(bool_true->data.const_val.data.boolean, true);

  cn_term* bool_false = cn_smt_bool(false);
  ASSERT_NE(bool_false, nullptr);
  EXPECT_EQ(bool_false->data.const_val.data.boolean, false);

  // Test null pointer
  cn_term* null_term = cn_smt_null();
  ASSERT_NE(null_term, nullptr);
  EXPECT_EQ(null_term->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(null_term->base_type, CN_BASE_LOC));
  EXPECT_EQ(null_term->data.const_val.type, CN_CONST_NULL);

  // Test unit value
  cn_term* unit_term = cn_smt_unit();
  ASSERT_NE(unit_term, nullptr);
  EXPECT_EQ(unit_term->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(unit_term->base_type, CN_BASE_UNIT));
  EXPECT_EQ(unit_term->data.const_val.type, CN_CONST_UNIT);
}

// Test symbolic variables
TEST_F(CnSmtTest, SymbolicVariables) {
  reset_cn_sym_counter();
  cn_term* sym_term = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  ASSERT_NE(sym_term, nullptr);
  EXPECT_EQ(sym_term->type, CN_TERM_SYM);
  EXPECT_TRUE(cn_base_type_is(sym_term->base_type, CN_BASE_INTEGER));
  EXPECT_STREQ(sym_term->data.sym.name, "x");
  EXPECT_EQ(sym_term->data.sym.id, 0);  // ID should be 0

  cn_term* bool_sym = cn_smt_sym_string("flag", cn_base_type_simple(CN_BASE_BOOL));
  ASSERT_NE(bool_sym, nullptr);
  EXPECT_TRUE(cn_base_type_is(bool_sym->base_type, CN_BASE_BOOL));
  EXPECT_STREQ(bool_sym->data.sym.name, "flag");
  EXPECT_GT(bool_sym->data.sym.id, 0);  // Second symbol should have ID > 0
  EXPECT_NE(sym_term->data.sym.id, bool_sym->data.sym.id);  // IDs should be unique
}

// Test bitvector constants
TEST_F(CnSmtTest, BitvectorConstants) {
  cn_term* bits_term = cn_smt_bits(true, 32, 0xDEADBEEF);
  ASSERT_NE(bits_term, nullptr);
  EXPECT_EQ(bits_term->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(bits_term->base_type, CN_BASE_BITS));
  EXPECT_EQ(bits_term->data.const_val.type, CN_CONST_BITS);
  EXPECT_EQ(bits_term->data.const_val.data.bits.info.is_signed, true);
  EXPECT_EQ(bits_term->data.const_val.data.bits.info.size_bits, 32);
  EXPECT_EQ(bits_term->data.const_val.data.bits.value, (int64_t)0xDEADBEEF);

  // Test unsigned bitvector
  cn_term* ubits_term = cn_smt_bits(false, 16, 0xCAFE);
  ASSERT_NE(ubits_term, nullptr);
  EXPECT_EQ(ubits_term->data.const_val.data.bits.info.is_signed, false);
  EXPECT_EQ(ubits_term->data.const_val.data.bits.info.size_bits, 16);
  EXPECT_EQ(ubits_term->data.const_val.data.bits.value, 0xCAFE);
}

// Test pointer constants
TEST_F(CnSmtTest, PointerConstants) {
  cn_term* ptr_term = cn_smt_pointer(0x1000);
  ASSERT_NE(ptr_term, nullptr);
  EXPECT_EQ(ptr_term->type, CN_TERM_CONST);
  EXPECT_TRUE(cn_base_type_is(ptr_term->base_type, CN_BASE_LOC));
  EXPECT_EQ(ptr_term->data.const_val.type, CN_CONST_POINTER);
  EXPECT_EQ(ptr_term->data.const_val.data.pointer, 0x1000);
}

// Test logical operators
TEST_F(CnSmtTest, LogicalOperators) {
  cn_term* x = cn_smt_bool(true);
  cn_term* y = cn_smt_bool(false);

  // Test NOT
  cn_term* not_x = cn_smt_not(x);
  ASSERT_NE(not_x, nullptr);
  EXPECT_EQ(not_x->type, CN_TERM_UNOP);
  EXPECT_TRUE(cn_base_type_is(not_x->base_type, CN_BASE_BOOL));
  EXPECT_EQ(not_x->data.unop.op, CN_UNOP_NOT);
  EXPECT_EQ(not_x->data.unop.operand, x);

  // Test AND
  cn_term* and_term = cn_smt_and(x, y);
  ASSERT_NE(and_term, nullptr);
  EXPECT_EQ(and_term->type, CN_TERM_BINOP);
  EXPECT_TRUE(cn_base_type_is(and_term->base_type, CN_BASE_BOOL));
  EXPECT_EQ(and_term->data.binop.op, CN_BINOP_AND);
  EXPECT_EQ(and_term->data.binop.left, x);
  EXPECT_EQ(and_term->data.binop.right, y);

  // Test OR
  cn_term* or_term = cn_smt_or(x, y);
  ASSERT_NE(or_term, nullptr);
  EXPECT_EQ(or_term->data.binop.op, CN_BINOP_OR);

  // Test IMPLIES
  cn_term* impl_term = cn_smt_implies(x, y);
  ASSERT_NE(impl_term, nullptr);
  EXPECT_EQ(impl_term->data.binop.op, CN_BINOP_IMPLIES);
}

// Test comparison operators
TEST_F(CnSmtTest, ComparisonOperators) {
  cn_term* x = cn_smt_z(10);
  cn_term* y = cn_smt_z(20);

  // Test equality
  cn_term* eq_term = cn_smt_eq(x, y);
  ASSERT_NE(eq_term, nullptr);
  EXPECT_EQ(eq_term->type, CN_TERM_BINOP);
  EXPECT_TRUE(cn_base_type_is(eq_term->base_type, CN_BASE_BOOL));
  EXPECT_EQ(eq_term->data.binop.op, CN_BINOP_EQ);
  EXPECT_EQ(eq_term->data.binop.left, x);
  EXPECT_EQ(eq_term->data.binop.right, y);

  // Test less than
  cn_term* lt_term = cn_smt_lt(x, y);
  ASSERT_NE(lt_term, nullptr);
  EXPECT_EQ(lt_term->data.binop.op, CN_BINOP_LT);

  // Test less than or equal
  cn_term* le_term = cn_smt_le(x, y);
  ASSERT_NE(le_term, nullptr);
  EXPECT_EQ(le_term->data.binop.op, CN_BINOP_LE);

  // Test greater than (should be implemented as lt(y, x))
  cn_term* gt_term = cn_smt_gt(x, y);
  ASSERT_NE(gt_term, nullptr);
  EXPECT_EQ(gt_term->data.binop.op, CN_BINOP_LT);
  EXPECT_EQ(gt_term->data.binop.left, y);  // operands swapped
  EXPECT_EQ(gt_term->data.binop.right, x);

  // Test greater than or equal (should be implemented as le(y, x))
  cn_term* ge_term = cn_smt_ge(x, y);
  ASSERT_NE(ge_term, nullptr);
  EXPECT_EQ(ge_term->data.binop.op, CN_BINOP_LE);
  EXPECT_EQ(ge_term->data.binop.left, y);  // operands swapped
  EXPECT_EQ(ge_term->data.binop.right, x);
}

// Test arithmetic operators
TEST_F(CnSmtTest, ArithmeticOperators) {
  cn_term* x = cn_smt_z(10);
  cn_term* y = cn_smt_z(5);

  // Test addition
  cn_term* add_term = cn_smt_add(x, y);
  ASSERT_NE(add_term, nullptr);
  EXPECT_EQ(add_term->type, CN_TERM_BINOP);
  EXPECT_TRUE(cn_base_type_is(add_term->base_type, CN_BASE_INTEGER));
  EXPECT_EQ(add_term->data.binop.op, CN_BINOP_ADD);
  EXPECT_EQ(add_term->data.binop.left, x);
  EXPECT_EQ(add_term->data.binop.right, y);

  // Test subtraction
  cn_term* sub_term = cn_smt_sub(x, y);
  ASSERT_NE(sub_term, nullptr);
  EXPECT_EQ(sub_term->data.binop.op, CN_BINOP_SUB);

  // Test multiplication
  cn_term* mul_term = cn_smt_mul(x, y);
  ASSERT_NE(mul_term, nullptr);
  EXPECT_EQ(mul_term->data.binop.op, CN_BINOP_MUL);

  // Test division
  cn_term* div_term = cn_smt_div(x, y);
  ASSERT_NE(div_term, nullptr);
  EXPECT_EQ(div_term->data.binop.op, CN_BINOP_DIV);

  // Test remainder
  cn_term* rem_term = cn_smt_rem(x, y);
  ASSERT_NE(rem_term, nullptr);
  EXPECT_EQ(rem_term->data.binop.op, CN_BINOP_REM);

  // Test modulo
  cn_term* mod_term = cn_smt_mod(x, y);
  ASSERT_NE(mod_term, nullptr);
  EXPECT_EQ(mod_term->data.binop.op, CN_BINOP_MOD);

  // Test min/max
  cn_term* min_term = cn_smt_min(x, y);
  ASSERT_NE(min_term, nullptr);
  EXPECT_EQ(min_term->data.binop.op, CN_BINOP_MIN);

  cn_term* max_term = cn_smt_max(x, y);
  ASSERT_NE(max_term, nullptr);
  EXPECT_EQ(max_term->data.binop.op, CN_BINOP_MAX);
}

// Test bitwise operators
TEST_F(CnSmtTest, BitwiseOperators) {
  cn_term* x = cn_smt_bits(false, 32, 0xF0F0);
  cn_term* y = cn_smt_bits(false, 32, 0x0F0F);

  // Test bitwise AND
  cn_term* bw_and = cn_smt_bw_and(x, y);
  ASSERT_NE(bw_and, nullptr);
  EXPECT_EQ(bw_and->type, CN_TERM_BINOP);
  EXPECT_EQ(bw_and->data.binop.op, CN_BINOP_BW_AND);

  // Test bitwise OR
  cn_term* bw_or = cn_smt_bw_or(x, y);
  ASSERT_NE(bw_or, nullptr);
  EXPECT_EQ(bw_or->data.binop.op, CN_BINOP_BW_OR);

  // Test bitwise XOR
  cn_term* bw_xor = cn_smt_bw_xor(x, y);
  ASSERT_NE(bw_xor, nullptr);
  EXPECT_EQ(bw_xor->data.binop.op, CN_BINOP_BW_XOR);

  // Test bitwise complement
  cn_term* bw_compl = cn_smt_bw_compl(x);
  ASSERT_NE(bw_compl, nullptr);
  EXPECT_EQ(bw_compl->type, CN_TERM_UNOP);
  EXPECT_EQ(bw_compl->data.unop.op, CN_UNOP_BW_COMPL);
  EXPECT_EQ(bw_compl->data.unop.operand, x);

  // Test shift operations
  cn_term* shift_amt = cn_smt_z(4);

  cn_term* shl_term = cn_smt_shift_left(x, shift_amt);
  ASSERT_NE(shl_term, nullptr);
  EXPECT_EQ(shl_term->data.binop.op, CN_BINOP_SHIFT_LEFT);

  cn_term* shr_term = cn_smt_shift_right(x, shift_amt);
  ASSERT_NE(shr_term, nullptr);
  EXPECT_EQ(shr_term->data.binop.op, CN_BINOP_SHIFT_RIGHT);
}

// Test if-then-else
TEST_F(CnSmtTest, IfThenElse) {
  cn_term* cond = cn_smt_bool(true);
  cn_term* then_val = cn_smt_z(42);
  cn_term* else_val = cn_smt_z(24);

  cn_term* ite_term = cn_smt_ite(cond, then_val, else_val);
  ASSERT_NE(ite_term, nullptr);
  EXPECT_EQ(ite_term->type, CN_TERM_ITE);
  EXPECT_TRUE(cn_base_type_is(
      ite_term->base_type, CN_BASE_INTEGER));  // type of then/else branches
  EXPECT_EQ(ite_term->data.ite.cond, cond);
  EXPECT_EQ(ite_term->data.ite.then_term, then_val);
  EXPECT_EQ(ite_term->data.ite.else_term, else_val);
}

// Test pointer operations
TEST_F(CnSmtTest, PointerOperations) {
  cn_term* base_ptr = cn_smt_pointer(0x1000);

  // Test member shift
  cn_term* member_shift = cn_smt_member_shift(base_ptr, 0);  // x is at offset 0
  ASSERT_NE(member_shift, nullptr);
  EXPECT_EQ(member_shift->type, CN_TERM_MEMBER_SHIFT);
  EXPECT_TRUE(cn_base_type_is(member_shift->base_type, CN_BASE_LOC));
  EXPECT_EQ(member_shift->data.member_shift.base, base_ptr);
  EXPECT_EQ(member_shift->data.member_shift.offset, 0);

  // Test array shift
  cn_term* index = cn_smt_z(5);
  cn_term* array_shift = cn_smt_array_shift(base_ptr, sizeof(int), index);
  ASSERT_NE(array_shift, nullptr);
  EXPECT_EQ(array_shift->type, CN_TERM_ARRAY_SHIFT);
  EXPECT_TRUE(cn_base_type_is(array_shift->base_type, CN_BASE_LOC));
  EXPECT_EQ(array_shift->data.array_shift.base, base_ptr);
  EXPECT_EQ(array_shift->data.array_shift.element_size, sizeof(int));
  EXPECT_EQ(array_shift->data.array_shift.index, index);
}

// Test type operations
TEST_F(CnSmtTest, TypeOperations) {
  cn_term* int_val = cn_smt_z(42);

  // Test cast
  cn_term* cast_term = cn_smt_cast(cn_base_type_simple(CN_BASE_REAL), int_val);
  ASSERT_NE(cast_term, nullptr);
  EXPECT_EQ(cast_term->type, CN_TERM_CAST);
  EXPECT_TRUE(cn_base_type_is(cast_term->base_type, CN_BASE_REAL));
  EXPECT_TRUE(cn_base_type_is(cast_term->data.cast.target_type, CN_BASE_REAL));
  EXPECT_EQ(cast_term->data.cast.value, int_val);
}

// Test map operations
TEST_F(CnSmtTest, MapOperations) {
  cn_term* default_val = cn_smt_z(0);

  // Test map_set
  cn_term* key = cn_smt_z(5);
  cn_term* value = cn_smt_z(42);
  cn_term* map = cn_smt_map_get(
      default_val, key, cn_base_type_simple(CN_BASE_MAP));  // Create a dummy map
  cn_term* map_set = cn_smt_map_set(map, key, value);
  ASSERT_NE(map_set, nullptr);
  EXPECT_EQ(map_set->type, CN_TERM_MAP_SET);
  EXPECT_TRUE(cn_base_type_is(map_set->base_type, CN_BASE_MAP));
  EXPECT_EQ(map_set->data.map_set.map, map);
  EXPECT_EQ(map_set->data.map_set.key, key);
  EXPECT_EQ(map_set->data.map_set.value, value);

  // Test map_get
  cn_term* map_get = cn_smt_map_get(map_set, key, cn_base_type_simple(CN_BASE_INTEGER));
  ASSERT_NE(map_get, nullptr);
  EXPECT_EQ(map_get->type, CN_TERM_MAP_GET);
  EXPECT_TRUE(cn_base_type_is(map_get->base_type, CN_BASE_INTEGER));
  EXPECT_EQ(map_get->data.map_get.map, map_set);
  EXPECT_EQ(map_get->data.map_get.key, key);
}

// Test function application
TEST_F(CnSmtTest, FunctionApplication) {
  cn_term* arg1 = cn_smt_z(10);
  cn_term* arg2 = cn_smt_z(20);

  // Create vector of arguments
  bennet_vector(cn_term_ptr) args;
  bennet_vector_init(cn_term_ptr)(&args);
  bennet_vector_push(cn_term_ptr)(&args, arg1);
  bennet_vector_push(cn_term_ptr)(&args, arg2);

  cn_term* apply_term = cn_smt_apply("add", cn_base_type_simple(CN_BASE_INTEGER), &args);
  ASSERT_NE(apply_term, nullptr);
  EXPECT_EQ(apply_term->type, CN_TERM_APPLY);
  EXPECT_TRUE(cn_base_type_is(apply_term->base_type, CN_BASE_INTEGER));
  EXPECT_STREQ(apply_term->data.apply.function_name, "add");
  EXPECT_EQ(bennet_vector_size(cn_term_ptr)(&apply_term->data.apply.args), 2);
  EXPECT_EQ(*bennet_vector_get(cn_term_ptr)(&apply_term->data.apply.args, 0), arg1);
  EXPECT_EQ(*bennet_vector_get(cn_term_ptr)(&apply_term->data.apply.args, 1), arg2);

  bennet_vector_free(cn_term_ptr)(&args);
}

// Test let binding
TEST_F(CnSmtTest, LetBinding) {
  cn_term* value = cn_smt_z(42);
  cn_term* body_var = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* body = cn_smt_add(body_var, cn_smt_z(1));

  cn_term* let_term = cn_smt_let("x", value, body);
  ASSERT_NE(let_term, nullptr);
  EXPECT_EQ(let_term->type, CN_TERM_LET);
  EXPECT_TRUE(cn_base_type_is(let_term->base_type, CN_BASE_INTEGER));  // type of body
  EXPECT_STREQ(let_term->data.let.var_name, "x");
  EXPECT_EQ(let_term->data.let.value, value);
  EXPECT_EQ(let_term->data.let.body, body);
}

// Test error handling with null inputs
TEST_F(CnSmtTest, ErrorHandling) {
  // Null operand tests
  EXPECT_EQ(cn_smt_not(nullptr), nullptr);
  EXPECT_EQ(cn_smt_and(nullptr, cn_smt_bool(true)), nullptr);
  EXPECT_EQ(cn_smt_and(cn_smt_bool(true), nullptr), nullptr);
  EXPECT_EQ(cn_smt_add(nullptr, cn_smt_z(5)), nullptr);
  EXPECT_EQ(cn_smt_add(cn_smt_z(5), nullptr), nullptr);

  // Null string tests
  EXPECT_EQ(cn_smt_sym_string(nullptr, cn_base_type_simple(CN_BASE_INTEGER)), nullptr);

  // Null arguments for apply
  cn_term* arg = cn_smt_z(1);
  bennet_vector(cn_term_ptr) args;
  bennet_vector_init(cn_term_ptr)(&args);
  bennet_vector_push(cn_term_ptr)(&args, arg);

  EXPECT_EQ(cn_smt_apply("func", cn_base_type_simple(CN_BASE_INTEGER), nullptr), nullptr);
  EXPECT_EQ(cn_smt_apply(nullptr, cn_base_type_simple(CN_BASE_INTEGER), &args), nullptr);

  bennet_vector_free(cn_term_ptr)(&args);
}

// Test complex expression building
TEST_F(CnSmtTest, ComplexExpressions) {
  // Build: (x + y) > (z * 2)
  cn_term* x = cn_smt_sym_string("x", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* y = cn_smt_sym_string("y", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* z = cn_smt_sym_string("z", cn_base_type_simple(CN_BASE_INTEGER));
  cn_term* two = cn_smt_z(2);

  cn_term* x_plus_y = cn_smt_add(x, y);
  cn_term* z_times_2 = cn_smt_mul(z, two);
  cn_term* comparison = cn_smt_gt(x_plus_y, z_times_2);

  ASSERT_NE(comparison, nullptr);
  EXPECT_TRUE(cn_base_type_is(comparison->base_type, CN_BASE_BOOL));

  // Should be implemented as lt(z_times_2, x_plus_y)
  EXPECT_EQ(comparison->data.binop.op, CN_BINOP_LT);
  EXPECT_EQ(comparison->data.binop.left, z_times_2);
  EXPECT_EQ(comparison->data.binop.right, x_plus_y);
}

// Test chained operations
TEST_F(CnSmtTest, ChainedOperations) {
  // Build: (a && b) || (c && d)
  cn_term* a = cn_smt_bool(true);
  cn_term* b = cn_smt_bool(false);
  cn_term* c = cn_smt_sym_string("c", cn_base_type_simple(CN_BASE_BOOL));
  cn_term* d = cn_smt_sym_string("d", cn_base_type_simple(CN_BASE_BOOL));

  cn_term* a_and_b = cn_smt_and(a, b);
  cn_term* c_and_d = cn_smt_and(c, d);
  cn_term* result = cn_smt_or(a_and_b, c_and_d);

  ASSERT_NE(result, nullptr);
  EXPECT_EQ(result->type, CN_TERM_BINOP);
  EXPECT_TRUE(cn_base_type_is(result->base_type, CN_BASE_BOOL));
  EXPECT_EQ(result->data.binop.op, CN_BINOP_OR);
  EXPECT_EQ(result->data.binop.left, a_and_b);
  EXPECT_EQ(result->data.binop.right, c_and_d);
}
