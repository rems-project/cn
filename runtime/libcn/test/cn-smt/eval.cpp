#include <gtest/gtest.h>

#include <cn-executable/utils.h>
#include <cn-smt/eval.h>

class CnSmtEvalTest : public ::testing::Test {};

TEST_F(CnSmtEvalTest, EvalIntegerConstant) {
  cn_term* term = cn_smt_z(42);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalBooleanConstant) {
  cn_term* term = cn_smt_bool(true);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, true);
}

TEST_F(CnSmtEvalTest, EvalNot) {
  cn_term* term = cn_smt_not(cn_smt_bool(true));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, false);
}

TEST_F(CnSmtEvalTest, EvalAdd) {
  cn_term* term = cn_smt_add(cn_smt_z(10), cn_smt_z(32));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalAnd) {
  cn_term* term = cn_smt_and(cn_smt_bool(true), cn_smt_bool(false));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, false);
}

TEST_F(CnSmtEvalTest, EvalIte) {
  cn_term* term = cn_smt_ite(cn_smt_bool(true), cn_smt_z(42), cn_smt_z(0));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);

  term = cn_smt_ite(cn_smt_bool(false), cn_smt_z(42), cn_smt_z(0));
  result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 0);
}

TEST_F(CnSmtEvalTest, EvalSub) {
  cn_term* term = cn_smt_sub(cn_smt_z(52), cn_smt_z(10));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMul) {
  cn_term* term = cn_smt_mul(cn_smt_z(6), cn_smt_z(7));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalDiv) {
  cn_term* term = cn_smt_div(cn_smt_z(84), cn_smt_z(2));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalAddBitvector) {
  cn_term* term = cn_smt_add(cn_smt_bits(true, 64, 10), cn_smt_bits(true, 64, 32));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i64* bv_res = (cn_bits_i64*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalSubBitvector) {
  cn_term* term = cn_smt_sub(cn_smt_bits(true, 64, 52), cn_smt_bits(true, 64, 10));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i64* bv_res = (cn_bits_i64*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMulBitvector) {
  cn_term* term = cn_smt_mul(cn_smt_bits(true, 64, 6), cn_smt_bits(true, 64, 7));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i64* bv_res = (cn_bits_i64*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalDivBitvector) {
  cn_term* term = cn_smt_div(cn_smt_bits(true, 64, 84), cn_smt_bits(true, 64, 2));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i64* bv_res = (cn_bits_i64*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalAddBitvector32) {
  cn_term* term = cn_smt_add(cn_smt_bits(true, 32, 10), cn_smt_bits(true, 32, 32));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalAddBitvector16) {
  cn_term* term = cn_smt_add(cn_smt_bits(true, 16, 10), cn_smt_bits(true, 16, 32));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i16* bv_res = (cn_bits_i16*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalAddBitvector8) {
  cn_term* term = cn_smt_add(cn_smt_bits(true, 8, 10), cn_smt_bits(true, 8, 32));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i8* bv_res = (cn_bits_i8*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalSubBitvector32) {
  cn_term* term = cn_smt_sub(cn_smt_bits(true, 32, 52), cn_smt_bits(true, 32, 10));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalSubBitvector16) {
  cn_term* term = cn_smt_sub(cn_smt_bits(true, 16, 52), cn_smt_bits(true, 16, 10));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i16* bv_res = (cn_bits_i16*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalSubBitvector8) {
  cn_term* term = cn_smt_sub(cn_smt_bits(true, 8, 52), cn_smt_bits(true, 8, 10));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i8* bv_res = (cn_bits_i8*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMulBitvector32) {
  cn_term* term = cn_smt_mul(cn_smt_bits(true, 32, 6), cn_smt_bits(true, 32, 7));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMulBitvector16) {
  cn_term* term = cn_smt_mul(cn_smt_bits(true, 16, 6), cn_smt_bits(true, 16, 7));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i16* bv_res = (cn_bits_i16*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMulBitvector8) {
  cn_term* term = cn_smt_mul(cn_smt_bits(true, 8, 6), cn_smt_bits(true, 8, 7));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i8* bv_res = (cn_bits_i8*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalDivBitvector32) {
  cn_term* term = cn_smt_div(cn_smt_bits(true, 32, 84), cn_smt_bits(true, 32, 2));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalDivBitvector16) {
  cn_term* term = cn_smt_div(cn_smt_bits(true, 16, 84), cn_smt_bits(true, 16, 2));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i16* bv_res = (cn_bits_i16*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalDivBitvector8) {
  cn_term* term = cn_smt_div(cn_smt_bits(true, 8, 84), cn_smt_bits(true, 8, 2));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i8* bv_res = (cn_bits_i8*)result;
  EXPECT_EQ(bv_res->val, 42);
}

// ========== NEW CONSTANT TESTS ==========
TEST_F(CnSmtEvalTest, EvalPointerConstant) {
  cn_term* term = cn_smt_pointer(0x12345678);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_pointer* ptr_res = (cn_pointer*)result;
  EXPECT_EQ((uintptr_t)ptr_res->ptr, 0x12345678);
}

TEST_F(CnSmtEvalTest, EvalNullConstant) {
  cn_term* term = cn_smt_null();
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_pointer* ptr_res = (cn_pointer*)result;
  EXPECT_EQ(ptr_res->ptr, nullptr);
}

TEST_F(CnSmtEvalTest, EvalDefaultConstant) {
  cn_term* term = cn_smt_default(cn_base_type_simple(CN_BASE_INTEGER));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 0);
}

// ========== NEW UNARY OPERATION TESTS ==========
TEST_F(CnSmtEvalTest, EvalNegateInteger) {
  cn_term* term = cn_smt_sub(cn_smt_z(0), cn_smt_z(42));  // Negate via subtraction
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, -42);
}

TEST_F(CnSmtEvalTest, EvalBitwiseComplI32) {
  // Create a term with BW_COMPL operation manually since no smart constructor exists
  cn_term* operand = cn_smt_bits(true, 32, 0xAAAAAAAA);
  cn_term* term = (cn_term*)malloc(sizeof(cn_term));
  term->type = CN_TERM_UNOP;
  term->base_type = cn_base_type_bits(true, 32);
  term->data.unop.op = CN_UNOP_BW_COMPL;
  term->data.unop.operand = operand;

  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ((uint32_t)bv_res->val, 0x55555555);

  free(term);
}

// ========== UNSIGNED BITVECTOR TESTS ==========
TEST_F(CnSmtEvalTest, EvalAddBitvectorU64) {
  cn_term* term = cn_smt_add(cn_smt_bits(false, 64, 10), cn_smt_bits(false, 64, 32));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_u64* bv_res = (cn_bits_u64*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalSubBitvectorU64) {
  cn_term* term = cn_smt_sub(cn_smt_bits(false, 64, 52), cn_smt_bits(false, 64, 10));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_u64* bv_res = (cn_bits_u64*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMulBitvectorU32) {
  cn_term* term = cn_smt_mul(cn_smt_bits(false, 32, 6), cn_smt_bits(false, 32, 7));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_u32* bv_res = (cn_bits_u32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalDivBitvectorU32) {
  cn_term* term = cn_smt_div(cn_smt_bits(false, 32, 84), cn_smt_bits(false, 32, 2));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_u32* bv_res = (cn_bits_u32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

// ========== EXTENDED SIGNED BITVECTOR TESTS ==========
TEST_F(CnSmtEvalTest, EvalRemBitvectorI32) {
  cn_term* term = cn_smt_rem(cn_smt_bits(true, 32, 47), cn_smt_bits(true, 32, 5));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 2);
}

TEST_F(CnSmtEvalTest, EvalModBitvectorI32) {
  cn_term* term = cn_smt_mod(cn_smt_bits(true, 32, 47), cn_smt_bits(true, 32, 5));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 2);
}

TEST_F(CnSmtEvalTest, EvalBwAndBitvectorI32) {
  cn_term* term = cn_smt_bw_and(cn_smt_bits(true, 32, 0xFF), cn_smt_bits(true, 32, 0x0F));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 0x0F);
}

TEST_F(CnSmtEvalTest, EvalBwOrBitvectorI32) {
  cn_term* term = cn_smt_bw_or(cn_smt_bits(true, 32, 0xF0), cn_smt_bits(true, 32, 0x0F));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 0xFF);
}

TEST_F(CnSmtEvalTest, EvalBwXorBitvectorI32) {
  cn_term* term = cn_smt_bw_xor(cn_smt_bits(true, 32, 0xAA), cn_smt_bits(true, 32, 0x55));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 0xFF);
}

TEST_F(CnSmtEvalTest, EvalShiftLeftBitvectorI32) {
  cn_term* term = cn_smt_shift_left(cn_smt_bits(true, 32, 21), cn_smt_bits(true, 32, 1));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalShiftRightBitvectorI32) {
  cn_term* term = cn_smt_shift_right(cn_smt_bits(true, 32, 84), cn_smt_bits(true, 32, 1));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalLtBitvectorI32) {
  cn_term* term = cn_smt_lt(cn_smt_bits(true, 32, 10), cn_smt_bits(true, 32, 42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, true);
}

TEST_F(CnSmtEvalTest, EvalLeBitvectorI32) {
  cn_term* term = cn_smt_le(cn_smt_bits(true, 32, 42), cn_smt_bits(true, 32, 42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, true);
}

TEST_F(CnSmtEvalTest, EvalMinBitvectorI32) {
  cn_term* term = cn_smt_min(cn_smt_bits(true, 32, 100), cn_smt_bits(true, 32, 42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMaxBitvectorI32) {
  cn_term* term = cn_smt_max(cn_smt_bits(true, 32, 10), cn_smt_bits(true, 32, 42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

// ========== EXTENDED INTEGER TESTS ==========
TEST_F(CnSmtEvalTest, EvalLtInteger) {
  cn_term* term = cn_smt_lt(cn_smt_z(10), cn_smt_z(42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, true);
}

TEST_F(CnSmtEvalTest, EvalLeInteger) {
  cn_term* term = cn_smt_le(cn_smt_z(42), cn_smt_z(42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, true);
}

TEST_F(CnSmtEvalTest, EvalMinInteger) {
  cn_term* term = cn_smt_min(cn_smt_z(100), cn_smt_z(42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalMaxInteger) {
  cn_term* term = cn_smt_max(cn_smt_z(10), cn_smt_z(42));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalRemInteger) {
  cn_term* term = cn_smt_rem(cn_smt_z(47), cn_smt_z(5));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 2);
}

TEST_F(CnSmtEvalTest, EvalModInteger) {
  cn_term* term = cn_smt_mod(cn_smt_z(47), cn_smt_z(5));
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 2);
}

// ========== POINTER OPERATION TESTS ==========
TEST_F(CnSmtEvalTest, EvalPointerEquality) {
  cn_term* ptr1 = cn_smt_pointer(0x12345678);
  cn_term* ptr2 = cn_smt_pointer(0x12345678);
  cn_term* term = cn_smt_eq(ptr1, ptr2);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_bool* bool_res = (cn_bool*)result;
  EXPECT_EQ(bool_res->val, true);
}

// ========== TYPE CASTING TESTS ==========
TEST_F(CnSmtEvalTest, EvalCastIntegerToBitvector) {
  cn_term* int_term = cn_smt_z(42);
  cn_term* term = cn_smt_cast(cn_base_type_bits(true, 32), int_term);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  // Cast should respect the target type (32-bit signed bitvector)
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalCastBitvectorToInteger) {
  cn_term* bv_term = cn_smt_bits(true, 32, 42);
  cn_term* term = cn_smt_cast(cn_base_type_simple(CN_BASE_INTEGER), bv_term);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalCastBitvectorToBitvector) {
  cn_term* bv_term = cn_smt_bits(true, 32, 42);
  cn_term* term = cn_smt_cast(cn_base_type_bits(true, 32), bv_term);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  // For now, bitvector-to-bitvector casts return the same value
  cn_bits_i32* bv_res = (cn_bits_i32*)result;
  EXPECT_EQ(bv_res->val, 42);
}

TEST_F(CnSmtEvalTest, EvalCastPointerToInteger) {
  cn_term* ptr_term = cn_smt_pointer(0x42);
  cn_term* term = cn_smt_cast(cn_base_type_simple(CN_BASE_INTEGER), ptr_term);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_integer* int_res = (cn_integer*)result;
  EXPECT_EQ(int_res->val, 0x42);
}

// ========== MAP OPERATION TESTS ==========

// ========== MEMORY OPERATION TESTS ==========

TEST_F(CnSmtEvalTest, EvalArrayShift) {
  // Test array pointer arithmetic: base + index * element_size
  cn_term* base = cn_smt_pointer(0x1000);
  cn_term* index = cn_smt_z(5);
  cn_term* term = cn_smt_array_shift(base, sizeof(int), index);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_pointer* ptr_res = (cn_pointer*)result;
  uintptr_t expected = 0x1000 + (5 * sizeof(int));
  EXPECT_EQ((uintptr_t)ptr_res->ptr, expected);
}

TEST_F(CnSmtEvalTest, EvalArrayShiftWithBitvectorIndex) {
  // Test array pointer arithmetic with bitvector index
  cn_term* base = cn_smt_pointer(0x2000);
  cn_term* index = cn_smt_bits(true, 32, 3);  // 32-bit signed bitvector with value 3
  cn_term* term = cn_smt_array_shift(base, sizeof(double), index);
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_pointer* ptr_res = (cn_pointer*)result;
  uintptr_t expected = 0x2000 + (3 * sizeof(double));
  EXPECT_EQ((uintptr_t)ptr_res->ptr, expected);
}

TEST_F(CnSmtEvalTest, EvalArrayShiftWithUnsignedBitvectorIndex) {
  // Test array pointer arithmetic with unsigned bitvector index
  cn_term* base = cn_smt_pointer(0x3000);
  cn_term* index = cn_smt_bits(false, 16, 7);  // 16-bit unsigned bitvector with value 7
  cn_term* term = cn_smt_array_shift(base, 1, index);  // 1-byte elements
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_pointer* ptr_res = (cn_pointer*)result;
  uintptr_t expected = 0x3000 + (7 * 1);
  EXPECT_EQ((uintptr_t)ptr_res->ptr, expected);
}

TEST_F(CnSmtEvalTest, EvalMemberShift) {
  // Test struct member pointer arithmetic: base + member_offset
  cn_term* base = cn_smt_pointer(0x2000);
  cn_term* term = cn_smt_member_shift(base, sizeof(int));  // y is at offset sizeof(int)
  void* result = cn_eval_term(term);
  ASSERT_NE(result, nullptr);
  cn_pointer* ptr_res = (cn_pointer*)result;
  uintptr_t expected = 0x2000 + sizeof(int);  // y is at offset sizeof(int)
  EXPECT_EQ((uintptr_t)ptr_res->ptr, expected);
}
