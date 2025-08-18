#include <gtest/gtest.h>

#include <cn-smt/from_smt.h>
#include <cn-smt/sexp.h>

class FromSmtTest : public ::testing::Test {
 protected:
  void SetUp() override {}
  void TearDown() override {}
};

TEST_F(FromSmtTest, ToBoolTrue) {
  sexp_t* true_sexp = sexp_atom("true");
  EXPECT_TRUE(to_bool(true_sexp));
  sexp_free(true_sexp);
}

TEST_F(FromSmtTest, ToBoolFalse) {
  sexp_t* false_sexp = sexp_atom("false");
  EXPECT_FALSE(to_bool(false_sexp));
  sexp_free(false_sexp);
}

TEST_F(FromSmtTest, ToZPositive) {
  sexp_t* num_sexp = sexp_atom("42");
  EXPECT_EQ(to_z(num_sexp), 42);
  sexp_free(num_sexp);
}

TEST_F(FromSmtTest, ToZNegative) {
  sexp_t* minus_atom = sexp_atom("-");
  sexp_t* num_atom = sexp_atom("42");
  sexp_t* elements[] = {minus_atom, num_atom};
  sexp_t* neg_sexp = sexp_list(elements, 2);

  EXPECT_EQ(to_z(neg_sexp), -42);

  sexp_free(minus_atom);
  sexp_free(num_atom);
  sexp_free(neg_sexp);
}

TEST_F(FromSmtTest, ToBitsHex) {
  sexp_t* hex_sexp = sexp_atom("#x0F");
  EXPECT_EQ(to_bits(8, false, hex_sexp), 15);
  sexp_free(hex_sexp);
}

TEST_F(FromSmtTest, ToBitsBinary) {
  sexp_t* bin_sexp = sexp_atom("#b1111");
  EXPECT_EQ(to_bits(4, false, bin_sexp), 15);
  sexp_free(bin_sexp);
}

TEST_F(FromSmtTest, ToConSimple) {
  sexp_t* con_sexp = sexp_atom("None");
  constructor_result_t result = to_con(con_sexp);

  EXPECT_STREQ(result.name, "None");
  EXPECT_EQ(result.field_count, 0);
  EXPECT_EQ(result.fields, nullptr);

  free_constructor_result(&result);
  sexp_free(con_sexp);
}

TEST_F(FromSmtTest, ToConWithFields) {
  sexp_t* some_atom = sexp_atom("Some");
  sexp_t* value_atom = sexp_atom("42");
  sexp_t* elements[] = {some_atom, value_atom};
  sexp_t* con_sexp = sexp_list(elements, 2);

  constructor_result_t result = to_con(con_sexp);

  EXPECT_STREQ(result.name, "Some");
  EXPECT_EQ(result.field_count, 1);
  EXPECT_NE(result.fields, nullptr);
  ASSERT_EQ(result.field_count, 1);      // Ensure fields array size matches expected
  EXPECT_NE(result.fields[0], nullptr);  // Verify first field is valid
  EXPECT_STREQ(sexp_to_string(result.fields[0]), "42");  // Check field content

  free_constructor_result(&result);
  sexp_free(some_atom);
  sexp_free(value_atom);
  sexp_free(con_sexp);
}
