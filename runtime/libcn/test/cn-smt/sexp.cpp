#include <gtest/gtest.h>

#include <cstdio>
#include <cstring>
#include <string>

#include <cn-smt/sexp.h>

class SexpCommandsTest : public ::testing::Test {
 protected:
  void SetUp() override {
    // Setup if needed
  }

  void TearDown() override {
    // Cleanup if needed
  }
};

TEST_F(SexpCommandsTest, SimpleCommand) {
  const char* strs[] = {"set-logic", "QF_LIA"};
  sexp_t* cmd = simple_command(strs, 2);

  ASSERT_NE(cmd, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(cmd), "(set-logic QF_LIA)");

  sexp_free(cmd);
}

TEST_F(SexpCommandsTest, SetOption) {
  sexp_t* opt = set_option(":produce-models", "true");

  ASSERT_NE(opt, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(opt), "(set-option :produce-models true)");

  sexp_free(opt);
}

TEST_F(SexpCommandsTest, SetLogic) {
  sexp_t* logic = set_logic("QF_LIA");

  ASSERT_NE(logic, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(logic), "(set-logic QF_LIA)");

  sexp_free(logic);
}

TEST_F(SexpCommandsTest, PushPop) {
  sexp_t* push_cmd = push(1);
  sexp_t* pop_cmd = pop(1);

  ASSERT_NE(push_cmd, nullptr);
  ASSERT_NE(pop_cmd, nullptr);

  ASSERT_STRCASEEQ(sexp_to_string(push_cmd), "(push 1)");
  ASSERT_STRCASEEQ(sexp_to_string(pop_cmd), "(pop 1)");

  sexp_free(push_cmd);
  sexp_free(pop_cmd);
}

TEST_F(SexpCommandsTest, DeclareSort) {
  sexp_t* sort_decl = declare_sort("MySort", 0);

  ASSERT_NE(sort_decl, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(sort_decl), "(declare-sort MySort 0)");

  sexp_free(sort_decl);
}

TEST_F(SexpCommandsTest, DeclareConst) {
  sexp_t* int_type = t_int();
  sexp_t* const_decl = declare_const("x", int_type);

  ASSERT_NE(const_decl, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(const_decl), "(declare-fun x () Int)");

  sexp_free(const_decl);
  sexp_free(int_type);
}

TEST_F(SexpCommandsTest, DeclareFun) {
  sexp_t* int_type1 = t_int();
  sexp_t* int_type2 = t_int();
  sexp_t* bool_type = t_bool();
  sexp_t* param_types[] = {int_type1, int_type2};
  sexp_t* fun_decl = declare_fun("f", param_types, 2, bool_type);

  ASSERT_NE(fun_decl, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(fun_decl), "(declare-fun f (Int Int) Bool)");

  sexp_free(fun_decl);
  sexp_free(int_type1);
  sexp_free(int_type2);
  sexp_free(bool_type);
}

TEST_F(SexpCommandsTest, DefineConst) {
  sexp_t* int_type = t_int();
  sexp_t* value = int_k(42);
  sexp_t* const_def = define_const("c", int_type, value);

  ASSERT_NE(const_def, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(const_def), "(define-fun c () Int 42)");

  sexp_free(const_def);
  sexp_free(int_type);
  sexp_free(value);
}

TEST_F(SexpCommandsTest, Assume) {
  sexp_t* x = symbol("x");
  sexp_t* zero = int_k(0);
  sexp_t* gt_expr = num_gt(x, zero);
  sexp_t* assertion = assume(gt_expr);

  ASSERT_NE(assertion, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(assertion), "(assert (> x 0))");

  sexp_free(assertion);
  sexp_free(x);
  sexp_free(zero);
  sexp_free(gt_expr);
}

TEST_F(SexpCommandsTest, IsCon) {
  sexp_t* expr = symbol("my_expr");
  sexp_t* test = is_con("Some", expr);

  ASSERT_NE(test, nullptr);
  ASSERT_STRCASEEQ(sexp_to_string(test), "((_ is Some) my_expr)");

  sexp_free(test);
  sexp_free(expr);
}

TEST_F(SexpCommandsTest, SimpleDatatype) {
  // Test declaring a simple datatype: (declare-datatype Option ((None) (Some (value Int))))

  // Create None constructor (no fields)
  constructor_t none_cons;
  none_cons.name = "None";
  none_cons.fields = nullptr;
  none_cons.field_count = 0;

  // Create Some constructor (one field)
  con_field_t some_field;
  some_field.name = "value";
  some_field.type = t_int();

  constructor_t some_cons;
  some_cons.name = "Some";
  some_cons.fields = &some_field;
  some_cons.field_count = 1;

  constructor_t constructors[] = {none_cons, some_cons};
  sexp_t* datatype_decl = declare_datatype("Option", nullptr, 0, constructors, 2);

  ASSERT_NE(datatype_decl, nullptr);

  // The exact string format may vary, but should contain the key components
  std::string result = sexp_to_string(datatype_decl);
  EXPECT_TRUE(result.find("declare-datatype") != std::string::npos);
  EXPECT_TRUE(result.find("Option") != std::string::npos);
  EXPECT_TRUE(result.find("None") != std::string::npos);
  EXPECT_TRUE(result.find("Some") != std::string::npos);
  EXPECT_TRUE(result.find("value") != std::string::npos);

  sexp_free(datatype_decl);
  sexp_free(some_field.type);
}
