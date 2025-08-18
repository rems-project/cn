#include <gtest/gtest.h>

#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>

class SolverTest : public ::testing::Test {
 protected:
  void SetUp() override {
    // Skip if Z3 is not available
    if (system("which z3 > /dev/null 2>&1") != 0) {
      GTEST_SKIP() << "Z3 not found, skipping solver tests";
    }
  }
};

TEST_F(SolverTest, NewSolverZ3DoesNotCrash) {
  struct cn_smt_solver *solver = cn_smt_new_solver(SOLVER_Z3);
  ASSERT_NE(solver, nullptr);

  stop_solver(solver);
  free(solver);
}

TEST_F(SolverTest, NewSolverCVC5DoesNotCrash) {
  if (system("which cvc5 > /dev/null 2>&1") != 0) {
    GTEST_SKIP() << "CVC5 not found, skipping CVC5 solver test";
  }

  struct cn_smt_solver *solver = cn_smt_new_solver(SOLVER_CVC5);
  ASSERT_NE(solver, nullptr);

  stop_solver(solver);
  free(solver);
}

TEST_F(SolverTest, BasicSolverOperations1) {
  struct cn_smt_solver *solver = cn_smt_new_solver(SOLVER_Z3);
  ASSERT_NE(solver, nullptr);

  sexp_t *true_expr = sexp_atom("true");
  sexp_t *assert_cmd = sexp_app_str("assert", &true_expr, 1);

  ack_command(solver, assert_cmd);

  enum cn_smt_solver_result result = check(solver);
  EXPECT_EQ(result, CN_SOLVER_SAT);

  sexp_free(assert_cmd);
  stop_solver(solver);
  free(solver);
}

TEST_F(SolverTest, BasicSolverOperations2) {
  struct cn_smt_solver *solver = cn_smt_new_solver(SOLVER_Z3);
  ASSERT_NE(solver, nullptr);

  sexp_t *false_expr = sexp_atom("false");
  sexp_t *assert_cmd = sexp_app_str("assert", &false_expr, 1);

  ack_command(solver, assert_cmd);

  enum cn_smt_solver_result result = check(solver);
  EXPECT_EQ(result, CN_SOLVER_UNSAT);

  sexp_free(assert_cmd);
  stop_solver(solver);
  free(solver);
}
