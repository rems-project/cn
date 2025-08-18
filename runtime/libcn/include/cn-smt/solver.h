#ifndef CN_SMT_SOLVER_H
#define CN_SMT_SOLVER_H

#include <sys/wait.h>

#include <stdio.h>

#include <cn-smt/sexp.h>

#ifdef __cplusplus
extern "C" {
#endif

struct cn_smt_solver {
  solver_extensions_t ext;
  pid_t pid;
  FILE *write_input;
  FILE *read_output;
  FILE *log_file;
};

enum cn_smt_solver_result {
  CN_SOLVER_SAT,
  CN_SOLVER_UNSAT,
  CN_SOLVER_UNKNOWN
};

// Forward declare sexp_t
typedef struct sexp sexp_t;

// Function declarations
void send_string(struct cn_smt_solver *solver, const char *str);
sexp_t *send_command(struct cn_smt_solver *solver, sexp_t *sexp);
void stop_solver(struct cn_smt_solver *solver);
void ack_command(struct cn_smt_solver *solver, sexp_t *cmd);
enum cn_smt_solver_result check(struct cn_smt_solver *solver);
struct cn_smt_solver *cn_smt_new_solver(solver_extensions_t ext);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_SOLVER_H
