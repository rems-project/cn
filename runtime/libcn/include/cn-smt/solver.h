#ifndef CN_SMT_SOLVER_H
#define CN_SMT_SOLVER_H

#include <sys/wait.h>

#include <stdio.h>

#include <cn-smt/sexp.h>
#include <cn-smt/terms.h>

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

// Struct declaration types
typedef struct {
  const char *label;       // Member label/name
  cn_base_type base_type;  // Member base type
} struct_member_t;

typedef struct {
  struct_member_t *members;
  size_t member_count;
} struct_decl_t;

// Datatype declaration types
typedef struct {
  const char *label;       // Field label/name
  cn_base_type base_type;  // Field base type
} dt_param_t;

typedef struct {
  dt_param_t *params;
  size_t param_count;
} dt_constr_info_t;

typedef struct {
  const char **constrs;  // Constructor symbol names
  size_t constr_count;
} dt_info_t;

// Function declarations
void send_string(struct cn_smt_solver *solver, const char *str);
sexp_t *send_command(struct cn_smt_solver *solver, sexp_t *sexp);
void stop_solver(struct cn_smt_solver *solver);
void ack_command(struct cn_smt_solver *solver, sexp_t *cmd);
enum cn_smt_solver_result check(struct cn_smt_solver *solver);
struct cn_smt_solver *cn_smt_new_solver(solver_extensions_t ext);
void cn_structs_declare(struct cn_smt_solver *s,
    const char **struct_names,
    struct_decl_t **struct_decls,
    size_t struct_count);
void cn_datatypes_declare(struct cn_smt_solver *s,
    const char ***datatype_order,
    size_t order_count,
    size_t *group_sizes,
    dt_info_t **all_datatype_infos,
    dt_constr_info_t ***all_constr_infos);
void cn_smt_solver_reset(struct cn_smt_solver *solver);
void cn_tuple_declare(struct cn_smt_solver *solver);
void cn_option_declare(struct cn_smt_solver *solver);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_SOLVER_H
