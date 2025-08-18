#include <sys/wait.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>

void send_string(struct cn_smt_solver *solver, const char *str) {
  fprintf(solver->write_input, "%s\n", str);
  fflush(solver->write_input);

  fprintf(solver->log_file, "\nStart Write:\n%s\nEnd Write\n", str);
  fflush(solver->log_file);
}

char *read_output(struct cn_smt_solver *solver) {
  size_t buffer_size = 128;
  char *buffer = malloc(buffer_size);
  if (!buffer) {
    return NULL;
  }

  size_t total_len = 0;
  int c;

  while ((c = fgetc(solver->read_output)) != EOF) {
    if (total_len + 1 >= buffer_size) {
      buffer_size *= 2;
      char *new_buffer = realloc(buffer, buffer_size);
      if (!new_buffer) {
        free(buffer);
        return NULL;
      }
      buffer = new_buffer;
    }

    buffer[total_len++] = c;

    if (c == '\n') {
      break;
    }
  }

  if (total_len == 0) {
    free(buffer);
    return NULL;
  }

  buffer[total_len] = '\0';

  // Remove trailing newline if present
  if (total_len > 0 && buffer[total_len - 1] == '\n') {
    buffer[total_len - 1] = '\0';
  }

  fprintf(solver->log_file, "\nStart Read:\n%s\nEnd Read\n", buffer);
  fflush(solver->log_file);

  return buffer;
}

sexp_t *send_command(struct cn_smt_solver *solver, sexp_t *sexp) {
  send_string(solver, sexp_to_string(sexp));

  char *buffer = read_output(solver);
  sexp_t *result = sexp_parse(buffer);
  free(buffer);

  return result;
}

void stop_solver(struct cn_smt_solver *solver) {
  send_string(solver, "(exit)");
}

void ack_command(struct cn_smt_solver *solver, sexp_t *cmd) {
  sexp_t *res = send_command(solver, cmd);

  if (!sexp_is_atom(res)) {
    fprintf(stderr, "%s", sexp_to_string(res));
    assert(false);
  }
  if (strcmp(res->data.atom, "success") != 0) {
    fprintf(stderr, "Expected: 'success', got: '%s'", res->data.atom);
    exit(1);
  }
}

enum cn_smt_solver_result check(struct cn_smt_solver *solver) {
  sexp_t *args[] = {sexp_atom("check-sat")};
  sexp_t *res = send_command(solver, sexp_list(args, 1));
  assert(sexp_is_atom(res));

  if (strcmp(res->data.atom, "sat") == 0) {
    return CN_SOLVER_SAT;
  }

  if (strcmp(res->data.atom, "unsat") == 0) {
    return CN_SOLVER_UNSAT;
  }

  if (strcmp(res->data.atom, "unknown") == 0) {
    return CN_SOLVER_UNKNOWN;
  }

  assert(false);
  return 0;
}

struct cn_smt_solver *cn_smt_new_solver(solver_extensions_t ext) {
  struct cn_smt_solver *solver = malloc(sizeof(struct cn_smt_solver));
  assert(solver);

  int pipe_fd_in[2];
  int pipe_fd_out[2];

  if (pipe(pipe_fd_in) == -1 || pipe(pipe_fd_out) == -1) {
    free(solver);
    return NULL;
  }

  pid_t pid = fork();
  assert(pid != -1);

  if (pid == 0) {
    dup2(pipe_fd_in[0], STDIN_FILENO);
    close(pipe_fd_in[1]);

    dup2(pipe_fd_out[1], STDOUT_FILENO);
    close(pipe_fd_out[0]);

    switch (ext) {
      case SOLVER_CVC5:
        execlp("cvc5", "cvc5", "--sets-ext", "--force-logic=QF_ALL", NULL);
        perror("execlp cvc5 failed");
        exit(1);
      case SOLVER_Z3:
        execlp("z3", "z3", "-in", "-smt2", NULL);
        perror("execlp z3 failed");
        exit(1);
      default:
        assert(false);
    }

    exit(1);
  }

  solver->ext = ext;
  solver->pid = pid;
  solver->write_input = fdopen(pipe_fd_in[1], "w");
  solver->read_output = fdopen(pipe_fd_out[0], "r");
  solver->log_file = fopen("./smt.log", "w");

  ack_command(solver, set_option(":print-success", "true"));
  ack_command(solver, set_option(":produce-models", "true"));

  switch (ext) {
    case SOLVER_CVC5:
      break;
    case SOLVER_Z3:
      ack_command(solver, set_option(":auto_config", "false"));
      ack_command(solver, set_option(":model.completion", "true"));
      ack_command(solver, set_option(":smt.relevancy", "0"));
  }

  return solver;
}
