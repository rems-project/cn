#include <sys/select.h>
#include <sys/wait.h>

#include <assert.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <cn-smt/context.h>
#include <cn-smt/datatypes.h>
#include <cn-smt/memory/arena.h>
#include <cn-smt/memory/intern.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>
#include <cn-smt/terms.h>
#include <cn-smt/to_smt.h>
#include <fulminate/api.h>

// SMT logging configuration (NULL = disabled)
const char *cn_smt_log_file_path = NULL;

void cn_smt_set_log_file_path(const char *path) {
  cn_smt_log_file_path = path;
}

void send_string(struct cn_smt_solver *solver, const char *str) {
  assert(solver && str);
  fprintf(solver->write_input, "%s\n", str);
  fflush(solver->write_input);

  if (solver->log_file != NULL) {
    fprintf(solver->log_file, "\nStart Write:\n%s\nEnd Write\n", str);
    fflush(solver->log_file);

    fclose(solver->log_file);
    solver->log_file = fopen(cn_smt_log_file_path, "a");
  }
}

char *read_output(struct cn_smt_solver *solver) {
  assert(solver);

  // Wait for data to be available with a timeout
  int fd = fileno(solver->read_output);
  fd_set read_fds;
  struct timeval timeout;
  int select_result;

  // Retry select() if interrupted by signal (EINTR)
  while (1) {
    FD_ZERO(&read_fds);
    FD_SET(fd, &read_fds);

    // 30 second timeout
    timeout.tv_sec = 30;
    timeout.tv_usec = 0;

    select_result = select(fd + 1, &read_fds, NULL, NULL, &timeout);

    if (select_result == -1) {
      if (errno == EINTR) {
        // Interrupted by signal, retry
        continue;
      }
      perror("select failed in read_output");
      assert(false);
    }

    // Success or timeout, break out of retry loop
    break;
  }

  if (select_result == 0) {
    // fprintf(stderr, "Timeout waiting for SMT solver response\n");
    // assert(false);
    // FIXME: Use an appropriate error
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }

  // Data is available, proceed with reading
  size_t buffer_size = 128;
  char *buffer = cn_test_malloc(buffer_size);
  assert(buffer);

  size_t total_len = 0;
  int c;
  int paren_balance = 0;
  bool has_parens = false;

  while ((c = fgetc(solver->read_output)) != EOF) {
    // Skip leading newlines/whitespace before any content
    if (total_len == 0 && (c == '\n' || c == '\r' || c == ' ' || c == '\t')) {
      continue;
    }

    if (total_len + 1 >= buffer_size) {
      buffer_size *= 2;
      char *new_buffer = cn_test_realloc(buffer, buffer_size);
      assert(new_buffer);
      buffer = new_buffer;
    }

    buffer[total_len++] = c;

    if (c == '(') {
      paren_balance++;
      has_parens = true;
    } else if (c == ')') {
      paren_balance--;
    }

    // Stop conditions:
    // 1. Newline and no parentheses seen (simple response like "success")
    // 2. All parentheses balanced and we've seen at least one
    if (c == '\n' && !has_parens) {
      break;
    }
    if (has_parens && paren_balance == 0) {
      break;
    }
  }

  assert(total_len > 0);

  buffer[total_len] = '\0';

  // Remove trailing newline if present
  if (total_len > 0 && buffer[total_len - 1] == '\n') {
    buffer[total_len - 1] = '\0';
  }

  if (solver->log_file != NULL) {
    fprintf(solver->log_file, "\nStart Read:\n%s\nEnd Read\n", buffer);
    fflush(solver->log_file);
  }

  return buffer;
}

sexp_t *send_command(struct cn_smt_solver *solver, sexp_t *sexp) {
  char *sexp_str = sexp_to_string(sexp);
  send_string(solver, sexp_str);
  // Only free if sexp_to_string allocated a new string (lists) vs returning atom pointer
  if (!sexp_is_atom(sexp)) {
    cn_test_free(sexp_str);
  }

  char *buffer = read_output(solver);
  sexp_t *result = sexp_parse(buffer);
  cn_test_free(buffer);

  return result;
}

void stop_solver(struct cn_smt_solver *solver) {
  assert(solver);
  send_string(solver, "(exit)");

  // Close FILE* streams (this closes the underlying file descriptors)
  fclose(solver->write_input);
  fclose(solver->read_output);
  if (solver->log_file != NULL) {
    fclose(solver->log_file);
  }

  // Wait for solver process to exit
  int status;
  waitpid(solver->pid, &status, 0);
}

void ack_command(struct cn_smt_solver *solver, sexp_t *cmd) {
  assert(solver && cmd);
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

sexp_t *get_unsat_core(struct cn_smt_solver *solver) {
  sexp_t *args[] = {sexp_atom("get-unsat-core")};
  return send_command(solver, sexp_list(args, 1));
}

struct cn_smt_solver *cn_smt_new_solver(solver_extensions_t ext) {
  struct cn_smt_solver *solver = cn_test_malloc(sizeof(struct cn_smt_solver));
  assert(solver);

  int pipe_fd_in[2];
  int pipe_fd_out[2];

  assert(pipe(pipe_fd_in) != -1 && pipe(pipe_fd_out) != -1);

  pid_t pid = fork();
  assert(pid != -1);

  if (pid == 0) {
    dup2(pipe_fd_in[0], STDIN_FILENO);
    close(pipe_fd_in[1]);

    dup2(pipe_fd_out[1], STDOUT_FILENO);
    close(pipe_fd_out[0]);

    switch (ext) {
      case SOLVER_CVC5:
        execlp("cvc5", "cvc5", "--sets-exp", "--force-logic=QF_ALL", NULL);
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

  // Check CVC5 + skewing compatibility
  if (ext == SOLVER_CVC5 && cn_get_smt_skewing_mode() != CN_SMT_SKEWING_NONE) {
    fprintf(stderr,
        "\033[33mWarning: CVC5 does not support skewing; disabling skewing.\033[0m\n");
    cn_set_smt_skewing_mode(CN_SMT_SKEWING_NONE);
  }

  // Close unused pipe ends in parent
  close(pipe_fd_in[0]);
  close(pipe_fd_out[1]);

  solver->write_input = fdopen(pipe_fd_in[1], "w");
  solver->read_output = fdopen(pipe_fd_out[0], "r");

  // Only open log file if logging is enabled
  if (cn_smt_log_file_path != NULL) {
    solver->log_file = fopen(cn_smt_log_file_path, "w");
  } else {
    solver->log_file = NULL;
  }

  ack_command(solver, set_option(":print-success", "true"));
  ack_command(solver, set_option(":produce-models", "true"));

  // Only enable unsat cores if logging is requested
  if (cn_smt_get_unsat_core_log_path() != NULL) {
    ack_command(solver, set_option(":produce-unsat-cores", "true"));
  }

  switch (ext) {
    case SOLVER_CVC5:
      break;
    case SOLVER_Z3:
      ack_command(solver, set_option(":auto_config", "false"));
      ack_command(solver, set_option(":model.completion", "true"));
      ack_command(solver, set_option(":smt.relevancy", "0"));
      ack_command(solver, set_option(":smt.phase_selection", "5"));
  }

  return solver;
}

void cn_smt_solver_reset(struct cn_smt_solver *solver) {
  sexp_t *reset_atom = sexp_atom("reset");
  sexp_t *reset_cmd = sexp_list(&reset_atom, 1);
  ack_command(solver, reset_cmd);
}

//////////////////////////////////////////////////////////////////////////////
// CN_Datatypes and CN_Structs modules (converted from OCaml)
//////////////////////////////////////////////////////////////////////////////

// CN_Datatypes module functions

// Helper function to create datatype field from label and base type
// Converts: (l, t) -> (CN_Names.datatype_field_name l, translate_base_type t)
typedef struct {
  const char *name;
  sexp_t *type;
} dt_con_field_t;

// Helper function to create constructor info
// Converts: c -> (CN_Names.datatype_con_name c, List.map mk_con_field ci.params)
typedef struct {
  const char *name;
  dt_con_field_t *fields;
  size_t field_count;
} dt_constructor_t;

// Declare a group of mutually recursive datatypes
// Takes specific datatype info instead of accessing globals
void cn_datatypes_declare_datatype_group(struct cn_smt_solver *s,
    const char **names,
    size_t name_count,
    dt_info_t *datatype_infos,
    dt_constr_info_t **constr_infos) {
  assert(s && names && name_count > 0 && datatype_infos && constr_infos);

  datatype_def_t *datatypes = cn_test_malloc(sizeof(datatype_def_t) * name_count);
  assert(datatypes);

  cn_bump_frame_id frame = cn_bump_get_frame_id();

  // Build each datatype definition
  for (size_t i = 0; i < name_count; i++) {
    const char *dt_name = names[i];
    dt_info_t *dt_info = &datatype_infos[i];

    // Create datatype name using CN_Names.datatype_name
    char dt_buffer[256];
    snprintf(dt_buffer, sizeof(dt_buffer), "%s_dt", dt_name);

    datatypes[i].name = cn_intern_string(dt_buffer);
    datatypes[i].type_params = NULL;
    datatypes[i].type_param_count = 0;

    // Build constructors for this datatype
    datatypes[i].constructor_count = dt_info->constr_count;
    datatypes[i].constructors =
        cn_test_malloc(sizeof(constructor_t) * dt_info->constr_count);
    assert(datatypes[i].constructors);

    for (size_t c = 0; c < dt_info->constr_count; c++) {
      const char *constr_name = dt_info->constrs[c];
      dt_constr_info_t *ci = constr_infos[i * dt_info->constr_count + c];

      // Create constructor name using CN_Names.datatype_con_name
      char con_buffer[256];
      snprintf(con_buffer, sizeof(con_buffer), "%s_con", constr_name);

      datatypes[i].constructors[c].name = cn_intern_string(con_buffer);
      datatypes[i].constructors[c].field_count = ci->param_count;

      // Build constructor fields for SMT
      if (ci->param_count > 0) {
        datatypes[i].constructors[c].fields =
            cn_test_malloc(sizeof(con_field_t) * ci->param_count);
        assert(datatypes[i].constructors[c].fields);

        for (size_t f = 0; f < ci->param_count; f++) {
          dt_param_t *param = &ci->params[f];

          // Create field name using CN_Names.datatype_field_name
          char field_buffer[256];
          snprintf(field_buffer, sizeof(field_buffer), "%s_data_fld", param->label);

          datatypes[i].constructors[c].fields[f].name = cn_intern_string(field_buffer);
          datatypes[i].constructors[c].fields[f].type =
              translate_base_type(param->base_type);
        }
      } else {
        datatypes[i].constructors[c].fields = NULL;
      }

      // Register constructor with metadata and function pointer
      cn_datatype_field_info *fields = NULL;
      if (ci->param_count > 0) {
        fields = cn_test_malloc(sizeof(cn_datatype_field_info) * ci->param_count);
        assert(fields);
        for (size_t f = 0; f < ci->param_count; f++) {
          fields[f].label = ci->params[f].label;
          fields[f].base_type = ci->params[f].base_type;
        }
      }

      // Register constructor with function pointer from dt_constr_info_t
      cn_register_datatype_constructor(
          dt_name, constr_name, ci->constructor_fn, fields, ci->param_count);

      // Note: fields array is copied by cn_register_datatype_constructor
      if (fields) {
        cn_test_free(fields);
      }
    }
  }

  // Create and send command
  sexp_t *cmd = declare_datatypes(datatypes, name_count);
  if (cmd) {
    ack_command(s, cmd);
  }

  cn_bump_free_after(frame);

  // Cleanup
  // Note: datatype names, constructor names, and field names are interned, don't free them
  for (size_t i = 0; i < name_count; i++) {
    for (size_t c = 0; c < datatypes[i].constructor_count; c++) {
      cn_test_free(datatypes[i].constructors[c].fields);
    }
    cn_test_free(datatypes[i].constructors);
  }
  cn_test_free(datatypes);
}

// Declare datatypes given the datatype order (groups of mutually recursive types)
void cn_datatypes_declare(struct cn_smt_solver *s,
    const char ***datatype_order,
    size_t order_count,
    size_t *group_sizes,
    dt_info_t **all_datatype_infos,
    dt_constr_info_t ***all_constr_infos) {
  assert(s && datatype_order && group_sizes && all_datatype_infos && all_constr_infos);

  // Iterate over each group in the datatype order
  for (size_t g = 0; g < order_count; g++) {
    const char **group_names = datatype_order[g];
    size_t group_size = group_sizes[g];
    dt_info_t *group_infos = all_datatype_infos[g];
    dt_constr_info_t **group_constr_infos = all_constr_infos[g];

    cn_datatypes_declare_datatype_group(
        s, group_names, group_size, group_infos, group_constr_infos);
  }
}

// CN_Structs module functions

// Helper to track declared structs (equivalent to done_struct in OCaml)
typedef struct declared_struct_set {
  const char **names;
  size_t count;
  size_t capacity;
} declared_struct_set_t;

static declared_struct_set_t *create_declared_struct_set(void) {
  declared_struct_set_t *set = cn_test_malloc(sizeof(declared_struct_set_t));
  assert(set);

  set->names = cn_test_malloc(sizeof(char *) * 16);  // Initial capacity
  assert(set->names);

  set->count = 0;
  set->capacity = 16;
  return set;
}

static bool is_struct_declared(declared_struct_set_t *set, const char *name) {
  if (!set || !name)
    return false;

  for (size_t i = 0; i < set->count; i++) {
    if (strcmp(set->names[i], name) == 0) {
      return true;
    }
  }
  return false;
}

static void add_declared_struct(declared_struct_set_t *set, const char *name) {
  assert(set && name);
  if (is_struct_declared(set, name)) {
    return;
  }

  // Resize if needed
  if (set->count >= set->capacity) {
    set->capacity *= 2;
    set->names = cn_test_realloc(set->names, sizeof(char *) * set->capacity);
    assert(set->names);
  }

  set->names[set->count++] = name;
}

static void free_declared_struct_set(declared_struct_set_t *set) {
  if (!set)
    return;
  cn_test_free(set->names);
  cn_test_free(set);
}

// Note: struct_member_t and struct_decl_t are now defined in cn-smt/solver.h

// Recursive function to declare struct and dependencies
void cn_structs_declare_struct(struct cn_smt_solver *s,
    declared_struct_set_t *done_struct,
    const char *name,
    struct_decl_t *struct_decl,
    struct_decl_t **all_struct_decls,
    const char **all_struct_names,
    size_t struct_count) {
  assert(s && done_struct && name && struct_decl);

  // Check if already declared
  if (is_struct_declared(done_struct, name)) {
    return;
  }

  // Mark as declared
  add_declared_struct(done_struct, name);

  // Recursively declare any nested struct dependencies
  for (size_t i = 0; i < struct_decl->member_count; i++) {
    struct_member_t *member = &struct_decl->members[i];

    // Check if this member is a struct type that needs declaring
    if (member->base_type.tag == CN_BASE_STRUCT) {
      // Find the struct declaration and recursively declare it
      for (size_t s_idx = 0; s_idx < struct_count; s_idx++) {
        if (strcmp(all_struct_names[s_idx], member->label) == 0) {
          cn_structs_declare_struct(s,
              done_struct,
              all_struct_names[s_idx],
              all_struct_decls[s_idx],
              all_struct_decls,
              all_struct_names,
              struct_count);
          break;
        }
      }
    }
  }

  cn_bump_frame_id frame = cn_bump_get_frame_id();

  // Create struct constructor name using CN_Names.struct_con_name
  const char *con_name = struct_con_name(name);
  assert(con_name);
  // Build constructor fields
  con_field_t *fields = NULL;
  size_t field_count = 0;

  if (struct_decl->member_count > 0) {
    fields = cn_test_malloc(sizeof(con_field_t) * struct_decl->member_count);
    assert(fields);
    field_count = struct_decl->member_count;

    for (size_t i = 0; i < struct_decl->member_count; i++) {
      struct_member_t *member = &struct_decl->members[i];

      // Create field name using CN_Names.struct_field_name
      char field_buffer[256];
      snprintf(field_buffer, sizeof(field_buffer), "%s_struct_fld", member->label);

      fields[i].name = cn_intern_string(field_buffer);
      fields[i].type = translate_base_type(member->base_type);
    }
  }

  // Create struct name using CN_Names.struct_name
  const char *struct_name = cn_smt_struct_name(name);

  // Create constructor
  constructor_t constructor;
  constructor.name = con_name;
  constructor.fields = fields;
  constructor.field_count = field_count;

  // Declare the struct datatype
  const char *type_params[] = {};
  sexp_t *struct_decl_sexp =
      declare_datatype(struct_name, type_params, 0, &constructor, 1);
  if (struct_decl_sexp) {
    ack_command(s, struct_decl_sexp);
  }

  cn_bump_free_after(frame);

  // Cleanup
  // con_name, struct_name, and field names are interned, no need to free
  if (fields) {
    cn_test_free(fields);
  }
}

// Declare all structs with their dependencies
void cn_structs_declare(struct cn_smt_solver *s,
    const char **struct_names,
    struct_decl_t **struct_decls,
    size_t struct_count) {
  assert(s && struct_names && struct_decls && struct_count > 0);

  declared_struct_set_t *done_structs = create_declared_struct_set();
  assert(done_structs);

  // Declare each struct (dependencies will be declared recursively)
  for (size_t i = 0; i < struct_count; i++) {
    cn_structs_declare_struct(s,
        done_structs,
        struct_names[i],
        struct_decls[i],
        struct_decls,
        struct_names,
        struct_count);
  }

  free_declared_struct_set(done_structs);
}

/** Declare a datatype for a tuple */
void cn_tuple_declare(struct cn_smt_solver *solver) {
  for (int arity = 1; arity <= CN_TUPLE_MAX_ARITY; arity++) {
    const char *name = cn_tuple_constructor_name(arity);

    cn_bump_frame_id frame = cn_bump_get_frame_id();

    // Create type parameter names: a0, a1, a2, ...
    const char **type_params = NULL;
    type_params = cn_test_malloc(arity * sizeof(char *));
    assert(type_params);
    for (int i = 0; i < arity; i++) {
      char buffer[32];
      snprintf(buffer, sizeof(buffer), "a%d", i);
      type_params[i] = cn_intern_string(buffer);
    }

    // Create constructor fields
    con_field_t *fields = NULL;
    if (arity > 0) {
      fields = cn_test_malloc(arity * sizeof(con_field_t));
      assert(fields);
      for (int i = 0; i < arity; i++) {
        fields[i].name = cn_tuple_get_selector_name(arity, i);
        fields[i].type = sexp_atom(type_params[i]);
      }
    }

    // Create constructor
    constructor_t constructor;
    constructor.name = name;
    constructor.fields = fields;
    constructor.field_count = arity;

    // Declare the datatype
    sexp_t *datatype_decl = declare_datatype(name, type_params, arity, &constructor, 1);
    ack_command(solver, datatype_decl);

    cn_bump_free_after(frame);

    // Clean up
    if (arity > 0) {
      // Type params and field names are interned, only free the arrays
      cn_test_free(type_params);
      cn_test_free(fields);
    }
    // name is interned, no need to free
  }
}

/** Declare the option datatype */
void cn_option_declare(struct cn_smt_solver *solver) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();

  // Type parameter
  const char *type_params[] = {"a"};

  // None constructor (no fields)
  constructor_t none_constructor;
  none_constructor.name = cn_option_none_name;
  none_constructor.fields = NULL;
  none_constructor.field_count = 0;

  // Some constructor with value field
  con_field_t some_field;
  some_field.name = cn_option_val_name;
  some_field.type = sexp_atom("a");

  constructor_t some_constructor;
  some_constructor.name = cn_option_some_name;
  some_constructor.fields = &some_field;
  some_constructor.field_count = 1;

  // Array of constructors
  constructor_t constructors[] = {none_constructor, some_constructor};

  // Declare the datatype
  sexp_t *datatype_decl =
      declare_datatype(cn_option_name, type_params, 1, constructors, 2);
  ack_command(solver, datatype_decl);

  cn_bump_free_after(frame);
}
