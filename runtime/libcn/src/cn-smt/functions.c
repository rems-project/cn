#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <cn-smt/functions.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>
#include <cn-smt/structs.h>
#include <cn-smt/terms.h>
#include <cn-smt/to_smt.h>

// Global registry of function handlers (hash-based, mapping function name to handler)
static bennet_hash_table(const_char_ptr, void_ptr) g_func_registry;
static bool g_func_registry_initialized = false;

// Initialize the global function registry
static void init_func_registry(void) {
  if (!g_func_registry_initialized) {
    bennet_hash_table_init(const_char_ptr, void_ptr)(
        &g_func_registry, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);
    g_func_registry_initialized = true;
  }
}

// Helper function from to_smt.c - gets function name with _func suffix
extern char* fn_def_name(cn_sym sym);
extern char* fn_name(cn_sym sym);
extern sexp_t* translate_cn_base_type(cn_base_type bt);
extern sexp_t* translate_term(struct cn_smt_solver* s, cn_term* iterm);
extern sexp_t* define_fun(const char* name,
    sexp_t** args,
    size_t arg_count,
    sexp_t* ret_t,
    bool recursive,
    sexp_t* body);
extern void ack_command(struct cn_smt_solver* s, sexp_t* cmd);

void cn_register_func(cn_sym name,
    cn_func_handler handler,
    cn_arg_binder* arg_binders,
    size_t arg_count,
    cn_base_type res_bt,
    bool recursive,
    cn_term* body,
    struct cn_smt_solver* s) {
  assert(handler);
  assert(s);

  // Check for uninterpreted functions
  if (body == NULL) {
    fprintf(stderr,
        "\nUninterpreted CN functions not supported at runtime. Please provide a "
        "concrete "
        "function definition for '%s'\n",
        name.name);
    exit(2);
  }

  init_func_registry();

  // Get function name for registry (use _func suffix)
  char* func_name = fn_def_name(name);
  assert(func_name);

  // Store handler in registry (duplicate the name for stable storage)
  char* stored_name = strdup(func_name);
  assert(stored_name);
  bennet_hash_table_set(const_char_ptr, void_ptr)(
      &g_func_registry, stored_name, (void_ptr)handler);

  // Now define the function in SMT solver
  // This is the logic from cn_define_fun in to_smt.c

  // Create parameter list for SMT define_fun
  sexp_t** args = NULL;
  if (arg_count > 0) {
    args = cn_test_malloc(sizeof(sexp_t*) * arg_count);
    assert(args);
  }

  for (size_t i = 0; i < arg_count; i++) {
    // mk_arg (sym, bt) = (CN_Names.fn_name sym, translate_base_type bt)
    char* arg_name = fn_name(arg_binders[i].sym);
    assert(arg_name);

    sexp_t* arg_type = translate_base_type(arg_binders[i].bt);
    assert(arg_type);

    // Create parameter as (name type)
    sexp_t* name_atom = sexp_atom(arg_name);
    assert(name_atom);

    cn_test_free(arg_name);

    sexp_t* param_elements[] = {name_atom, arg_type};
    args[i] = sexp_list(param_elements, 2);
    assert(args[i]);
  }

  // Translate result type
  sexp_t* ret_t = translate_base_type(res_bt);
  assert(ret_t);

  // Translate body
  sexp_t* body_smt = translate_term(s, body);
  assert(body_smt);

  // Create SMT define_fun command
  sexp_t* def_cmd = define_fun(func_name, args, arg_count, ret_t, recursive, body_smt);
  assert(def_cmd);

  // Send command
  ack_command(s, def_cmd);

  // Cleanup
  for (size_t i = 0; i < arg_count; i++) {
  }
  cn_test_free(args);
}

cn_func_handler cn_get_func_handler(const char* func_name) {
  assert(func_name);
  init_func_registry();

  bennet_optional(void_ptr) opt =
      bennet_hash_table_get(const_char_ptr, void_ptr)(&g_func_registry, func_name);

  if (bennet_optional_is_none(opt)) {
    fprintf(stderr, "\nFunction not registered: %s\n", func_name);
    assert(bennet_optional_is_some(opt));
  }

  return (cn_func_handler)bennet_optional_unwrap(opt);
}

bool cn_func_exists(const char* func_name) {
  assert(func_name);
  init_func_registry();
  return bennet_hash_table_contains(const_char_ptr, void_ptr)(
      &g_func_registry, func_name);
}

// Reset the function registry
void cn_smt_func_registry_reset(void) {
  g_func_registry_initialized = false;
}
