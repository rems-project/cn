#ifndef CN_SMT_TO_SMT_H
#define CN_SMT_TO_SMT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>
#include <cn-smt/terms.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations (types already defined in included headers)
// cn_term, cn_base_type, cn_const are defined in terms.h
// cn_smt_solver is defined in solver.h

// Helper structure for function argument binders
typedef struct {
  cn_sym sym;
  cn_base_type bt;
} cn_arg_binder;

// ================================
// CN Names module functionality
// ================================

// Symbol and identifier helper functions
const char* sym_pp_string_no_nums(cn_sym sym);
uint64_t sym_num(cn_sym sym);
const char* id_get_string(int id);

// Name generation functions
char* fn_name(cn_sym x);
const char* named_expr_name(void);
char* struct_name(cn_sym x);
char* struct_con_name(cn_sym x);
char* struct_field_name(int x);
char* datatype_name(cn_sym x);
char* datatype_con_name(cn_sym x);
char* datatype_field_name(int x);
char* fresh_name(const char* x);

// ================================
// CN Tuple module functionality
// ================================

// Tuple type and constructor functions
sexp_t* cn_tuple_type_name(sexp_t** types, size_t type_count);
char* cn_tuple_constructor_name(int arity);
char* cn_tuple_get_selector_name(int arity, int field);
void cn_tuple_declare(struct cn_smt_solver* solver);

// ================================
// CN Option module functionality
// ================================

// Option type functions
sexp_t* cn_option_type(sexp_t* a);
void cn_option_declare(struct cn_smt_solver* solver);
sexp_t* cn_option_none(sexp_t* elT);
sexp_t* cn_option_some(sexp_t* x);
sexp_t* cn_option_is_some(sexp_t* x);
sexp_t* cn_option_val(sexp_t* x);

// ================================
// CN List module functionality
// ================================

sexp_t* cn_list_type(sexp_t* element_type);

// ================================
// Type translation functions
// ================================

// Translate CN base types to SMT
sexp_t* translate_base_type(cn_base_type bt);

// ================================
// Term translation functions
// ================================

// Translate CN constants to SMT
sexp_t* translate_const(void* s, cn_const* co);

// Bit-vector casting operations
sexp_t* bv_cast(cn_base_type to_type, cn_base_type from_type, sexp_t* x);

// Bit-vector count operations
sexp_t* bv_clz(int result_w, int w, sexp_t* e);
sexp_t* bv_ctz(int result_w, int w, sexp_t* e);

// Main term translation function
sexp_t* translate_term(struct cn_smt_solver* s, cn_term* iterm);

// ================================
// Function declaration and definition
// ================================

// Declare an uninterpreted function in SMT
void cn_declare_fun(struct cn_smt_solver* s,
    cn_sym name,
    cn_base_type* args_bts,
    size_t args_count,
    cn_base_type res_bt);

// Define a function in SMT
void cn_define_fun(struct cn_smt_solver* s,
    cn_sym name,
    cn_arg_binder* arg_binders,
    size_t arg_count,
    cn_base_type res_bt,
    cn_term* body);

// Declare a variable (zero-argument function) in SMT
void cn_declare_variable(struct cn_smt_solver* solver, cn_sym sym, cn_base_type bt);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_TO_SMT_H
