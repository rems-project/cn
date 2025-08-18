#ifndef CN_SMT_SEXP_H
#define CN_SMT_SEXP_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  SEXP_ATOM,
  SEXP_LIST
} sexp_type_t;

typedef struct sexp {
  sexp_type_t type;
  union {
    char *atom;
    struct {
      struct sexp **elements;
      size_t count;
      size_t capacity;
    } list;
  } data;
} sexp_t;

char *sexp_to_string(sexp_t *sexp);

// Constructor functions
sexp_t *sexp_atom(const char *str);
sexp_t *sexp_list(sexp_t **elements, size_t count);

// Query functions
bool sexp_is_atom(const sexp_t *sexp);
sexp_t **sexp_to_list(const sexp_t *sexp, size_t *count);

// Pattern matching functions
sexp_t *sexp_to_assert(const sexp_t *sexp);

// Memory management
void sexp_free(sexp_t *sexp);

// Parsing
sexp_t *sexp_parse(const char *input);

// SMT-LIB construction helpers
sexp_t *sexp_app(sexp_t *f, sexp_t **args, size_t arg_count);
sexp_t *sexp_app_str(const char *f, sexp_t **args, size_t arg_count);
sexp_t *sexp_as_type(sexp_t *x, sexp_t *t);
sexp_t *sexp_let(sexp_t **bindings, size_t binding_count, sexp_t *e);
sexp_t *sexp_nat_k(int x);
sexp_t *sexp_fam(const char *f, sexp_t **indices, size_t index_count);
sexp_t *sexp_ifam(const char *f, int *indices, size_t index_count);
sexp_t *sexp_named(const char *name, sexp_t *e);

// Symbol handling functions
bool allowed_simple_char(char c);
bool is_simple_symbol(const char *s);
char *quote_symbol(const char *s);
sexp_t *symbol(const char *x);

// Boolean and arithmetic functions
sexp_t *t_bool(void);
sexp_t *bool_k(bool b);
sexp_t *ite(sexp_t *x, sexp_t *y, sexp_t *z);
sexp_t *eq(sexp_t *x, sexp_t *y);
sexp_t *distinct(sexp_t **xs, size_t count);
sexp_t *bool_not(sexp_t *p);
sexp_t *bool_and(sexp_t *p, sexp_t *q);
sexp_t *bool_ands(sexp_t **ps, size_t count);
sexp_t *bool_or(sexp_t *p, sexp_t *q);
sexp_t *bool_ors(sexp_t **ps, size_t count);
sexp_t *bool_xor(sexp_t *p, sexp_t *q);
sexp_t *bool_implies(sexp_t *p, sexp_t *q);

// Numeric functions
sexp_t *t_int(void);
sexp_t *t_real(void);
sexp_t *num_neg(sexp_t *x);
sexp_t *int_k(int x);
sexp_t *real_div(sexp_t *x, sexp_t *y);
sexp_t *num_gt(sexp_t *x, sexp_t *y);
sexp_t *num_lt(sexp_t *x, sexp_t *y);
sexp_t *num_geq(sexp_t *x, sexp_t *y);
sexp_t *num_leq(sexp_t *x, sexp_t *y);
sexp_t *num_add(sexp_t *x, sexp_t *y);
sexp_t *num_sub(sexp_t *x, sexp_t *y);
sexp_t *num_mul(sexp_t *x, sexp_t *y);
sexp_t *num_abs(sexp_t *x);
sexp_t *num_div(sexp_t *x, sexp_t *y);
sexp_t *num_mod(sexp_t *x, sexp_t *y);
sexp_t *num_rem(sexp_t *x, sexp_t *y);
sexp_t *num_divisible(sexp_t *x, int n);
sexp_t *real_to_int(sexp_t *e);
sexp_t *int_to_real(sexp_t *e);

// Bit Vector functions
sexp_t *t_bits(int w);
sexp_t *bv_nat_bin(int w, long long v);
sexp_t *bv_nat_hex(int w, long long v);
sexp_t *bv_neg(sexp_t *x);
sexp_t *bv_compl(sexp_t *x);
sexp_t *bv_bin(int w, long long v);
sexp_t *bv_hex(int w, long long v);
sexp_t *bv_k(int w, long long v);
sexp_t *bv_ult(sexp_t *x, sexp_t *y);
sexp_t *bv_uleq(sexp_t *x, sexp_t *y);
sexp_t *bv_slt(sexp_t *x, sexp_t *y);
sexp_t *bv_sleq(sexp_t *x, sexp_t *y);
sexp_t *bv_concat(sexp_t *x, sexp_t *y);
sexp_t *bv_sign_extend(int i, sexp_t *x);
sexp_t *bv_zero_extend(int i, sexp_t *x);
sexp_t *bv_extract(int last_ix, int first_ix, sexp_t *x);
sexp_t *bv_not(sexp_t *x);
sexp_t *bv_and(sexp_t *x, sexp_t *y);
sexp_t *bv_or(sexp_t *x, sexp_t *y);
sexp_t *bv_xor(sexp_t *x, sexp_t *y);
sexp_t *bv_add(sexp_t *x, sexp_t *y);
sexp_t *bv_sub(sexp_t *x, sexp_t *y);
sexp_t *bv_mul(sexp_t *x, sexp_t *y);
sexp_t *bv_udiv(sexp_t *x, sexp_t *y);
sexp_t *bv_urem(sexp_t *x, sexp_t *y);
sexp_t *bv_sdiv(sexp_t *x, sexp_t *y);
sexp_t *bv_srem(sexp_t *x, sexp_t *y);
sexp_t *bv_smod(sexp_t *x, sexp_t *y);
sexp_t *bv_shl(sexp_t *x, sexp_t *y);
sexp_t *bv_lshr(sexp_t *x, sexp_t *y);
sexp_t *bv_ashr(sexp_t *x, sexp_t *y);

sexp_t *loc_k(uintptr_t ptr);

// Array functions
sexp_t *t_array(sexp_t *kt, sexp_t *vt);
sexp_t *arr_const(sexp_t *kt, sexp_t *vt, sexp_t *v);
sexp_t *arr_select(sexp_t *arr, sexp_t *i);
sexp_t *arr_store(sexp_t *arr, sexp_t *i, sexp_t *v);

// Solver extensions for non-standard theories
typedef enum {
  SOLVER_Z3,
  SOLVER_CVC5
} solver_extensions_t;

// Set functions
sexp_t *t_set(sexp_t *x);
sexp_t *set_empty(solver_extensions_t ext, sexp_t *t);
sexp_t *set_universe(solver_extensions_t ext, sexp_t *t);
sexp_t *set_insert(solver_extensions_t ext, sexp_t *x, sexp_t *xs);
sexp_t *set_union(solver_extensions_t ext, sexp_t *x, sexp_t *y);
sexp_t *set_intersection(solver_extensions_t ext, sexp_t *x, sexp_t *y);
sexp_t *set_difference(solver_extensions_t ext, sexp_t *x, sexp_t *y);
sexp_t *set_complement(solver_extensions_t ext, sexp_t *x);
sexp_t *set_member(solver_extensions_t ext, sexp_t *x, sexp_t *xs);
sexp_t *set_subset(solver_extensions_t ext, sexp_t *xs, sexp_t *ys);

// Quantifier functions
sexp_t *forall(sexp_t **bindings, size_t binding_count, sexp_t *p);

// Command functions
sexp_t *simple_command(const char **strs, size_t count);
sexp_t *set_option(const char *opt, const char *val);
sexp_t *set_logic(const char *logic);
sexp_t *push(int n);
sexp_t *pop(int n);
sexp_t *declare_sort(const char *name, int arity);
sexp_t *declare_fun(
    const char *name, sexp_t **param_types, size_t param_count, sexp_t *result_type);
sexp_t *declare_const(const char *name, sexp_t *type);
sexp_t *define_fun(const char *name,
    sexp_t **params,
    size_t param_count,
    sexp_t *result_type,
    sexp_t *definition);
sexp_t *define_const(const char *name, sexp_t *type, sexp_t *definition);

// Constructor field and datatype types
typedef struct {
  const char *name;
  sexp_t *type;
} con_field_t;

typedef struct {
  const char *name;
  con_field_t *fields;
  size_t field_count;
} constructor_t;

sexp_t *declare_datatype(const char *name,
    const char **type_params,
    size_t type_param_count,
    constructor_t *constructors,
    size_t constructor_count);

// Pattern matching types and functions
typedef enum {
  PAT_VAR,
  PAT_CON
} pattern_type_t;

typedef struct {
  pattern_type_t type;
  union {
    const char *var_name;
    struct {
      const char *con_name;
      const char **var_names;
      size_t var_count;
    } con;
  } data;
} pattern_t;

typedef struct {
  pattern_t pattern;
  sexp_t *expr;
} match_alt_t;

sexp_t *match_datatype(sexp_t *expr, match_alt_t *alts, size_t alt_count);
sexp_t *is_con(const char *constructor, sexp_t *expr);
sexp_t *assume(sexp_t *expr);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_SEXP_H
