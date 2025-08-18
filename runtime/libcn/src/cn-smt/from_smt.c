#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/from_smt.h>

// Hash and equality functions are now imported from cn-smt/terms.h

// Generate hash table implementation for string -> sexp mapping
BENNET_HASH_TABLE_IMPL(const_char_ptr, sexp_ptr)

// Helper function to report unexpected solver responses
static void unexpected_solver_response(sexp_t* response, const char* context) {
  fprintf(stderr,
      "Unexpected solver response in %s: %s\n",
      context,
      sexp_to_string(response));
  assert(false);
}

/** {2 Decoding Results} */

/** Get all definitions currently in scope. Only valid after a [Sat] result.
    See also {!model_eval}. */
sexp_t* get_model(struct cn_smt_solver* s) {
  sexp_t* get_model_atom = sexp_atom("get-model");
  sexp_t* get_model_cmd = sexp_list(&get_model_atom, 1);
  sexp_t* ans = send_command(s, get_model_cmd);
  sexp_free(get_model_atom);
  sexp_free(get_model_cmd);
  return ans;
}

/** Get the values of some s-expressions. Only valid after a [Sat] result.
    Throws {!UnexpectedSolverResponse}. */
sexp_t** get_exprs(
    struct cn_smt_solver* s, sexp_t** vals, size_t val_count, size_t* result_count) {
  // Create (get-value (list vals))
  sexp_t* vals_list = sexp_list(vals, val_count);
  sexp_t* get_value_atom = sexp_atom("get-value");
  sexp_t* cmd_elements[] = {get_value_atom, vals_list};
  sexp_t* get_value_cmd = sexp_list(cmd_elements, 2);

  sexp_t* res = send_command(s, get_value_cmd);
  sexp_free(get_value_cmd);
  sexp_free(get_value_atom);
  sexp_free(vals_list);

  // Extract results - expect List of pairs
  size_t pair_count;
  sexp_t** pairs = sexp_to_list(res, &pair_count);
  if (!pairs) {
    unexpected_solver_response(res, "get_exprs - expected list");
  }

  sexp_t** results = malloc(sizeof(sexp_t*) * pair_count);
  if (!results) {
    fprintf(stderr, "Memory allocation failed in get_exprs\n");
    assert(false);
  }

  for (size_t i = 0; i < pair_count; i++) {
    size_t pair_elements_count;
    sexp_t** pair_elements = sexp_to_list(pairs[i], &pair_elements_count);
    if (!pair_elements || pair_elements_count != 2) {
      unexpected_solver_response(pairs[i], "get_exprs - expected pair");
    }
    // Take the second element (value)
    results[i] = pair_elements[1];
  }

  *result_count = pair_count;
  return results;
}

/** Evaluate the given expression in the current context.
    Only valid after a [Sat] result.
    Throws {!UnexpectedSolverResponse}. */
sexp_t* get_expr(struct cn_smt_solver* s, sexp_t* v) {
  // Create (get-value (v))
  sexp_t* vals_list = sexp_list(&v, 1);
  sexp_t* get_value_atom = sexp_atom("get-value");
  sexp_t* cmd_elements[] = {get_value_atom, vals_list};
  sexp_t* get_value_cmd = sexp_list(cmd_elements, 2);

  sexp_t* res = send_command(s, get_value_cmd);
  sexp_free(get_value_cmd);
  sexp_free(get_value_atom);
  sexp_free(vals_list);

  // Expect List [ List [ _; x ] ]
  size_t outer_count;
  sexp_t** outer_list = sexp_to_list(res, &outer_count);
  if (!outer_list || outer_count != 1) {
    unexpected_solver_response(res, "get_expr - expected single element list");
  }

  size_t pair_count;
  sexp_t** pair_elements = sexp_to_list(outer_list[0], &pair_count);
  if (!pair_elements || pair_count != 2) {
    unexpected_solver_response(outer_list[0], "get_expr - expected pair");
  }

  return pair_elements[1];
}

/** Check if expression is a let-binding and extract bindings and body */
bennet_optional(let_binding_t) is_let(sexp_t* exp) {
  if (!exp || sexp_is_atom(exp)) {
    return bennet_optional_none(let_binding_t);
  }

  size_t list_count;
  sexp_t** list_elements = sexp_to_list(exp, &list_count);
  if (!list_elements || list_count != 3) {
    return bennet_optional_none(let_binding_t);
  }

  // Check if first element is "let"
  if (!sexp_is_atom(list_elements[0]) ||
      strcmp(sexp_to_string(list_elements[0]), "let") != 0) {
    return bennet_optional_none(let_binding_t);
  }

  // Extract bindings
  size_t bind_count;
  sexp_t** binds = sexp_to_list(list_elements[1], &bind_count);
  if (!binds) {
    unexpected_solver_response(list_elements[1], "is_let - expected bindings list");
  }

  let_binding_t result;
  bennet_hash_table_init(const_char_ptr, sexp_ptr)(
      &result.bindings, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

  // Process each binding
  for (size_t i = 0; i < bind_count; i++) {
    size_t bind_pair_count;
    sexp_t** bind_pair = sexp_to_list(binds[i], &bind_pair_count);
    if (!bind_pair || bind_pair_count != 2 || !sexp_is_atom(bind_pair[0])) {
      unexpected_solver_response(binds[i], "is_let - expected binding pair");
    }

    const char* var_name = sexp_to_string(bind_pair[0]);
    bennet_hash_table_set(const_char_ptr, sexp_ptr)(
        &result.bindings, var_name, bind_pair[1]);
  }

  result.body = list_elements[2];
  return bennet_optional_some(let_binding_t, result);
}

// Forward declaration for recursive call
static sexp_t* expand_with_substitution(
    bennet_hash_table(const_char_ptr, sexp_ptr) * su, sexp_t* exp);

/** Expand let-definitions in this term.
    NOTE: this is intended to be used mostly on models generated by the
    solver (e.g., `get-value` in Z3 sometimes contains `let`).  As such
    we assume that `forall` and `exist` won't occur, and so we don't need
    to check for variable capture. */
sexp_t* no_let(sexp_t* exp0) {
  bennet_hash_table(const_char_ptr, sexp_ptr) empty_su;
  bennet_hash_table_init(const_char_ptr, sexp_ptr)(
      &empty_su, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

  sexp_t* result = expand_with_substitution(&empty_su, exp0);
  bennet_hash_table_free(const_char_ptr, sexp_ptr)(&empty_su);
  return result;
}

static sexp_t* expand_with_substitution(
    bennet_hash_table(const_char_ptr, sexp_ptr) * su, sexp_t* exp) {
  bennet_optional(let_binding_t) let_opt = is_let(exp);
  if (bennet_optional_is_some(let_opt)) {
    let_binding_t let_binding = bennet_optional_unwrap(let_opt);

    // Expand all bindings with current substitution
    bennet_hash_table(const_char_ptr, sexp_ptr) binds1;
    bennet_hash_table_init(const_char_ptr, sexp_ptr)(
        &binds1, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

    // Process bindings from let_binding.bindings hash table
    for (size_t i = 0; i < let_binding.bindings.capacity; i++) {
      if (let_binding.bindings.entries[i].occupied) {
        const char* key = let_binding.bindings.entries[i].key;
        sexp_t* value = let_binding.bindings.entries[i].value;
        sexp_t* expanded_value = expand_with_substitution(su, value);
        bennet_hash_table_set(const_char_ptr, sexp_ptr)(&binds1, key, expanded_value);
      }
    }

    // Merge binds1 with su (binds1 takes precedence)
    bennet_hash_table(const_char_ptr, sexp_ptr) su1;
    bennet_hash_table_init(const_char_ptr, sexp_ptr)(
        &su1, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

    // Copy su first
    for (size_t i = 0; i < su->capacity; i++) {
      if (su->entries[i].occupied) {
        bennet_hash_table_set(const_char_ptr, sexp_ptr)(
            &su1, su->entries[i].key, su->entries[i].value);
      }
    }

    // Copy binds1 (overwriting any conflicts)
    for (size_t i = 0; i < binds1.capacity; i++) {
      if (binds1.entries[i].occupied) {
        bennet_hash_table_set(const_char_ptr, sexp_ptr)(
            &su1, binds1.entries[i].key, binds1.entries[i].value);
      }
    }

    sexp_t* result = expand_with_substitution(&su1, let_binding.body);

    bennet_hash_table_free(const_char_ptr, sexp_ptr)(&binds1);
    bennet_hash_table_free(const_char_ptr, sexp_ptr)(&su1);
    bennet_hash_table_free(const_char_ptr, sexp_ptr)(&let_binding.bindings);

    return result;
  } else {
    // Not a let expression
    if (sexp_is_atom(exp)) {
      // Check for substitution
      const char* atom_str = sexp_to_string(exp);
      bennet_optional(sexp_ptr) subst =
          bennet_hash_table_get(const_char_ptr, sexp_ptr)(su, atom_str);
      if (bennet_optional_is_some(subst)) {
        return bennet_optional_unwrap(subst);
      } else {
        return exp;
      }
    } else {
      // List - recursively expand elements
      size_t list_count;
      sexp_t** list_elements = sexp_to_list(exp, &list_count);
      if (!list_elements) {
        return exp;
      }

      sexp_t** new_elements = malloc(sizeof(sexp_t*) * list_count);
      if (!new_elements) {
        fprintf(stderr, "Memory allocation failed in expand_with_substitution\n");
        assert(false);
      }

      for (size_t i = 0; i < list_count; i++) {
        new_elements[i] = expand_with_substitution(su, list_elements[i]);
      }

      sexp_t* result = sexp_list(new_elements, list_count);
      free(new_elements);
      return result;
    }
  }
}

/** Try to decode an s-expression as a boolean
    Throws {!UnexpectedSolverResponse}. */
bool to_bool(sexp_t* exp) {
  if (!sexp_is_atom(exp)) {
    unexpected_solver_response(exp, "to_bool - expected atom");
  }

  const char* atom_str = sexp_to_string(exp);
  if (strcmp(atom_str, "true") == 0) {
    return true;
  } else if (strcmp(atom_str, "false") == 0) {
    return false;
  } else {
    unexpected_solver_response(exp, "to_bool - expected true or false");
  }
  return false;  // Never reached
}

/** Try to decode an s-expression as a bitvector of the given width.
    The 2nd argument indicates if the number is signed.
    Throws {!UnexpectedSolverResponse}. */
int64_t to_bits(int w, bool signed_val, sexp_t* exp) {
  if (!sexp_is_atom(exp)) {
    unexpected_solver_response(exp, "to_bits - expected atom");
  }

  const char* s = sexp_to_string(exp);
  size_t len = strlen(s);

  if (len < 2 || s[0] != '#') {
    unexpected_solver_response(exp, "to_bits - expected # prefix");
  }

  int64_t result;
  if (s[1] == 'b') {
    // Binary format
    if (len - 2 != w) {
      unexpected_solver_response(exp, "to_bits - binary width mismatch");
    }
    result = 0;
    for (int i = 2; i < len; i++) {
      if (s[i] != '0' && s[i] != '1') {
        unexpected_solver_response(exp, "to_bits - invalid binary digit");
      }
      result = (result << 1) + (s[i] - '0');
    }
  } else if (s[1] == 'x') {
    // Hexadecimal format
    if ((len - 2) * 4 != w) {
      unexpected_solver_response(exp, "to_bits - hex width mismatch");
    }
    char* endptr;
    result = strtoll(s + 2, &endptr, 16);
    if (*endptr != '\0') {
      unexpected_solver_response(exp, "to_bits - invalid hex digit");
    }
  } else {
    unexpected_solver_response(exp, "to_bits - expected #b or #x format");
  }

  if (signed_val) {
    // Sign extend if needed
    if (w < 64 && (result & (1LL << (w - 1)))) {
      // Negative number, sign extend
      int64_t mask = ~((1LL << w) - 1);
      result |= mask;
    }
  }

  return result;
}

/** Try to decode an s-expression as an integer.
    Throws {!UnexpectedSolverResponse}. */
int64_t to_z(sexp_t* exp) {
  if (sexp_is_atom(exp)) {
    const char* s = sexp_to_string(exp);
    char* endptr;
    int64_t result = strtoll(s, &endptr, 10);
    if (*endptr != '\0') {
      unexpected_solver_response(exp, "to_z - invalid integer format");
    }
    return result;
  } else {
    // Could be (- number)
    size_t list_count;
    sexp_t** list_elements = sexp_to_list(exp, &list_count);
    if (list_elements && list_count == 2 && sexp_is_atom(list_elements[0]) &&
        strcmp(sexp_to_string(list_elements[0]), "-") == 0 &&
        sexp_is_atom(list_elements[1])) {
      const char* s = sexp_to_string(list_elements[1]);
      char* endptr;
      int64_t result = strtoll(s, &endptr, 10);
      if (*endptr != '\0') {
        unexpected_solver_response(exp, "to_z - invalid negative integer format");
      }
      return -result;
    } else {
      unexpected_solver_response(exp, "to_z - expected atom or (- number)");
    }
  }
  return 0;  // Never reached
}

/** Try to decode an s-expression as a rational number.
    Throws {!UnexpectedSolverResponse}. */
double to_q(sexp_t* exp) {
  if (sexp_is_atom(exp)) {
    const char* s = sexp_to_string(exp);
    char* endptr;
    double result = strtod(s, &endptr);
    if (*endptr != '\0') {
      unexpected_solver_response(exp, "to_q - invalid rational format");
    }
    return result;
  } else {
    size_t list_count;
    sexp_t** list_elements = sexp_to_list(exp, &list_count);

    if (list_elements && list_count == 2) {
      if (sexp_is_atom(list_elements[0]) &&
          strcmp(sexp_to_string(list_elements[0]), "-") == 0) {
        // Negative number
        double val = to_q(list_elements[1]);
        return -val;
      } else if (sexp_is_atom(list_elements[0]) &&
                 strcmp(sexp_to_string(list_elements[0]), "/") == 0) {
        unexpected_solver_response(exp, "to_q - division needs 3 elements");
      }
    } else if (list_elements && list_count == 3 && sexp_is_atom(list_elements[0]) &&
               strcmp(sexp_to_string(list_elements[0]), "/") == 0) {
      // Division
      double num = to_q(list_elements[1]);
      double den = to_q(list_elements[2]);
      if (den == 0.0) {
        unexpected_solver_response(exp, "to_q - division by zero");
      }
      return num / den;
    }

    unexpected_solver_response(exp, "to_q - unexpected list format");
  }
  return 0.0;  // Never reached
}

/** Try to decode an s-expression as constructor with field values.
    Throws {!UnexpectedSolverResponse}. */
constructor_result_t to_con(sexp_t* exp) {
  constructor_result_t result;

  if (sexp_is_atom(exp)) {
    // Simple constructor with no fields
    result.name = sexp_to_string(exp);
    result.fields = NULL;
    result.field_count = 0;
    return result;
  } else {
    size_t list_count;
    sexp_t** list_elements = sexp_to_list(exp, &list_count);

    if (!list_elements || list_count == 0) {
      unexpected_solver_response(exp, "to_con - empty list");
    }

    sexp_t* first = list_elements[0];
    if (sexp_is_atom(first)) {
      // Normal constructor
      result.name = sexp_to_string(first);
      result.field_count = list_count - 1;
      if (result.field_count > 0) {
        result.fields = malloc(sizeof(sexp_t*) * result.field_count);
        if (!result.fields) {
          fprintf(stderr, "Memory allocation failed in to_con\n");
          assert(false);
        }
        for (size_t i = 0; i < result.field_count; i++) {
          result.fields[i] = list_elements[i + 1];
        }
      } else {
        result.fields = NULL;
      }
      return result;
    } else {
      // Could be CVC5 format: (as con ty)
      size_t first_list_count;
      sexp_t** first_list_elements = sexp_to_list(first, &first_list_count);

      if (first_list_elements && first_list_count == 3 &&
          sexp_is_atom(first_list_elements[0]) &&
          strcmp(sexp_to_string(first_list_elements[0]), "as") == 0 &&
          sexp_is_atom(first_list_elements[1])) {
        // CVC5 format
        result.name = sexp_to_string(first_list_elements[1]);
        result.field_count = list_count - 1;
        if (result.field_count > 0) {
          result.fields = malloc(sizeof(sexp_t*) * result.field_count);
          if (!result.fields) {
            fprintf(stderr, "Memory allocation failed in to_con (cvc5 format)\n");
            assert(false);
          }
          for (size_t i = 0; i < result.field_count; i++) {
            result.fields[i] = list_elements[i + 1];
          }
        } else {
          result.fields = NULL;
        }
        return result;
      } else {
        unexpected_solver_response(exp, "to_con - unexpected list format");
      }
    }
  }

  // This should never be reached due to unexpected_solver_response above
  constructor_result_t error_result = {0};
  return error_result;
}

/** Try to decode an s-expression as an array. The result is `(is,v)`
    where is are (key,value) pairs, and `v` is the default array value.
    Throws {!UnexpectedSolverResponse}. */
array_result_t to_array(sexp_t* exp0) {
  array_result_t result;
  result.pairs = NULL;
  result.pair_count = 0;
  result.default_value = NULL;

  // Arrays are represented as nested stores: (store (store ... (as const default) key1 val1) key2 val2)
  // We need to recursively extract the stores

  sexp_t* current = exp0;
  size_t pairs_capacity = 4;
  result.pairs = malloc(sizeof(*result.pairs) * pairs_capacity);
  if (!result.pairs) {
    fprintf(stderr, "Memory allocation failed in to_array\n");
    assert(false);
  }

  while (true) {
    if (sexp_is_atom(current)) {
      unexpected_solver_response(current, "to_array - unexpected atom");
    }

    size_t list_count;
    sexp_t** list_elements = sexp_to_list(current, &list_count);

    if (!list_elements || list_count == 0) {
      unexpected_solver_response(current, "to_array - empty list");
    }

    if (sexp_is_atom(list_elements[0]) &&
        strcmp(sexp_to_string(list_elements[0]), "store") == 0) {
      // Store operation: (store array index value)
      if (list_count != 4) {
        unexpected_solver_response(current, "to_array - store needs 4 elements");
      }

      // Expand pairs array if needed
      if (result.pair_count >= pairs_capacity) {
        pairs_capacity *= 2;
        result.pairs = realloc(result.pairs, sizeof(*result.pairs) * pairs_capacity);
        if (!result.pairs) {
          fprintf(stderr, "Memory reallocation failed in to_array\n");
          assert(false);
        }
      }

      // Add this store operation (note: we add in reverse order)
      result.pairs[result.pair_count].key = list_elements[2];
      result.pairs[result.pair_count].value = list_elements[3];
      result.pair_count++;

      // Continue with the base array
      current = list_elements[1];
    } else {
      // Should be (as const type) default_value format
      if (list_count != 2) {
        unexpected_solver_response(
            current, "to_array - expected (as const type) default");
      }

      if (!sexp_is_atom(list_elements[0])) {
        size_t as_list_count;
        sexp_t** as_list_elements = sexp_to_list(list_elements[0], &as_list_count);

        if (as_list_elements && as_list_count == 3 && sexp_is_atom(as_list_elements[0]) &&
            strcmp(sexp_to_string(as_list_elements[0]), "as") == 0 &&
            sexp_is_atom(as_list_elements[1]) &&
            strcmp(sexp_to_string(as_list_elements[1]), "const") == 0) {
          // Found (as const type)
          result.default_value = list_elements[1];
          break;
        }
      }

      unexpected_solver_response(current, "to_array - expected const base");
    }
  }

  // Reverse the pairs array since we added them in reverse order
  for (size_t i = 0; i < result.pair_count / 2; i++) {
    size_t j = result.pair_count - 1 - i;
    sexp_t* temp_key = result.pairs[i].key;
    sexp_t* temp_value = result.pairs[i].value;
    result.pairs[i].key = result.pairs[j].key;
    result.pairs[i].value = result.pairs[j].value;
    result.pairs[j].key = temp_key;
    result.pairs[j].value = temp_value;
  }

  return result;
}

// Memory management functions
void free_constructor_result(constructor_result_t* result) {
  if (result->fields) {
    free(result->fields);
    result->fields = NULL;
  }
  result->field_count = 0;
}

void free_array_result(array_result_t* result) {
  if (result->pairs) {
    free(result->pairs);
    result->pairs = NULL;
  }
  result->pair_count = 0;
}

void free_let_binding(let_binding_t* binding) {
  bennet_hash_table_free(const_char_ptr, sexp_ptr)(&binding->bindings);
}

/** {1 SMT to Term} */

// Forward declaration
static cn_term* get_value_impl(cn_base_type bt, sexp_t* sexp);

/** Translate an SMT value to a CN term */
cn_term* get_ivalue(cn_base_type bt, sexp_t* sexp) {
  // In CN, IT wraps the value with base type and location info
  // For C implementation, we just return the term directly since cn_term already has base_type
  return get_value_impl(bt, sexp);
}

/** Get the value from an SMT s-expression based on base type */
cn_term* get_value(cn_base_type bt, sexp_t* sexp) {
  return get_value_impl(bt, sexp);
}

// Some constants that would normally be defined elsewhere
// These are simplified placeholders for the actual CN constants
#define CN_LIST_NIL_NAME    "nil"
#define CN_LIST_CONS_NAME   "cons"
#define CN_OPTION_SOME_NAME "some"
#define CN_OPTION_NONE_NAME "none"

static cn_term* get_value_impl(cn_base_type bt, sexp_t* sexp) {
  switch (bt.tag) {
    case CN_BASE_UNIT:
      return cn_smt_unit();

    case CN_BASE_BOOL:
      return cn_smt_bool(to_bool(sexp));

    case CN_BASE_INTEGER:
      return cn_smt_z(to_z(sexp));

    case CN_BASE_BITS: {
      // Extract sign and width information from the base type
      cn_bits_info bits_info = cn_base_type_get_bits_info(bt);
      bool is_signed = bits_info.is_signed;
      int width = bits_info.size_bits;
      int64_t value = to_bits(width, is_signed, sexp);
      return cn_smt_bits(is_signed, width, value);
    }

    case CN_BASE_REAL:
      return cn_smt_rational(to_q(sexp));

    case CN_BASE_LOC: {
      bool is_signed = false;
      int width = CHAR_BIT * sizeof(uintptr_t);
      uintptr_t value = to_bits(width, is_signed, sexp);
      return cn_smt_pointer(value);
    }

    case CN_BASE_LIST: {
      constructor_result_t con_result = to_con(sexp);

      if (strcmp(con_result.name, CN_LIST_NIL_NAME) == 0 && con_result.field_count == 0) {
        free_constructor_result(&con_result);
        // Return nil - in CN this would be a specific nil constructor
        // For now we'll create a default value
        return cn_smt_default(cn_base_type_simple(CN_BASE_LIST));
      }

      if (strcmp(con_result.name, CN_LIST_CONS_NAME) == 0 &&
          con_result.field_count == 2) {
        // For cons, we'd need element type to recurse properly
        // This is simplified for now
        free_constructor_result(&con_result);
        return cn_smt_default(cn_base_type_simple(CN_BASE_LIST));
      }

      free_constructor_result(&con_result);
      fprintf(stderr, "Unsupported List constructor\\n");
      assert(false);
      return NULL;
    }

    case CN_BASE_MAP: {
      // For maps, we need to handle array representation
      array_result_t array_result = to_array(sexp);

      // Create a map constant with default value
      cn_term* base = get_ivalue(cn_base_type_simple(CN_BASE_INTEGER),
          array_result.default_value);  // vt should come from Map(kt,vt)

      // Apply all the store operations
      cn_term* result = base;
      for (size_t i = 0; i < array_result.pair_count; i++) {
        cn_term* key = get_ivalue(cn_base_type_simple(CN_BASE_INTEGER),
            array_result.pairs[i].key);  // kt
        cn_term* value = get_ivalue(cn_base_type_simple(CN_BASE_INTEGER),
            array_result.pairs[i].value);  // vt
        result = cn_smt_map_set(result, key, value);
      }

      free_array_result(&array_result);
      return result;
    }

    case CN_BASE_TUPLE: {
      constructor_result_t con_result = to_con(sexp);

      // For tuples, we'd need to know the component types
      // This is a simplified implementation
      free_constructor_result(&con_result);
      return cn_smt_default(cn_base_type_simple(CN_BASE_TUPLE));
    }

    case CN_BASE_STRUCT: {
      constructor_result_t con_result = to_con(sexp);

      // For structs, we'd need to look up the struct declaration
      // This is a simplified implementation
      free_constructor_result(&con_result);
      return cn_smt_default(cn_base_type_simple(CN_BASE_STRUCT));
    }

    case CN_BASE_RECORD: {
      constructor_result_t con_result = to_con(sexp);

      // For records, we'd create a record with the field mappings
      // This is a simplified implementation
      free_constructor_result(&con_result);
      return cn_smt_default(cn_base_type_record(NULL, NULL, 0));
    }

    case CN_BASE_OPTION: {
      constructor_result_t con_result = to_con(sexp);

      if (strcmp(con_result.name, CN_OPTION_SOME_NAME) == 0 &&
          con_result.field_count == 1) {
        // For Some(value), we'd recursively convert the inner value
        // This is simplified
        free_constructor_result(&con_result);
        return cn_smt_default(cn_base_type_simple(CN_BASE_OPTION));
      }

      if (strcmp(con_result.name, CN_OPTION_NONE_NAME) == 0 &&
          con_result.field_count == 0) {
        free_constructor_result(&con_result);
        return cn_smt_default(cn_base_type_simple(CN_BASE_OPTION));
      }

      // Handle CVC5 format: "as", [Atom "none"; _]
      if (strcmp(con_result.name, "as") == 0 && con_result.field_count == 2) {
        if (sexp_is_atom(con_result.fields[0]) &&
            strcmp(sexp_to_string(con_result.fields[0]), CN_OPTION_NONE_NAME) == 0) {
          free_constructor_result(&con_result);
          return cn_smt_default(cn_base_type_simple(CN_BASE_OPTION));
        }
      }

      free_constructor_result(&con_result);
      fprintf(stderr, "Unsupported Option constructor\\n");
      assert(false);
      return NULL;
    }

    default:
      // For any other base types, return a default value
      return cn_smt_default(bt);
  }
}
