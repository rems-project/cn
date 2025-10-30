#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/sexp.h>
#include <cn-smt/solver.h>
#include <cn-smt/subst.h>
#include <cn-smt/terms.h>
#include <cn-smt/to_smt.h>

// Generate vector implementation for cn_member_pair
BENNET_VECTOR_IMPL(cn_member_pair)

// Generate vector implementation for cn_term_ptr
BENNET_VECTOR_IMPL(cn_term_ptr)

// Generate vector implementation for cn_match_case
BENNET_VECTOR_IMPL(cn_match_case)

// Functions that pick names for things (converted from OCaml CN_Names module)

// Helper function to get string representation of a symbol without numbers
const char* sym_pp_string_no_nums(cn_sym sym) {
  // Return the name part of the cn_sym struct
  return sym.name;
}

// Helper function to get the numeric part of a symbol
uint64_t sym_num(cn_sym sym) {
  // Return the id part of the cn_sym struct
  return sym.id;
}

// Helper function to get string from Id
const char* id_get_string(int id) {
  // This would need to be implemented based on the actual Id module
  static char buffer[256];
  snprintf(buffer, sizeof(buffer), "id_%d", id);
  return buffer;
}

char* fn_name(cn_sym x) {
  assert(x.name);
  const char* base_name = sym_pp_string_no_nums(x);
  uint64_t num = sym_num(x);

  // Calculate required buffer size: base_name + "_" + num + null terminator
  size_t len = strlen(base_name) + 1 + snprintf(NULL, 0, "%" PRIu64, num) + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_%" PRIu64, base_name, num);
  return result;
}

// Function definition name - just append _func without number
char* fn_def_name(cn_sym x) {
  assert(x.name);
  const char* base_name = sym_pp_string_no_nums(x);

  // Append _func suffix
  size_t len = strlen(base_name) + 6;  // +5 for "_func" +1 for null
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_func", base_name);
  return result;
}

const char* named_expr_name(void) {
  return "_cn_named";
}

char* cn_smt_struct_name(const char* name) {
  // Calculate required buffer size: base_name + "_struct" + null terminator
  size_t len = strlen(name) + snprintf(NULL, 0, "_struct") + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_struct", name);
  return result;
}

char* struct_con_name(const char* name) {
  // Calculate required buffer size: name + "_struct_con" + null terminator
  size_t len = strlen(name) + snprintf(NULL, 0, "_struct_con") + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_struct_con", name);
  return result;
}

char* struct_field_name(const char* member_name) {
  // Calculate required buffer size: member_name + "_struct_fld" + null terminator
  size_t len = strlen(member_name) + strlen("_struct_fld") + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_struct_fld", member_name);
  return result;
}

char* datatype_name(cn_sym x) {
  const char* base_name = sym_pp_string_no_nums(x);
  uint64_t num = sym_num(x);

  // Calculate required buffer size: base_name + "_" + num + null terminator
  size_t len = strlen(base_name) + 1 + snprintf(NULL, 0, "%" PRIu64, num) + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_%" PRIu64, base_name, num);
  return result;
}

char* datatype_con_name(cn_sym x) {
  const char* base_name = sym_pp_string_no_nums(x);
  uint64_t num = sym_num(x);

  // Calculate required buffer size: base_name + "_" + num + null terminator
  size_t len = strlen(base_name) + 1 + snprintf(NULL, 0, "%" PRIu64, num) + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_%" PRIu64, base_name, num);
  return result;
}

char* datatype_field_name(int x) {
  const char* base_name = id_get_string(x);

  // Calculate required buffer size: base_name + "_data_fld" + null terminator
  size_t len = strlen(base_name) + strlen("_data_fld") + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_data_fld", base_name);
  return result;
}

// Global counter for fresh name generation
static int fresh_counter = 0;

/** Generate a fresh name */
char* fresh_name(const char* x) {
  // Calculate required buffer size: x + "_" + fresh_counter + null terminator
  size_t len = strlen(x) + 1 + snprintf(NULL, 0, "%d", fresh_counter) + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "%s_%d", x, fresh_counter);
  fresh_counter++;
  return result;
}

/* Note: CVC5 would have support for arbitrary tuples without declaring them. */

// CN_Tuple module functionality
const int CN_TUPLE_MAX_ARITY =
    15; /* TODO: compute required arity based on the input program. */

static char* cn_tuple_name(int arity) {
  assert(arity <= CN_TUPLE_MAX_ARITY);

  // Calculate required buffer size: "cn_tuple_" + arity + null terminator
  size_t len = strlen("cn_tuple_") + snprintf(NULL, 0, "%d", arity) + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "cn_tuple_%d", arity);
  return result;
}

static char* cn_tuple_selector(int arity, int field) {
  assert(arity <= CN_TUPLE_MAX_ARITY);

  // Calculate required buffer size: "cn_get_" + field + "_of_" + arity + null terminator
  size_t len = strlen("cn_get_") + snprintf(NULL, 0, "%d", field) + strlen("_of_") +
               snprintf(NULL, 0, "%d", arity) + 1;
  char* result = malloc(len);
  assert(result);

  sprintf(result, "cn_get_%d_of_%d", field, arity);
  return result;
}

/** A tuple type with the given list of types */
sexp_t* cn_tuple_type_name(sexp_t** types, size_t type_count) {
  int arity = (int)type_count;
  char* name = cn_tuple_name(arity);
  sexp_t* result = sexp_app_str(name, types, type_count);
  free(name);
  return result;
}

/** Make a tuple value */
char* cn_tuple_constructor_name(int arity) {
  assert(arity <= CN_TUPLE_MAX_ARITY);
  return cn_tuple_name(arity);
}

/** Get a field of a tuple */
char* cn_tuple_get_selector_name(int arity, int field) {
  return cn_tuple_selector(arity, field);
}

// CN_Option module functionality (converted from OCaml)

/** Option type name */
const char* cn_option_name = "cn_option";

/** None constructor name */
const char* cn_option_none_name = "cn_none";

/** Some constructor name */
const char* cn_option_some_name = "cn_some";

/** Value field name */
const char* cn_option_val_name = "cn_val";

/** Create an option type with element type a */
sexp_t* cn_option_type(sexp_t* a) {
  sexp_t* args[] = {a};
  return sexp_app_str(cn_option_name, args, 1);
}

/** Create a None value with given element type */
sexp_t* cn_option_none(sexp_t* elT) {
  return sexp_as_type(sexp_atom(cn_option_none_name), cn_option_type(elT));
}

/** Create a Some value with given content */
sexp_t* cn_option_some(sexp_t* x) {
  sexp_t* args[] = {x};
  return sexp_app_str(cn_option_some_name, args, 1);
}

/** Test if an option value is Some */
sexp_t* cn_option_is_some(sexp_t* x) {
  // Generate "is-cn_some" predicate name
  size_t len = strlen("is-") + strlen(cn_option_some_name) + 1;
  char* predicate_name = malloc(len);
  assert(predicate_name);
  sprintf(predicate_name, "is-%s", cn_option_some_name);

  sexp_t* args[] = {x};
  sexp_t* result = sexp_app_str(predicate_name, args, 1);

  free(predicate_name);
  return result;
}

/** Extract the value from a Some option */
sexp_t* cn_option_val(sexp_t* x) {
  sexp_t* args[] = {x};
  return sexp_app_str(cn_option_val_name, args, 1);
}

// CN_List module functionality (placeholder)
sexp_t* cn_list_type(sexp_t* element_type) {
  sexp_t* args[] = {element_type};
  return sexp_app_str("cn_list", args, 1);
}

/** Translate a base type to SMT */
sexp_t* translate_base_type(cn_base_type bt) {
  switch (bt.tag) {
    case CN_BASE_UNIT: {
      // Unit -> CN_Tuple.t []
      return cn_option_type(sexp_list(NULL, 0));  // Empty tuple
    }

    case CN_BASE_BOOL: {
      // Bool -> SMT.t_bool
      return t_bool();
    }

    case CN_BASE_INTEGER: {
      // Integer -> SMT.t_int
      return t_int();
    }

    case CN_BASE_BITS: {
      // Bits (_, n) -> SMT.t_bits n
      return t_bits(bt.data.bits.size_bits);
    }

    case CN_BASE_REAL: {
      // Real -> SMT.t_real
      return t_real();
    }

    case CN_BASE_LOC: {
      return t_loc();
    }

    case CN_BASE_CTYPE: {
      // CType -> SMT.t_int
      return t_int();
    }

    case CN_BASE_LIST: {
      // List bt -> CN_List.t (translate_base_type bt)
      assert(bt.data.list.element_type);
      sexp_t* element_sexp = translate_base_type(*bt.data.list.element_type);
      if (!element_sexp) {
        return NULL;
      }
      sexp_t* result = cn_list_type(element_sexp);
      return result;
    }

    case CN_BASE_SET: {
      // Set bt -> CN_Set.t (translate_base_type bt)
      assert(bt.data.set.element_type);
      sexp_t* element_sexp = translate_base_type(*bt.data.set.element_type);
      if (!element_sexp) {
        return NULL;
      }
      sexp_t* result = t_set(element_sexp);
      return result;
    }

    case CN_BASE_MAP: {
      // Map (k, v) -> SMT.t_array (translate_base_type k) (translate_base_type v)
      if (!bt.data.map.key_type || !bt.data.map.value_type) {
        return NULL;
      }

      sexp_t* key_sexp = translate_base_type(*bt.data.map.key_type);
      sexp_t* value_sexp = translate_base_type(*bt.data.map.value_type);

      if (!key_sexp || !value_sexp) {
        return NULL;
      }

      sexp_t* result = t_array(key_sexp, value_sexp);
      return result;
    }

    case CN_BASE_TUPLE: {
      // Tuple bts -> CN_Tuple.t (List.map translate_base_type bts)

      sexp_t** translated_types = malloc(bt.data.tuple.count * sizeof(sexp_t*));
      if (!translated_types) {
        return NULL;
      }

      for (size_t i = 0; i < bt.data.tuple.count; i++) {
        translated_types[i] = translate_base_type(bt.data.tuple.types[i]);
        if (!translated_types[i]) {
          // Clean up on failure
          for (size_t j = 0; j < i; j++) {
          }
          free(translated_types);
          return NULL;
        }
      }

      sexp_t* result = cn_tuple_type_name(translated_types, bt.data.tuple.count);

      // Clean up
      for (size_t i = 0; i < bt.data.tuple.count; i++) {
      }
      free(translated_types);
      return result;
    }

    case CN_BASE_STRUCT: {
      // Struct tag -> SMT.atom (CN_Names.struct_name tag)
      // Convert int tag to cn_sym (assuming tag is symbol id)
      char* struct_name_str = cn_smt_struct_name(bt.data.struct_tag.tag);
      assert(struct_name_str);

      sexp_t* result = sexp_atom(struct_name_str);
      free(struct_name_str);
      return result;
    }

    case CN_BASE_DATATYPE: {
      // Datatype tag -> SMT.atom (datatype name with _dt suffix)
      const char* tag_name = bt.data.datatype_tag.tag;
      assert(tag_name);

      // Create datatype SMT name: tag_name + "_dt"
      size_t len = strlen(tag_name) + 4;  // "_dt" + null terminator
      char* datatype_name_str = malloc(len);
      assert(datatype_name_str);
      snprintf(datatype_name_str, len, "%s_dt", tag_name);

      sexp_t* result = sexp_atom(datatype_name_str);
      free(datatype_name_str);
      return result;
    }

    case CN_BASE_OPTION: {
      // Option bt -> CN_Option.t (translate_base_type bt)
      if (!bt.data.option.element_type) {
        return NULL;
      }
      sexp_t* element_sexp = translate_base_type(*bt.data.option.element_type);
      if (!element_sexp) {
        return NULL;
      }
      sexp_t* result = cn_option_type(element_sexp);
      return result;
    }

    case CN_BASE_RECORD: {
      // Record members -> translate_base_type (Tuple (List.map snd members))

      // Create a tuple of the member types
      cn_base_type tuple_bt =
          cn_base_type_tuple(bt.data.record.types, bt.data.record.count);

      return translate_base_type(tuple_bt);
    }

    default:
      return NULL;
  }
}

/////////////////
/* Term to SMT */
/////////////////

/** Translate a constant to SMT */
sexp_t* translate_const(void* s, cn_const* co) {
  if (!co) {
    return NULL;
  }

  switch (co->type) {
    case CN_CONST_Z: {
      // Z z -> SMT.int_zk z
      return int_k(co->data.z);
    }

    case CN_CONST_BITS: {
      // Bits ((_, w), z) -> SMT.bv_k w z
      return bv_k(co->data.bits.info.size_bits, co->data.bits.value);
    }

    case CN_CONST_Q: {
      // Q q -> SMT.real_k q
      char buffer[64];
      snprintf(buffer, sizeof(buffer), "%.15g", co->data.q);
      return sexp_atom(buffer);
    }

    case CN_CONST_POINTER: {
      return loc_k(co->data.pointer);
    }

    case CN_CONST_BOOL: {
      // Bool b -> SMT.bool_k b
      return bool_k(co->data.boolean);
    }

    case CN_CONST_UNIT: {
      // Unit -> type-annotated 0-tuple
      // Create the nullary tuple type
      sexp_t* tuple_type = cn_tuple_type_name(NULL, 0);
      assert(tuple_type);

      // Create type-annotated tuple constructor
      char* tuple_name = cn_tuple_constructor_name(0);
      sexp_t* constructor_atom = sexp_atom(tuple_name);
      sexp_t* result = sexp_as_type(constructor_atom, tuple_type);

      // Cleanup
      free(tuple_name);

      return result;
    }

    case CN_CONST_NULL: {
      // Null -> CN_Pointer.con_null
      return loc_k((uintptr_t)NULL);
    }

    case CN_CONST_DEFAULT: {
      // Default t -> CN_Option.val_ (CN_Option.none (translate_base_type t))
      sexp_t* base_type_sexp = translate_base_type(co->data.default_type);
      assert(base_type_sexp);

      sexp_t* none_option = cn_option_none(base_type_sexp);
      sexp_t* result = cn_option_val(none_option);
      return result;
    }

    default:
      assert(false);
      return NULL;
  }
}

/** Helper to check if base type is a bits type and extract sign and size */
static bool is_bits_type(cn_base_type bt, bool* is_signed, int* size) {
  if (bt.tag != CN_BASE_BITS) {
    return false;
  }

  *is_signed = bt.data.bits.is_signed;
  *size = bt.data.bits.size_bits;

  return true;
}

/** Casting between bit-vector types */
sexp_t* bv_cast(cn_base_type to_type, cn_base_type from_type, sexp_t* x) {
  assert(x != NULL);

  bool to_signed, from_signed;
  int to_sz, from_sz;

  assert(is_bits_type(to_type, &to_signed, &to_sz));        // Error: non-bv type
  assert(is_bits_type(from_type, &from_signed, &from_sz));  // Error: non-bv type

  if (to_sz == from_sz) {
    // Same size, return as-is
    sexp_t* result = sexp_app(x, NULL, 0);  // Deep copy
    assert(result != NULL);
    return result;
  } else if (to_sz < from_sz) {
    // Truncate using extract
    sexp_t* result = bv_extract(to_sz - 1, 0, x);
    assert(result != NULL);
    return result;
  } else if (from_signed) {
    // Sign extend
    sexp_t* result = bv_sign_extend(to_sz - from_sz, x);
    assert(result != NULL);
    return result;
  } else {
    // Zero extend
    sexp_t* result = bv_zero_extend(to_sz - from_sz, x);
    assert(result != NULL);
    return result;
  }
}

// Forward declarations for recursive functions
static sexp_t* bv_clz_count(int result_w, int w, sexp_t* e);
static sexp_t* bv_ctz_count(int result_w, int w, sexp_t* e);

/** [bv_clz result_w w e] counts the leading zeroes in [e], which should
    be a bit-vector of width [w].  The result is a bit-vector of width [result_w].
    Note that this duplicates [e]. */
sexp_t* bv_clz(int result_w, int w, sexp_t* e) {
  assert(result_w > 0);
  assert(w > 0);
  assert(e != NULL);

  return bv_clz_count(result_w, w, e);
}

static sexp_t* bv_clz_count(int result_w, int w, sexp_t* e) {
  if (w == 1) {
    sexp_t* zero = bv_k(w, 0);
    assert(zero != NULL);

    sexp_t* is_zero = eq(e, zero);
    assert(is_zero != NULL);

    sexp_t* one_result = bv_k(result_w, 1);
    assert(one_result != NULL);

    sexp_t* zero_result = bv_k(result_w, 0);
    assert(zero_result != NULL);

    sexp_t* result = ite(is_zero, one_result, zero_result);
    assert(result != NULL);

    return result;
  } else {
    int top_w = w / 2;
    int bot_w = w - top_w;

    sexp_t* top = bv_extract(w - 1, w - top_w, e);
    assert(top != NULL);

    sexp_t* bot = bv_extract(bot_w - 1, 0, e);
    assert(bot != NULL);

    sexp_t* top_zero = bv_k(top_w, 0);
    assert(top_zero != NULL);

    sexp_t* top_is_zero = eq(top, top_zero);
    assert(top_is_zero != NULL);

    sexp_t* bot_count = bv_clz_count(result_w, bot_w, bot);
    assert(bot_count != NULL);

    sexp_t* top_count = bv_clz_count(result_w, top_w, top);
    assert(top_count != NULL);

    sexp_t* top_w_const = bv_k(result_w, top_w);
    assert(top_w_const != NULL);

    sexp_t* bot_plus_topw = bv_add(bot_count, top_w_const);
    assert(bot_plus_topw != NULL);

    sexp_t* result = ite(top_is_zero, bot_plus_topw, top_count);
    assert(result != NULL);

    // Clean up

    return result;
  }
}

/** [bv_ctz result_w w e] counts the trailing zeroes in [e], which should
    be a bit-vector of width [w].  The result is a bit-vector of width [result_w].
    Note that this duplicates [e]. */
sexp_t* bv_ctz(int result_w, int w, sexp_t* e) {
  if (!e || w <= 0 || result_w <= 0) {
    return NULL;
  }

  return bv_ctz_count(result_w, w, e);
}

static sexp_t* bv_ctz_count(int result_w, int w, sexp_t* e) {
  if (w == 1) {
    sexp_t* zero = bv_k(w, 0);
    assert(zero != NULL);

    sexp_t* is_zero = eq(e, zero);
    assert(is_zero != NULL);

    sexp_t* one_result = bv_k(result_w, 1);
    assert(one_result != NULL);

    sexp_t* zero_result = bv_k(result_w, 0);
    assert(zero_result != NULL);

    sexp_t* result = ite(is_zero, one_result, zero_result);
    assert(result != NULL);

    return result;
  } else {
    int top_w = w / 2;
    int bot_w = w - top_w;

    sexp_t* top = bv_extract(w - 1, w - top_w, e);
    assert(top != NULL);

    sexp_t* bot = bv_extract(bot_w - 1, 0, e);
    assert(bot != NULL);

    sexp_t* bot_zero = bv_k(bot_w, 0);
    assert(bot_zero != NULL);

    sexp_t* bot_is_zero = eq(bot, bot_zero);
    assert(bot_is_zero != NULL);

    sexp_t* bot_count = bv_ctz_count(result_w, bot_w, bot);
    assert(bot_count != NULL);

    sexp_t* top_count = bv_ctz_count(result_w, top_w, top);
    assert(top_count != NULL);

    sexp_t* bot_w_const = bv_k(result_w, bot_w);
    assert(bot_w_const != NULL);

    sexp_t* top_plus_botw = bv_add(top_count, bot_w_const);
    assert(top_plus_botw != NULL);

    sexp_t* result = ite(bot_is_zero, top_plus_botw, bot_count);
    assert(result != NULL);

    // Clean up

    return result;
  }
}

// Forward declarations for translate_term helper functions
sexp_t* translate_term(struct cn_smt_solver* s, cn_term* iterm);
static bennet_optional(sexp_ptr) get_num_z(cn_term* term, int64_t* result);
static sexp_t* uninterp_same_type(struct cn_smt_solver* s,
    cn_term* iterm,
    cn_term* e1,
    cn_term* e2,
    const char* (*name_fn)(cn_base_type));

// Helper function names from OCaml CN_Names module
const char* mul_name(cn_base_type bt) {
  return "cn_mul";
}
const char* div_name(cn_base_type bt) {
  return "cn_div";
}
const char* exp_name(cn_base_type bt) {
  return "cn_exp";
}
const char* rem_name(cn_base_type bt) {
  return "cn_rem";
}
const char* mod_name(cn_base_type bt) {
  return "cn_mod";
}

/** Helper to extract integer literal value */
static bennet_optional(sexp_ptr) get_num_z(cn_term* term, int64_t* result) {
  if (term && term->type == CN_TERM_CONST && term->data.const_val.type == CN_CONST_Z) {
    *result = term->data.const_val.data.z;
    sexp_t* found = sexp_atom("found");  // Placeholder for Some
    return bennet_optional_some(sexp_ptr, found);
  }
  return bennet_optional_none(sexp_ptr);  // None
}

/** Binary uninterpreted function with same type for arguments and result */
static sexp_t* uninterp_same_type(struct cn_smt_solver* s,
    cn_term* iterm,
    cn_term* e1,
    cn_term* e2,
    const char* (*name_fn)(cn_base_type)) {
  sexp_t* s1 = translate_term(s, e1);
  sexp_t* s2 = translate_term(s, e2);
  const char* fn_name = name_fn(iterm->base_type);

  sexp_t* fn_atom = sexp_atom(fn_name);
  sexp_t* args[] = {s1, s2};
  sexp_t* result = sexp_app(fn_atom, args, 2);

  return result;
}

/** Translate a CN term to SMT */
sexp_t* translate_term(struct cn_smt_solver* s, cn_term* iterm) {
  assert(s && iterm);

  // Get location (placeholder - would need actual location from term)
  // cn_location loc = cn_term_get_loc(iterm);
  // struct_decls would be extracted from 's' parameter

  switch (iterm->type) {
    case CN_TERM_CONST:
      return translate_const(s, &iterm->data.const_val);

    case CN_TERM_SYM: {
      char* name = fn_name(iterm->data.sym);
      sexp_t* result = sexp_atom(name);
      free(name);
      return result;
    }

    case CN_TERM_UNOP: {
      cn_term* e1 = iterm->data.unop.operand;

      switch (iterm->data.unop.op) {
        case CN_UNOP_BW_FFS_NOSMT: {
          // NOTE: This desugaring duplicates e1
          // let intl i = int_lit_ i (IT.get_bt e1) loc in
          // translate_term s (ite_ (eq_ (e1, intl 0) loc, intl 0, add_ (arith_unop BW_CTZ_NoSMT e1 loc, intl 1) loc) loc)

          cn_term* zero = cn_smt_num(e1->base_type, 0);  // int_lit with e1's base type
          cn_term* one = cn_smt_num(e1->base_type, 1);

          // Create eq_(e1, 0)
          cn_term* eq_zero = cn_smt_eq(e1, zero);

          // Create arith_unop BW_CTZ_NoSMT e1
          cn_term* ctz_term = cn_term_alloc(CN_TERM_UNOP, e1->base_type);
          ctz_term->data.unop.op = CN_UNOP_BW_CTZ_NOSMT;
          ctz_term->data.unop.operand = e1;

          // Create add_(ctz_term, 1)
          cn_term* add_one = cn_smt_add(ctz_term, one);

          // Create ite_(eq_zero, 0, add_one)
          cn_term* ite_term = cn_smt_ite(eq_zero, zero, add_one);

          sexp_t* result = translate_term(s, ite_term);

          return result;
        }

        case CN_UNOP_BW_FLS_NOSMT: {
          // Copying and adjusting BW_FFS_NoSMT rule
          // NOTE: This desugaring duplicates e1

          // Get size from e1's bit type
          assert(e1->base_type.tag == CN_BASE_BITS);
          int sz = e1->base_type.data.bits.size_bits;

          cn_term* zero = cn_smt_num(e1->base_type, 0);
          cn_term* sz_term = cn_smt_num(e1->base_type, sz);

          // Create eq_(e1, 0)
          cn_term* eq_zero = cn_smt_eq(e1, zero);

          // Create arith_unop BW_CLZ_NoSMT e1
          cn_term* clz_term = cn_term_alloc(CN_TERM_UNOP, e1->base_type);
          clz_term->data.unop.op = CN_UNOP_BW_CLZ_NOSMT;
          clz_term->data.unop.operand = e1;

          // Create sub_(sz, clz_term)
          cn_term* sub_term = cn_smt_sub(sz_term, clz_term);

          // Create ite_(eq_zero, 0, sub_term)
          cn_term* ite_term = cn_smt_ite(eq_zero, zero, sub_term);

          sexp_t* result = translate_term(s, ite_term);

          return result;
        }

        case CN_UNOP_NOT: {
          sexp_t* operand_smt = translate_term(s, e1);
          return bool_not(operand_smt);
        }

        case CN_UNOP_NEGATE: {
          sexp_t* operand_smt = translate_term(s, e1);

          switch (iterm->base_type.tag) {
            case CN_BASE_BITS:
              return bv_neg(operand_smt);
            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_neg(operand_smt);
            default:
              assert(false);  // "Unop (Negate, _)"
              return NULL;
          }
        }

        case CN_UNOP_BW_COMPL: {
          sexp_t* operand_smt = translate_term(s, e1);

          if (iterm->base_type.tag == CN_BASE_BITS) {
            return bv_compl(operand_smt);
          } else {
            assert(false);  // "Unop (BW_Compl, _)"
            return NULL;
          }
        }

        case CN_UNOP_BW_CLZ_NOSMT: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            int w = iterm->base_type.data.bits.size_bits;

            // Helper function wrapper for bv_clz
            sexp_t* operand_smt;
            if (sexp_is_atom(translate_term(s, e1))) {
              operand_smt = bv_clz(w, w, translate_term(s, e1));
            } else {
              char* x = fresh_name(named_expr_name());
              sexp_t* x_atom = sexp_atom(x);
              sexp_t* result_expr = bv_clz(w, w, x_atom);
              sexp_t* bindings[] = {
                  sexp_list((sexp_t*[]){x_atom, translate_term(s, e1)}, 2)};
              sexp_t* result = sexp_let(bindings, 1, result_expr);
              free(x);
              operand_smt = result;
            }

            return bv_clz(w, w, operand_smt);
          } else {
            assert(false);  // "solver: BW_CLZ_NoSMT: not a bitwise type"
            return NULL;
          }
        }

        case CN_UNOP_BW_CTZ_NOSMT: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            int w = iterm->base_type.data.bits.size_bits;

            // Helper function wrapper for bv_ctz
            sexp_t* operand_smt;
            if (sexp_is_atom(translate_term(s, e1))) {
              operand_smt = bv_ctz(w, w, translate_term(s, e1));
            } else {
              char* x = fresh_name(named_expr_name());
              sexp_t* x_atom = sexp_atom(x);
              sexp_t* result_expr = bv_ctz(w, w, x_atom);
              sexp_t* bindings[] = {
                  sexp_list((sexp_t*[]){x_atom, translate_term(s, e1)}, 2)};
              sexp_t* result = sexp_let(bindings, 1, result_expr);
              free(x);
              operand_smt = result;
            }

            return bv_ctz(w, w, operand_smt);
          } else {
            assert(false);  // "solver: BW_CTZ_NoSMT: not a bitwise type"
            return NULL;
          }
        }

        default:
          return NULL;
      }
    }

    case CN_TERM_BINOP: {
      cn_term* e1 = iterm->data.binop.left;
      cn_term* e2 = iterm->data.binop.right;

      sexp_t* s1 = translate_term(s, e1);
      assert(s1);

      sexp_t* s2 = translate_term(s, e2);
      assert(s2);

      switch (iterm->data.binop.op) {
        case CN_BINOP_AND:
          return bool_and(s1, s2);

        case CN_BINOP_OR:
          return bool_or(s1, s2);

        case CN_BINOP_IMPLIES:
          return bool_implies(s1, s2);

        case CN_BINOP_ADD: {
          switch (iterm->base_type.tag) {
            case CN_BASE_BITS:
            case CN_BASE_LOC:  // Pointers treated as bitvectors for arithmetic
              return bv_add(s1, s2);
            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_add(s1, s2);
            default:
              assert(false);  // "Add"
              return NULL;
          }
        }

        case CN_BINOP_SUB: {
          switch (iterm->base_type.tag) {
            case CN_BASE_BITS:
            case CN_BASE_LOC:  // Pointers treated as bitvectors for arithmetic
              return bv_sub(s1, s2);
            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_sub(s1, s2);
            default:
              assert(false);  // "Sub"
              return NULL;
          }
        }

        case CN_BINOP_MUL: {
          switch (iterm->base_type.tag) {
            case CN_BASE_BITS:
              return bv_mul(s1, s2);
            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_mul(s1, s2);
            default:
              assert(false);  // "Mul"
              return NULL;
          }
        }

        case CN_BINOP_MULNOSMT:
          return uninterp_same_type(s, iterm, e1, e2, mul_name);

        case CN_BINOP_DIV: {
          switch (iterm->base_type.tag) {
            case CN_BASE_BITS: {
              if (iterm->base_type.data.bits.is_signed) {
                return bv_sdiv(s1, s2);
              } else {
                return bv_udiv(s1, s2);
              }
            }
            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_div(s1, s2);
            default:
              assert(false);  // "Div"
              return NULL;
          }
        }

        case CN_BINOP_DIVNOSMT:
          return uninterp_same_type(s, iterm, e1, e2, div_name);

        case CN_BINOP_EXP: {
          // Check if both operands are numeric literals
          int64_t z1, z2;
          bennet_optional(sexp_ptr) opt1 = get_num_z(e1, &z1);
          bennet_optional(sexp_ptr) opt2 = get_num_z(e2, &z2);
          if (bennet_optional_is_some(opt1) && bennet_optional_is_some(opt2) &&
              z2 <= INT_MAX && z2 >= 0) {
            // Compute z1^z2
            int64_t result = 1;
            for (int i = 0; i < z2; i++) {
              result *= z1;
            }

            cn_term* result_term = cn_smt_num(e1->base_type, result);
            return translate_term(s, result_term);
          } else {
            assert(false);  // "Exp"
            return NULL;
          }
        }

        case CN_BINOP_EXPNOSMT:
          return uninterp_same_type(s, iterm, e1, e2, exp_name);

        case CN_BINOP_REM: {
          switch (iterm->base_type.tag) {
            case CN_BASE_BITS: {
              if (iterm->base_type.data.bits.is_signed) {
                return bv_srem(s1, s2);
              } else {
                return bv_urem(s1, s2);
              }
            }
            case CN_BASE_INTEGER:
              return num_rem(s1, s2);
            default:
              assert(false);  // "Rem"
              return NULL;
          }
        }

        case CN_BINOP_REMNOSMT:
          return uninterp_same_type(s, iterm, e1, e2, rem_name);

        case CN_BINOP_MOD: {
          switch (iterm->base_type.tag) {
            case CN_BASE_BITS: {
              if (iterm->base_type.data.bits.is_signed) {
                return bv_smod(s1, s2);
              } else {
                return bv_urem(s1, s2);
              }
            }
            case CN_BASE_INTEGER:
              return num_mod(s1, s2);
            default:
              assert(false);  // "Mod"
              return NULL;
          }
        }

        case CN_BINOP_MODNOSMT:
          return uninterp_same_type(s, iterm, e1, e2, mod_name);

        case CN_BINOP_BW_XOR: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            return bv_xor(s1, s2);
          } else {
            assert(false);  // "BW_Xor"
            return NULL;
          }
        }

        case CN_BINOP_BW_AND: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            return bv_and(s1, s2);
          } else {
            assert(false);  // "BW_And"
            return NULL;
          }
        }

        case CN_BINOP_BW_OR: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            return bv_or(s1, s2);
          } else {
            assert(false);  // "BW_Or"
            return NULL;
          }
        }

        case CN_BINOP_SHIFT_LEFT: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            return bv_shl(s1, s2);
          } else {
            assert(false);  // "ShiftLeft"
            return NULL;
          }
        }

        case CN_BINOP_SHIFT_RIGHT: {
          if (iterm->base_type.tag == CN_BASE_BITS) {
            if (iterm->base_type.data.bits.is_signed) {
              return bv_ashr(s1, s2);
            } else {
              return bv_lshr(s1, s2);
            }
          } else {
            assert(false);  // "ShiftRight"
            return NULL;
          }
        }

        case CN_BINOP_LT: {
          switch (e1->base_type.tag) {
            case CN_BASE_LOC: {
              return bv_ult(s1, s2);
            }

            case CN_BASE_BITS: {
              if (e1->base_type.data.bits.is_signed) {
                return bv_slt(s1, s2);
              } else {
                return bv_ult(s1, s2);
              }
            }
            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_lt(s1, s2);

            default:
              assert(false);  // "LT"
              return NULL;
          }
        }

        case CN_BINOP_LE: {
          switch (e1->base_type.tag) {
            case CN_BASE_LOC: {
              return bv_uleq(s1, s2);
            }

            case CN_BASE_BITS: {
              if (e1->base_type.data.bits.is_signed) {
                return bv_sleq(s1, s2);
              } else {
                return bv_uleq(s1, s2);
              }
            }

            case CN_BASE_INTEGER:
            case CN_BASE_REAL:
              return num_leq(s1, s2);

            default:
              // The OCaml code has error handling here with BT.pp
              assert(false);  // "LE"
              return NULL;
          }
        }

        case CN_BINOP_MIN: {
          // NOTE: duplicates terms
          // translate_term s (ite_ (le_ (e1, e2) loc, e1, e2) loc)
          cn_term* le_term = cn_smt_le(e1, e2);
          cn_term* ite_term = cn_smt_ite(le_term, e1, e2);
          return translate_term(s, ite_term);
        }

        case CN_BINOP_MAX: {
          // NOTE: duplicates terms
          // translate_term s (ite_ (ge_ (e1, e2) loc, e1, e2) loc)
          cn_term* ge_term = cn_smt_ge(e1, e2);
          cn_term* ite_term = cn_smt_ite(ge_term, e1, e2);
          return translate_term(s, ite_term);
        }

        case CN_BINOP_EQ:
          return eq(s1, s2);

        case CN_BINOP_LT_POINTER: {
          // Cast both pointers to uintptr_bt and then use regular LT
          cn_base_type uintptr_bt =
              cn_base_type_bits(false, 64);  // Memory.uintptr_bt equivalent
          cn_term* cast_e1 = cn_smt_cast(uintptr_bt, e1);
          cn_term* cast_e2 = cn_smt_cast(uintptr_bt, e2);
          return translate_term(s, cn_smt_lt(cast_e1, cast_e2));
        }

        case CN_BINOP_LE_POINTER: {
          // Cast both pointers to uintptr_bt and then use regular LE
          cn_base_type uintptr_bt =
              cn_base_type_bits(false, 64);  // Memory.uintptr_bt equivalent
          cn_term* cast_e1 = cn_smt_cast(uintptr_bt, e1);
          cn_term* cast_e2 = cn_smt_cast(uintptr_bt, e2);
          return translate_term(s, cn_smt_le(cast_e1, cast_e2));
        }

        case CN_BINOP_SET_UNION:
          return set_union(s->ext, s1, s2);

        case CN_BINOP_SET_INTERSECTION:
          return set_intersection(s->ext, s1, s2);

        case CN_BINOP_SET_DIFFERENCE:
          return set_difference(s->ext, s1, s2);

        case CN_BINOP_SET_MEMBER:
          return set_member(s->ext, s1, s2);

        case CN_BINOP_SUBSET:
          return set_subset(s->ext, s1, s2);

        default:
          assert(false);
          return NULL;
      }
    }

    case CN_TERM_ITE: {
      sexp_t* cond_smt = translate_term(s, iterm->data.ite.cond);
      sexp_t* then_smt = translate_term(s, iterm->data.ite.then_term);
      sexp_t* else_smt = translate_term(s, iterm->data.ite.else_term);
      return ite(cond_smt, then_smt, else_smt);
    }

    case CN_TERM_MEMBER_SHIFT: {
      sexp_t* ptr_smt = translate_term(s, iterm->data.member_shift.base);

      return bv_add(ptr_smt, loc_k(iterm->data.member_shift.offset));
    }

    case CN_TERM_ARRAY_SHIFT: {
      sexp_t* base_smt = translate_term(s, iterm->data.array_shift.base);
      sexp_t* offset_smt = bv_mul(loc_k(iterm->data.array_shift.element_size),
          translate_term(s, iterm->data.array_shift.index));

      return bv_add(base_smt, offset_smt);
    }

    case CN_TERM_CAST: {
      // Cast (cbt, t) -> complex casting logic based on source and target types
      cn_term* t = iterm->data.cast.value;
      cn_base_type target_bt = iterm->base_type;
      cn_base_type source_bt = t->base_type;

      sexp_t* smt_term = translate_term(s, t);

      // Handle different cast combinations - simplified version
      if (source_bt.tag == CN_BASE_BITS && target_bt.tag == CN_BASE_LOC) {
        // Bits -> Loc
        return bv_cast(
            cn_base_type_bits(false, CHAR_BIT * sizeof(uintptr_t)), source_bt, smt_term);
      } else if (source_bt.tag == CN_BASE_LOC && target_bt.tag == CN_BASE_BITS) {
        // Loc -> Bits
        return bv_cast(
            target_bt, cn_base_type_bits(false, CHAR_BIT * sizeof(uintptr_t)), smt_term);
      } else if (source_bt.tag == CN_BASE_BITS && target_bt.tag == CN_BASE_BITS) {
        // Bits -> Bits: bv_cast
        return bv_cast(target_bt, source_bt, smt_term);
      } else if (source_bt.tag == CN_BASE_BITS && target_bt.tag == CN_BASE_INTEGER) {
        // Bits -> Integer: Bitvector to integer conversion
        sexp_t* args[] = {smt_term};
        return sexp_app_str("bv2int", args, 1);
      } else if (source_bt.tag == CN_BASE_LOC && target_bt.tag == CN_BASE_INTEGER) {
        // Loc -> Integer: LOC is represented as a bitvector, convert bitvector to integer
        // In the C implementation, t_loc() returns t_bits(CHAR_BIT * sizeof(uintptr_t))
        // So we can directly convert the bitvector to integer using bv2int
        sexp_t* args[] = {smt_term};
        return sexp_app_str("bv2int", args, 1);
      } else if (source_bt.tag == CN_BASE_INTEGER && target_bt.tag == CN_BASE_LOC) {
        // Integer -> Loc: Convert integer to bitvector
        // LOC is represented as a bitvector of width CHAR_BIT * sizeof(uintptr_t)
        // Use (_ int2bv n) to convert integer to n-bit bitvector
        int ptr_width = CHAR_BIT * sizeof(uintptr_t);
        char int2bv_op[64];
        snprintf(int2bv_op, sizeof(int2bv_op), "(_ int2bv %d)", ptr_width);
        sexp_t* args[] = {smt_term};
        return sexp_app_str(int2bv_op, args, 1);
      } else if (source_bt.tag == CN_BASE_INTEGER && target_bt.tag == CN_BASE_BITS) {
        // Integer -> Bits: Convert integer to bitvector of target width
        int target_width = target_bt.data.bits.size_bits;
        char int2bv_op[64];
        snprintf(int2bv_op, sizeof(int2bv_op), "(_ int2bv %d)", target_width);
        sexp_t* args[] = {smt_term};
        return sexp_app_str(int2bv_op, args, 1);
      } else {
        // Other casts
        assert(false);
        return smt_term;
      }
    }

      // Placeholder for additional cases still to be implemented:
      // - EachI loops
      // - Tuple operations
      // - Struct operations
      // - List operations
      // - Map operations
      // - Pattern matching
      // - Option types
      // And many more cases from the original OCaml code

    case CN_TERM_EACHI: {
      // Universal quantification - for now, return a default boolean
      // TODO: Implement proper quantification
      return bool_k(true);
    }

    case CN_TERM_STRUCT: {
      // Struct construction - create constructor function call
      const char* tag = iterm->data.struct_val.tag;

      // Create constructor name using struct_con_name
      char* con_name = struct_con_name(tag);

      // Get member count from vector
      bennet_vector(cn_member_pair)* members = &iterm->data.struct_val.members;
      size_t arg_count = bennet_vector_size(cn_member_pair)(members);

      // Allocate array for translated member arguments
      sexp_t** args = NULL;
      if (arg_count > 0) {
        args = malloc(sizeof(sexp_t*) * arg_count);

        // Translate each member term
        for (size_t i = 0; i < arg_count; i++) {
          cn_member_pair* pair = bennet_vector_get(cn_member_pair)(members, i);
          cn_term* member_term = pair->value;
          args[i] = translate_term(s, member_term);
        }
      }

      // Create function application
      sexp_t* fn_atom = sexp_atom(con_name);
      sexp_t* result = sexp_app(fn_atom, args, arg_count);

      // Cleanup
      free(con_name);
      if (args) {
        free(args);
      }

      return result;
    }

    case CN_TERM_STRUCT_MEMBER: {
      // Struct member access
      sexp_t* struct_smt = translate_term(s, iterm->data.struct_member.struct_term);
      const char* member = iterm->data.struct_member.member_name;

      char* fn_name = struct_field_name(member);

      sexp_t* fn_atom = sexp_atom(fn_name);
      sexp_t* args[] = {struct_smt};
      sexp_t* result = sexp_app(fn_atom, args, 1);

      free(fn_name);
      return result;
    }

    case CN_TERM_STRUCT_UPDATE: {
      // Struct update - create uninterpreted function call
      sexp_t* struct_smt = translate_term(s, iterm->data.struct_update.struct_term);
      sexp_t* value_smt = translate_term(s, iterm->data.struct_update.new_value);
      const char* member = iterm->data.struct_update.member_name;

      char* fn_name = malloc(strlen(member) + 20);
      assert(fn_name);
      sprintf(fn_name, "set_%s", member);

      sexp_t* fn_atom = sexp_atom(fn_name);
      sexp_t* args[] = {struct_smt, value_smt};
      sexp_t* result = sexp_app(fn_atom, args, 2);

      free(fn_name);
      return result;
    }

    case CN_TERM_RECORD: {
      // Record construction - build tuple in canonical type order
      cn_base_type record_bt = iterm->base_type;
      assert(record_bt.tag == CN_BASE_RECORD);

      size_t member_count = record_bt.data.record.count;
      bennet_vector(cn_member_pair)* members = &iterm->data.record.members;

      // Allocate array for translated member arguments in type order
      sexp_t** args = NULL;
      if (member_count > 0) {
        args = malloc(sizeof(sexp_t*) * member_count);
        assert(args);

        // For each member in the base type (in canonical order), find its value
        for (size_t i = 0; i < member_count; i++) {
          const char* type_member_name = record_bt.data.record.names[i];

          // Find this member in the members vector
          bool found = false;
          for (size_t j = 0; j < bennet_vector_size(cn_member_pair)(members); j++) {
            cn_member_pair* pair = bennet_vector_get(cn_member_pair)(members, j);
            if (strcmp(pair->name, type_member_name) == 0) {
              args[i] = translate_term(s, pair->value);
              found = true;
              break;
            }
          }
          assert(found);  // All members should be present
        }
      }

      // Compute element types for type annotation
      sexp_t** element_types = NULL;
      if (member_count > 0) {
        element_types = malloc(sizeof(sexp_t*) * member_count);
        assert(element_types);

        for (size_t i = 0; i < member_count; i++) {
          element_types[i] = translate_base_type(record_bt.data.record.types[i]);
          assert(element_types[i]);
        }
      }

      // Create the tuple type for annotation
      sexp_t* tuple_type = cn_tuple_type_name(element_types, member_count);
      assert(tuple_type);

      // Create type-annotated tuple constructor
      char* tuple_con_name = cn_tuple_constructor_name(member_count);
      sexp_t* constructor_atom = sexp_atom(tuple_con_name);
      sexp_t* typed_constructor = sexp_as_type(constructor_atom, tuple_type);

      // Apply to values
      sexp_t* result = sexp_app(typed_constructor, args, member_count);

      // Cleanup
      free(tuple_con_name);
      if (element_types) {
        for (size_t i = 0; i < member_count; i++) {
        }
        free(element_types);
      }
      if (args) {
        free(args);
      }

      return result;
    }

    case CN_TERM_RECORD_MEMBER: {
      // Record member access - use tuple selector
      sexp_t* record_smt = translate_term(s, iterm->data.record_member.record_term);
      const char* member = iterm->data.record_member.member_name;

      // Get the record's base type to find member index
      cn_base_type record_bt = iterm->data.record_member.record_term->base_type;
      assert(record_bt.tag == CN_BASE_RECORD);

      // Find the index of the member in the record type
      size_t member_index = 0;
      bool found = false;
      for (size_t i = 0; i < record_bt.data.record.count; i++) {
        if (strcmp(record_bt.data.record.names[i], member) == 0) {
          member_index = i;
          found = true;
          break;
        }
      }
      assert(found);  // Member should exist in the record type

      // Get the tuple selector name (e.g., "cn_get_0_of_2")
      char* selector_name =
          cn_tuple_get_selector_name(record_bt.data.record.count, member_index);

      sexp_t* fn_atom = sexp_atom(selector_name);
      sexp_t* args[] = {record_smt};
      sexp_t* result = sexp_app(fn_atom, args, 1);

      free(selector_name);
      return result;
    }

    case CN_TERM_CONSTRUCTOR: {
      // Constructor call with arguments
      const char* ctor = iterm->data.constructor.constructor_name;

      // Add _con suffix to constructor name
      size_t ctor_len = strlen(ctor);
      char* ctor_smt = malloc(ctor_len + 5);  // +4 for "_con" +1 for null
      assert(ctor_smt);
      sprintf(ctor_smt, "%s_con", ctor);

      // Get the arguments vector
      bennet_vector(cn_member_pair)* args_vec = &iterm->data.constructor.args;
      size_t arg_count = bennet_vector_size(cn_member_pair)(args_vec);

      if (arg_count == 0) {
        // Nullary constructor
        sexp_t* fn_atom = sexp_atom(ctor_smt);
        sexp_t* result = sexp_app(fn_atom, NULL, 0);
        free(ctor_smt);
        return result;
      }

      // Translate arguments in the order they are provided
      sexp_t** arg_sexps = malloc(sizeof(sexp_t*) * arg_count);
      assert(arg_sexps);
      for (size_t i = 0; i < arg_count; i++) {
        cn_member_pair* pair = bennet_vector_get(cn_member_pair)(args_vec, i);
        arg_sexps[i] = translate_term(s, pair->value);
        assert(arg_sexps[i]);
      }

      // Build constructor application
      sexp_t* fn_atom = sexp_atom(ctor_smt);
      sexp_t* result = sexp_app(fn_atom, arg_sexps, arg_count);

      // Cleanup
      for (size_t i = 0; i < arg_count; i++) {
      }
      free(arg_sexps);
      free(ctor_smt);

      return result;
    }

    case CN_TERM_WRAPI: {
      assert(false);
    }

    case CN_TERM_MAP_SET: {
      // Map set operation - use arr_store
      sexp_t* map_smt = translate_term(s, iterm->data.map_set.map);
      sexp_t* key_smt = translate_term(s, iterm->data.map_set.key);
      sexp_t* value_smt = translate_term(s, iterm->data.map_set.value);

      return arr_store(map_smt, key_smt, value_smt);
    }

    case CN_TERM_MAP_GET: {
      // Map get operation - use arr_select
      sexp_t* map_smt = translate_term(s, iterm->data.map_get.map);
      sexp_t* key_smt = translate_term(s, iterm->data.map_get.key);

      return arr_select(map_smt, key_smt);
    }

    case CN_TERM_APPLY: {
      // Function application
      const char* fn_name = iterm->data.apply.function_name;

      // Append _func suffix to get SMT name
      size_t len = strlen(fn_name) + 6;  // +5 for "_func" +1 for null
      char* smt_fn_name = malloc(len);
      assert(smt_fn_name);
      sprintf(smt_fn_name, "%s_func", fn_name);

      sexp_t* fn_atom = sexp_atom(smt_fn_name);
      free(smt_fn_name);

      // Get argument count from vector
      bennet_vector(cn_term_ptr)* args_vec = &iterm->data.apply.args;
      size_t arg_count = bennet_vector_size(cn_term_ptr)(args_vec);

      // Allocate array for translated arguments
      sexp_t** args = NULL;
      if (arg_count > 0) {
        args = malloc(sizeof(sexp_t*) * arg_count);
        assert(args);

        // Translate each argument term
        for (size_t i = 0; i < arg_count; i++) {
          cn_term* arg_term = *bennet_vector_get(cn_term_ptr)(args_vec, i);
          args[i] = translate_term(s, arg_term);
        }
      }

      // Create function application
      sexp_t* result = sexp_app(fn_atom, args, arg_count);

      // Cleanup
      if (args) {
        free(args);
      }

      return result;
    }

    case CN_TERM_LET: {
      // Substitute the variable with its value in the body, then translate

      // Create a substitution table
      bennet_hash_table(uint64_t, cn_term_ptr)* subst_table = cn_create_subst_table();
      assert(subst_table);

      // Add the substitution from variable ID to the value term
      cn_add_substitution(subst_table, iterm->data.let.var.id, iterm->data.let.value);

      // Perform substitution on the body
      cn_term* substituted_body = cn_subst_term(iterm->data.let.body, subst_table);
      assert(substituted_body);

      // Translate the substituted body
      sexp_t* result = translate_term(s, substituted_body);

      // Cleanup
      cn_free_subst_table(subst_table);

      return result;
    }

    case CN_TERM_MATCH: {
      // Pattern matching - translate to SMT-LIB match expression

      // Translate scrutinee
      sexp_t* scrutinee_smt = translate_term(s, iterm->data.match_data.scrutinee);
      assert(scrutinee_smt);

      // Get cases vector
      bennet_vector(cn_match_case)* cases_vec = &iterm->data.match_data.cases;
      size_t case_count = bennet_vector_size(cn_match_case)(cases_vec);

      // Handle empty match (shouldn't happen, but be safe)
      if (case_count == 0) {
        return bool_k(true);
      }

      // Allocate alternatives array
      match_alt_t* alternatives = malloc(sizeof(match_alt_t) * case_count);
      assert(alternatives);

      // Track allocated strings for cleanup
      char*** var_names_arrays = malloc(sizeof(char**) * case_count);
      assert(var_names_arrays);

      // Build each alternative
      for (size_t i = 0; i < case_count; i++) {
        cn_match_case* match_case = bennet_vector_get(cn_match_case)(cases_vec, i);

        // Determine pattern type and build pattern
        if (match_case->constructor_tag == NULL) {
          // Variable or wildcard pattern (PAT_VAR)
          alternatives[i].pattern.type = PAT_VAR;

          if (match_case->pattern_var_count == 1 &&
              match_case->pattern_vars[0].name != NULL) {
            // Single variable binding
            char* var_name = fn_name(match_case->pattern_vars[0]);
            assert(var_name);
            alternatives[i].pattern.data.var_name = var_name;

            // Track for cleanup (1 variable)
            var_names_arrays[i] = malloc(sizeof(char*));
            var_names_arrays[i][0] = var_name;
          } else {
            // Wildcard pattern
            alternatives[i].pattern.data.var_name = "_";
            var_names_arrays[i] = NULL;  // No cleanup needed for constant
          }
        } else {
          // Constructor pattern (PAT_CON)
          alternatives[i].pattern.type = PAT_CON;

          // Add _con suffix to match datatype constructor naming convention
          size_t tag_len = strlen(match_case->constructor_tag);
          char* con_name = malloc(tag_len + 5);  // "_con" + null terminator
          assert(con_name);
          sprintf(con_name, "%s_con", match_case->constructor_tag);
          alternatives[i].pattern.data.con.con_name = con_name;

          alternatives[i].pattern.data.con.var_count = match_case->pattern_var_count;

          // Convert pattern variables to name strings
          if (match_case->pattern_var_count > 0) {
            const char** var_names =
                malloc(sizeof(const char*) * match_case->pattern_var_count);
            assert(var_names);

            for (size_t j = 0; j < match_case->pattern_var_count; j++) {
              if (match_case->pattern_vars[j].name != NULL) {
                // Named variable binding
                var_names[j] = fn_name(match_case->pattern_vars[j]);
                assert(var_names[j]);
              } else {
                // Wildcard in constructor pattern
                var_names[j] = "_";
              }
            }

            alternatives[i].pattern.data.con.var_names = var_names;
            var_names_arrays[i] = (char**)var_names;
          } else {
            // Constructor with no arguments
            alternatives[i].pattern.data.con.var_names = NULL;
            var_names_arrays[i] = NULL;
          }
        }

        // Translate body term
        alternatives[i].expr = translate_term(s, match_case->body_term);
        assert(alternatives[i].expr);
      }

      // Generate SMT match expression
      sexp_t* result = match_datatype(scrutinee_smt, alternatives, case_count);
      assert(result);

      // Clean up allocated memory

      for (size_t i = 0; i < case_count; i++) {
        if (var_names_arrays[i] != NULL) {
          cn_match_case* match_case = bennet_vector_get(cn_match_case)(cases_vec, i);

          if (alternatives[i].pattern.type == PAT_VAR) {
            // Free single variable name (not wildcard)
            free(var_names_arrays[i][0]);
            free(var_names_arrays[i]);
          } else {
            // PAT_CON - free constructor variable names and constructor name
            for (size_t j = 0; j < match_case->pattern_var_count; j++) {
              // Only free if it was allocated (not wildcard "_")
              if (match_case->pattern_vars[j].name != NULL) {
                free((char*)alternatives[i].pattern.data.con.var_names[j]);
              }
            }
            free((char**)alternatives[i].pattern.data.con.var_names);

            // Free the allocated constructor name (with _con suffix)
            free((char*)alternatives[i].pattern.data.con.con_name);
          }
        }
      }

      free(var_names_arrays);
      free(alternatives);

      return result;
    }

    default:
      // Return a default/error case
      fprintf(stderr, "ERROR: Unhandled term type %d in translate_term\n", iterm->type);
      assert(false);
      return NULL;
  }
}

///////////////////////////
/* OCaml function conversions */
///////////////////////////

// cn_arg_binder is now defined in the header file

// Simplified wrapper function (now cn_base_type has all the detail we need)
static sexp_t* translate_cn_base_type(cn_base_type bt) {
  return translate_base_type(bt);
}

/* let declare_fun s name args_bts res_bt =
  let sname = CN_Names.fn_name name in
  let args_ts = List.map translate_base_type args_bts in
  let res_t = translate_base_type res_bt in
  ack_command s (SMT.declare_fun sname args_ts res_t) */

void cn_declare_fun(struct cn_smt_solver* s,
    cn_sym name,
    cn_base_type* args_bts,
    size_t args_count,
    cn_base_type res_bt) {
  char* sname = fn_name(name);
  if (!sname)
    return;

  // Translate argument base types
  sexp_t** args_ts = NULL;
  if (args_count > 0) {
    args_ts = malloc(sizeof(sexp_t*) * args_count);
    if (!args_ts) {
      free(sname);
      return;
    }
  }

  for (size_t i = 0; i < args_count; i++) {
    args_ts[i] = translate_cn_base_type(args_bts[i]);

    if (!args_ts[i]) {
      // Cleanup on failure
      for (size_t j = 0; j < i; j++) {
      }
      free(args_ts);
      free(sname);
      return;
    }
  }

  // Translate result type
  sexp_t* res_t = translate_cn_base_type(res_bt);

  if (!res_t) {
    for (size_t i = 0; i < args_count; i++) {
    }
    free(args_ts);
    free(sname);
    return;
  }

  // Create SMT declare_fun command
  sexp_t* decl_cmd = declare_fun(sname, args_ts, args_count, res_t);
  if (decl_cmd) {
    // Send command
    ack_command(s, decl_cmd);
  }

  // Cleanup
  for (size_t i = 0; i < args_count; i++) {
  }
  free(args_ts);
  free(sname);
}

/* let define_fun s name arg_binders res_bt body =
  let sname = CN_Names.fn_name name in
  let mk_arg (sym, bt) = (CN_Names.fn_name sym, translate_base_type bt) in
  let args = List.map mk_arg arg_binders in
  let ret_t = translate_base_type res_bt in
  ack_command s (SMT.define_fun sname args ret_t (translate_term s body)) */

/* let declare_variable solver (sym, bt) = declare_fun solver sym [] bt */

void cn_declare_variable(struct cn_smt_solver* solver, cn_sym sym, cn_base_type bt) {
  cn_declare_fun(solver, sym, NULL, 0, bt);
}
