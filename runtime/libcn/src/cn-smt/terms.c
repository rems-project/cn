#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/context.h>
#include <cn-smt/subst.h>  // for bennet_optional_cn_term_ptr
#include <cn-smt/terms.h>

// Generate vector implementations
BENNET_VECTOR_IMPL(const_char_ptr)
BENNET_VECTOR_IMPL(cn_term_ptr)

// Generate hash table implementation
BENNET_HASH_TABLE_IMPL(const_char_ptr, cn_term_ptr)

#define DEFINE_CN_SMT_LOGICAL_UNOP(name, op_enum)                                        \
  cn_term* cn_smt_##name(cn_term* operand) {                                             \
    if (!operand)                                                                        \
      return NULL;                                                                       \
    cn_term* term = cn_term_alloc(CN_TERM_UNOP, cn_base_type_simple(CN_BASE_BOOL));      \
    if (!term)                                                                           \
      return NULL;                                                                       \
    term->data.unop.op = op_enum;                                                        \
    term->data.unop.operand = operand;                                                   \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_LOGICAL_BINOP(name, op_enum)                                       \
  cn_term* cn_smt_##name(cn_term* left, cn_term* right) {                                \
    if (!left || !right)                                                                 \
      return NULL;                                                                       \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, cn_base_type_simple(CN_BASE_BOOL));     \
    if (!term)                                                                           \
      return NULL;                                                                       \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_ARITH_BINOP(name, op_enum)                                         \
  cn_term* cn_smt_##name(cn_term* left, cn_term* right) {                                \
    if (!left || !right)                                                                 \
      return NULL;                                                                       \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, left->base_type);                       \
    if (!term)                                                                           \
      return NULL;                                                                       \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_BW_UNOP(name, op_enum)                                             \
  cn_term* cn_smt_##name(cn_term* operand) {                                             \
    if (!operand)                                                                        \
      return NULL;                                                                       \
    cn_term* term = cn_term_alloc(CN_TERM_UNOP, operand->base_type);                     \
    if (!term)                                                                           \
      return NULL;                                                                       \
    term->data.unop.op = op_enum;                                                        \
    term->data.unop.operand = operand;                                                   \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_BW_BINOP(name, op_enum)                                            \
  cn_term* cn_smt_##name(cn_term* left, cn_term* right) {                                \
    if (!left || !right)                                                                 \
      return NULL;                                                                       \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, left->base_type);                       \
    if (!term)                                                                           \
      return NULL;                                                                       \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
    return term;                                                                         \
  }

// Helper function to allocate and initialize a new term
cn_term* cn_term_alloc(cn_term_type type, cn_base_type base_type) {
  cn_term* term = malloc(sizeof(cn_term));
  if (!term)
    return NULL;

  term->type = type;
  term->base_type = base_type;
  return term;
}

// Basic constant constructors
cn_term* cn_smt_z(int64_t value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_INTEGER));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_Z;
  term->data.const_val.data.z = value;
  return term;
}

cn_term* cn_smt_bool(bool value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_BOOL));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_BOOL;
  term->data.const_val.data.boolean = value;
  return term;
}

// Global counter for unique symbol IDs
static uint64_t next_sym_id = 0;

// Reset the symbol counter to start fresh
void reset_cn_sym_counter(void) {
  next_sym_id = 0;
}

cn_term* cn_smt_sym(cn_sym sym, cn_base_type type) {
  if (!sym.name)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_SYM, type);
  if (!term)
    return NULL;

  term->data.sym = sym;

  return term;
}

cn_term* cn_smt_null(void) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_LOC));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_NULL;
  return term;
}

cn_term* cn_smt_unit(void) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_UNIT));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_UNIT;
  return term;
}

cn_term* cn_smt_bits(bool is_signed, int size_bits, intmax_t value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_bits(is_signed, size_bits));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_BITS;
  term->data.const_val.data.bits.info.is_signed = is_signed;
  term->data.const_val.data.bits.info.size_bits = size_bits;
  term->data.const_val.data.bits.value = value;
  return term;
}

cn_term* cn_smt_num(cn_base_type bt, intmax_t value) {
  if (bt.tag == CN_BASE_BITS) {
    return cn_smt_bits(bt.data.bits.is_signed, bt.data.bits.size_bits, value);
  } else if (bt.tag == CN_BASE_LOC) {
    return cn_smt_bits(false, CHAR_BIT * sizeof(uintptr_t), value);
  } else if (bt.tag == CN_BASE_INTEGER) {
    return cn_smt_z(value);
  }

  assert(false);
  return NULL;
}

cn_term* cn_smt_rational(double value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_REAL));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_Q;
  term->data.const_val.data.q = value;
  return term;
}

cn_term* cn_smt_pointer(uintptr_t ptr) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_LOC));
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_POINTER;
  term->data.const_val.data.pointer = ptr;
  return term;
}

cn_term* cn_smt_default(cn_base_type type) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, type);
  if (!term)
    return NULL;

  term->data.const_val.type = CN_CONST_DEFAULT;
  term->data.const_val.data.default_type = type;
  return term;
}

// Logical operators
DEFINE_CN_SMT_LOGICAL_UNOP(not, CN_UNOP_NOT);
DEFINE_CN_SMT_LOGICAL_BINOP(and, CN_BINOP_AND);
DEFINE_CN_SMT_LOGICAL_BINOP(or, CN_BINOP_OR);
DEFINE_CN_SMT_LOGICAL_BINOP(implies, CN_BINOP_IMPLIES);

// Comparison operators
DEFINE_CN_SMT_LOGICAL_BINOP(eq, CN_BINOP_EQ);
DEFINE_CN_SMT_LOGICAL_BINOP(lt, CN_BINOP_LT);
DEFINE_CN_SMT_LOGICAL_BINOP(le, CN_BINOP_LE);

cn_term* cn_smt_gt(cn_term* left, cn_term* right) {
  // gt(a, b) is equivalent to lt(b, a)
  return cn_smt_lt(right, left);
}

cn_term* cn_smt_ge(cn_term* left, cn_term* right) {
  // ge(a, b) is equivalent to le(b, a)
  return cn_smt_le(right, left);
}

// Arithmetic operators
DEFINE_CN_SMT_ARITH_BINOP(add, CN_BINOP_ADD);
DEFINE_CN_SMT_ARITH_BINOP(sub, CN_BINOP_SUB);
DEFINE_CN_SMT_ARITH_BINOP(mul, CN_BINOP_MUL);
DEFINE_CN_SMT_ARITH_BINOP(div, CN_BINOP_DIV);
DEFINE_CN_SMT_ARITH_BINOP(rem, CN_BINOP_REM);
DEFINE_CN_SMT_ARITH_BINOP(mod, CN_BINOP_MOD);
DEFINE_CN_SMT_ARITH_BINOP(min, CN_BINOP_MIN);
DEFINE_CN_SMT_ARITH_BINOP(max, CN_BINOP_MAX);

// Bitwise operators
DEFINE_CN_SMT_BW_BINOP(bw_and, CN_BINOP_BW_AND);
DEFINE_CN_SMT_BW_BINOP(bw_or, CN_BINOP_BW_OR);
DEFINE_CN_SMT_BW_BINOP(bw_xor, CN_BINOP_BW_XOR);
DEFINE_CN_SMT_BW_UNOP(bw_compl, CN_UNOP_BW_COMPL);
DEFINE_CN_SMT_BW_BINOP(shift_left, CN_BINOP_SHIFT_LEFT);
DEFINE_CN_SMT_BW_BINOP(shift_right, CN_BINOP_SHIFT_RIGHT);

// Control flow
cn_term* cn_smt_ite(cn_term* cond, cn_term* then_term, cn_term* else_term) {
  if (!cond || !then_term || !else_term)
    return NULL;

  // Result type should match the then/else branches
  cn_term* term = cn_term_alloc(CN_TERM_ITE, then_term->base_type);
  if (!term)
    return NULL;

  term->data.ite.cond = cond;
  term->data.ite.then_term = then_term;
  term->data.ite.else_term = else_term;
  return term;
}

// Pointer operations
cn_term* cn_smt_member_shift(cn_term* base, size_t offset) {
  if (!base)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_MEMBER_SHIFT, cn_base_type_simple(CN_BASE_LOC));
  if (!term)
    return NULL;

  term->data.member_shift.base = base;
  term->data.member_shift.offset = offset;

  return term;
}

cn_term* cn_smt_array_shift(cn_term* base, size_t element_size, cn_term* index) {
  if (!base || !index)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_ARRAY_SHIFT, cn_base_type_simple(CN_BASE_LOC));
  if (!term)
    return NULL;

  term->data.array_shift.base = base;
  term->data.array_shift.element_size = element_size;
  term->data.array_shift.index = index;

  return term;
}

// Type operations
cn_term* cn_smt_cast(cn_base_type target_type, cn_term* value) {
  if (!value)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_CAST, target_type);
  if (!term)
    return NULL;

  term->data.cast.target_type = target_type;
  term->data.cast.value = value;
  return term;
}

cn_term* cn_smt_map_get(cn_term* map, cn_term* key, cn_base_type result_type) {
  if (!map || !key)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_MAP_GET, result_type);
  if (!term)
    return NULL;

  term->data.map_get.map = map;
  term->data.map_get.key = key;
  return term;
}

cn_term* cn_smt_map_set(cn_term* map, cn_term* key, cn_term* value) {
  if (!map || !key || !value)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_MAP_SET, cn_base_type_simple(CN_BASE_MAP));
  if (!term)
    return NULL;

  term->data.map_set.map = map;
  term->data.map_set.key = key;
  term->data.map_set.value = value;
  return term;
}

// Function application
cn_term* cn_smt_apply(const char* function_name,
    cn_base_type result_type,
    bennet_vector(cn_term_ptr) * args) {
  if (!function_name || !args)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_APPLY, result_type);
  if (!term)
    return NULL;

  term->data.apply.function_name = strdup(function_name);
  if (!term->data.apply.function_name) {
    free(term);
    return NULL;
  }

  // Initialize the vector and copy from input vector
  bennet_vector_init(cn_term_ptr)(&term->data.apply.args);
  size_t arg_count = bennet_vector_size(cn_term_ptr)(args);

  for (size_t i = 0; i < arg_count; i++) {
    cn_term* arg = *bennet_vector_get(cn_term_ptr)(args, i);
    bennet_vector_push(cn_term_ptr)(&term->data.apply.args, arg);
  }

  return term;
}

// Let binding
cn_term* cn_smt_let(const char* var_name, cn_term* value, cn_term* body) {
  if (!var_name || !value || !body)
    return NULL;

  cn_term* term = cn_term_alloc(CN_TERM_LET, body->base_type);
  if (!term)
    return NULL;

  term->data.let.var_name = strdup(var_name);
  term->data.let.value = value;
  term->data.let.body = body;

  if (!term->data.let.var_name) {
    free(term);
    return NULL;
  }

  return term;
}

// Record operations
cn_term* cn_smt_record(
    size_t member_count, const char** member_names, cn_term** member_values) {
  if (!member_names || !member_values)
    return NULL;

  // Create a record base type with the field names and inferred types
  // Extract base types from the member values
  cn_base_type* field_types = malloc(member_count * sizeof(cn_base_type));
  for (size_t i = 0; i < member_count; i++) {
    field_types[i] = member_values[i]->base_type;
  }
  cn_base_type record_bt = create_record_type(member_count, member_names, field_types);

  cn_term* term = cn_term_alloc(CN_TERM_RECORD, record_bt);
  if (!term)
    return NULL;

  // Initialize the hash table with proper hash and equality functions
  bennet_hash_table_init(const_char_ptr, cn_term_ptr)(
      &term->data.record.members, bennet_hash_const_char_ptr, bennet_eq_const_char_ptr);

  // Add each member to the hash table
  for (size_t i = 0; i < member_count; i++) {
    if (!member_names[i] || !member_values[i]) {
      free(term);
      return NULL;
    }

    const char* name_copy = strdup(member_names[i]);
    if (!name_copy) {
      free(term);
      return NULL;
    }

    bennet_hash_table_set(const_char_ptr, cn_term_ptr)(
        &term->data.record.members, name_copy, member_values[i]);
  }

  return term;
}

cn_term* cn_smt_record_member(cn_term* record_term, const char* member_name) {
  if (!record_term || !member_name)
    return NULL;

  // Find the member type from the record (simplified - just use the base type of the record)
  cn_base_type member_type = cn_base_type_simple(CN_BASE_INTEGER);  // Default fallback

  // Try to look up the actual member in the record if it's a record term
  if (record_term->type == CN_TERM_RECORD) {
    bennet_optional(cn_term_ptr) found_member = bennet_hash_table_get(
        const_char_ptr, cn_term_ptr)(&record_term->data.record.members, member_name);
    if (bennet_optional_is_some(found_member)) {
      cn_term* member_term = bennet_optional_unwrap(found_member);
      if (member_term) {
        member_type = member_term->base_type;
      }
    }
  }

  cn_term* term = cn_term_alloc(CN_TERM_RECORD_MEMBER, member_type);
  if (!term)
    return NULL;

  term->data.record_member.record_term = record_term;
  term->data.record_member.member_name = strdup(member_name);

  if (!term->data.record_member.member_name) {
    free(term);
    return NULL;
  }

  return term;
}

char* cn_term_to_string(cn_term* term) {
  return "";
}
