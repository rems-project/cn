#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/context.h>
#include <cn-smt/subst.h>  // for bennet_optional_cn_term_ptr
#include <cn-smt/terms.h>

// Generate vector implementations
BENNET_VECTOR_IMPL(const_char_ptr)
BENNET_VECTOR_IMPL(cn_term_ptr)
BENNET_VECTOR_IMPL(cn_member_pair)

// Generate hash table implementation
BENNET_HASH_TABLE_IMPL(const_char_ptr, cn_term_ptr)

#define DEFINE_CN_SMT_LOGICAL_UNOP(name, op_enum)                                        \
  cn_term* cn_smt_##name(cn_term* operand) {                                             \
    assert(operand);                                                                     \
    cn_term* term = cn_term_alloc(CN_TERM_UNOP, cn_base_type_simple(CN_BASE_BOOL));      \
    assert(term);                                                                        \
    term->data.unop.op = op_enum;                                                        \
    term->data.unop.operand = operand;                                                   \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_LOGICAL_BINOP(name, op_enum)                                       \
  cn_term* cn_smt_##name(cn_term* left, cn_term* right) {                                \
    assert(left&& right);                                                                \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, cn_base_type_simple(CN_BASE_BOOL));     \
    assert(term);                                                                        \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_ARITH_BINOP(name, op_enum)                                         \
  cn_term* cn_smt_##name(cn_term* left, cn_term* right) {                                \
    assert(left&& right);                                                                \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, left->base_type);                       \
    assert(term);                                                                        \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_BW_UNOP(name, op_enum)                                             \
  cn_term* cn_smt_##name(cn_term* operand) {                                             \
    assert(operand);                                                                     \
    cn_term* term = cn_term_alloc(CN_TERM_UNOP, operand->base_type);                     \
    assert(term);                                                                        \
    term->data.unop.op = op_enum;                                                        \
    term->data.unop.operand = operand;                                                   \
    return term;                                                                         \
  }

#define DEFINE_CN_SMT_BW_BINOP(name, op_enum)                                            \
  cn_term* cn_smt_##name(cn_term* left, cn_term* right) {                                \
    assert(left&& right);                                                                \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, left->base_type);                       \
    assert(term);                                                                        \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
    return term;                                                                         \
  }

// Helper function to allocate and initialize a new term
cn_term* cn_term_alloc(cn_term_type type, cn_base_type base_type) {
  cn_term* term = malloc(sizeof(cn_term));
  assert(term);

  term->type = type;
  term->base_type = base_type;
  return term;
}

// Basic constant constructors
cn_term* cn_smt_z(int64_t value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_INTEGER));
  assert(term);

  term->data.const_val.type = CN_CONST_Z;
  term->data.const_val.data.z = value;
  return term;
}

cn_term* cn_smt_bool(bool value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_BOOL));
  assert(term);

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
  assert(sym.name);

  cn_term* term = cn_term_alloc(CN_TERM_SYM, type);
  assert(term);

  term->data.sym = sym;

  return term;
}

cn_term* cn_smt_null(void) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_LOC));
  assert(term);

  term->data.const_val.type = CN_CONST_NULL;
  return term;
}

cn_term* cn_smt_unit(void) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_UNIT));
  assert(term);

  term->data.const_val.type = CN_CONST_UNIT;
  return term;
}

cn_term* cn_smt_bits(bool is_signed, int size_bits, intmax_t value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_bits(is_signed, size_bits));
  assert(term);

  term->data.const_val.type = CN_CONST_BITS;
  term->data.const_val.data.bits.info.is_signed = is_signed;
  term->data.const_val.data.bits.info.size_bits = size_bits;
  term->data.const_val.data.bits.value = value;
  return term;
}

cn_term* cn_smt_loc(uintptr_t value) {
  return cn_smt_bits(false, CHAR_BIT * sizeof(uintptr_t), value);
}

cn_term* cn_smt_num(cn_base_type bt, intmax_t value) {
  if (bt.tag == CN_BASE_BITS) {
    return cn_smt_bits(bt.data.bits.is_signed, bt.data.bits.size_bits, value);
  } else if (bt.tag == CN_BASE_LOC) {
    return cn_smt_loc(value);
  } else if (bt.tag == CN_BASE_INTEGER) {
    return cn_smt_z(value);
  }

  assert(false);
  return NULL;
}

cn_term* cn_smt_rational(double value) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_REAL));
  assert(term);

  term->data.const_val.type = CN_CONST_Q;
  term->data.const_val.data.q = value;
  return term;
}

cn_term* cn_smt_pointer(uintptr_t ptr) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, cn_base_type_simple(CN_BASE_LOC));
  assert(term);

  term->data.const_val.type = CN_CONST_POINTER;
  term->data.const_val.data.pointer = ptr;
  return term;
}

cn_term* cn_smt_default(cn_base_type type) {
  cn_term* term = cn_term_alloc(CN_TERM_CONST, type);
  assert(term);

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
  assert(cond && then_term && else_term);

  // Result type should match the then/else branches
  cn_term* term = cn_term_alloc(CN_TERM_ITE, then_term->base_type);
  assert(term);

  term->data.ite.cond = cond;
  term->data.ite.then_term = then_term;
  term->data.ite.else_term = else_term;
  return term;
}

// Pointer operations
cn_term* cn_smt_member_shift(cn_term* base, size_t offset) {
  assert(base);

  cn_term* term = cn_term_alloc(CN_TERM_MEMBER_SHIFT, cn_base_type_simple(CN_BASE_LOC));
  assert(term);

  term->data.member_shift.base = base;
  term->data.member_shift.offset = offset;

  return term;
}

cn_term* cn_smt_array_shift(cn_term* base, size_t element_size, cn_term* index) {
  assert(base && index);

  cn_term* term = cn_term_alloc(CN_TERM_ARRAY_SHIFT, cn_base_type_simple(CN_BASE_LOC));
  assert(term);

  term->data.array_shift.base = base;
  term->data.array_shift.element_size = element_size;
  term->data.array_shift.index = index;

  return term;
}

// Type operations
cn_term* cn_smt_cast(cn_base_type target_type, cn_term* value) {
  assert(value);

  cn_term* term = cn_term_alloc(CN_TERM_CAST, target_type);
  assert(term);

  term->data.cast.target_type = target_type;
  term->data.cast.value = value;
  return term;
}

cn_term* cn_smt_map_get(cn_term* map, cn_term* key, cn_base_type result_type) {
  assert(map && key);

  cn_term* term = cn_term_alloc(CN_TERM_MAP_GET, result_type);
  assert(term);

  term->data.map_get.map = map;
  term->data.map_get.key = key;
  return term;
}

cn_term* cn_smt_map_set(cn_term* map, cn_term* key, cn_term* value) {
  assert(map && key && value);

  cn_term* term = cn_term_alloc(CN_TERM_MAP_SET, cn_base_type_simple(CN_BASE_MAP));
  assert(term);

  term->data.map_set.map = map;
  term->data.map_set.key = key;
  term->data.map_set.value = value;
  return term;
}

// Function application
cn_term* cn_smt_apply(const char* function_name,
    cn_base_type result_type,
    bennet_vector(cn_term_ptr) * args) {
  assert(function_name && args);

  cn_term* term = cn_term_alloc(CN_TERM_APPLY, result_type);
  assert(term);

  term->data.apply.function_name = strdup(function_name);
  assert(term->data.apply.function_name);

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
  assert(var_name && value && body);

  cn_term* term = cn_term_alloc(CN_TERM_LET, body->base_type);
  assert(term);

  term->data.let.var_name = strdup(var_name);
  term->data.let.value = value;
  term->data.let.body = body;

  assert(term->data.let.var_name);

  return term;
}

// Struct operations
cn_term* cn_smt_struct(const char* tag,
    size_t member_count,
    const char** member_names,
    cn_term** member_values) {
  assert(tag && member_names && member_values);

  cn_term* term = cn_term_alloc(CN_TERM_STRUCT, cn_base_type_struct(tag));
  assert(term);

  term->data.struct_val.tag = strdup(tag);
  assert(term->data.struct_val.tag);

  // Initialize the vector
  bennet_vector_init(cn_member_pair)(&term->data.struct_val.members);

  // Add each member to the vector
  for (size_t i = 0; i < member_count; i++) {
    assert(member_names[i] && member_values[i]);

    const char* name_copy = strdup(member_names[i]);
    assert(name_copy);

    cn_member_pair pair = {.name = name_copy, .value = member_values[i]};
    bennet_vector_push(cn_member_pair)(&term->data.struct_val.members, pair);
  }

  return term;
}

cn_term* cn_smt_struct_member(
    cn_term* struct_term, const char* member_name, cn_base_type member_type) {
  assert(struct_term && member_name);

  assert(struct_term->base_type.tag == CN_BASE_STRUCT);

  cn_term* term = cn_term_alloc(CN_TERM_STRUCT_MEMBER, member_type);
  assert(term);

  term->data.struct_member.struct_term = struct_term;
  term->data.struct_member.member_name = strdup(member_name);

  assert(term->data.struct_member.member_name);

  return term;
}

cn_term* cn_smt_struct_update(
    cn_term* struct_term, const char* member_name, cn_term* new_value) {
  assert(struct_term && member_name && new_value);

  cn_term* term = cn_term_alloc(CN_TERM_STRUCT_UPDATE, struct_term->base_type);
  assert(term);

  term->data.struct_update.struct_term = struct_term;
  term->data.struct_update.member_name = strdup(member_name);
  term->data.struct_update.new_value = new_value;

  assert(term->data.struct_update.member_name);

  return term;
}

// Record operations
cn_term* cn_smt_record(
    size_t member_count, const char** member_names, cn_term** member_values) {
  assert(member_names && member_values);

  // Create a record base type with the field names and inferred types
  // Extract base types from the member values
  cn_base_type* field_types = malloc(member_count * sizeof(cn_base_type));
  for (size_t i = 0; i < member_count; i++) {
    field_types[i] = member_values[i]->base_type;
  }
  cn_base_type record_bt = create_record_type(member_count, member_names, field_types);

  cn_term* term = cn_term_alloc(CN_TERM_RECORD, record_bt);
  assert(term);

  // Initialize the vector
  bennet_vector_init(cn_member_pair)(&term->data.record.members);

  // Add each member to the vector
  for (size_t i = 0; i < member_count; i++) {
    assert(member_names[i] && member_values[i]);

    const char* name_copy = strdup(member_names[i]);
    assert(name_copy);

    cn_member_pair pair = {.name = name_copy, .value = member_values[i]};
    bennet_vector_push(cn_member_pair)(&term->data.record.members, pair);
  }

  return term;
}

cn_term* cn_smt_record_member(cn_term* record_term, const char* member_name) {
  assert(record_term && member_name);

  // Find the member type from the record (simplified - just use the base type of the record)
  cn_base_type member_type = cn_base_type_simple(CN_BASE_INTEGER);  // Default fallback

  // Try to look up the actual member in the record if it's a record term
  if (record_term->type == CN_TERM_RECORD) {
    // Search for member in vector
    bennet_vector(cn_member_pair)* members = &record_term->data.record.members;
    for (size_t i = 0; i < bennet_vector_size(cn_member_pair)(members); i++) {
      cn_member_pair* pair = bennet_vector_get(cn_member_pair)(members, i);
      if (strcmp(pair->name, member_name) == 0) {
        member_type = pair->value->base_type;
        break;
      }
    }
  }

  cn_term* term = cn_term_alloc(CN_TERM_RECORD_MEMBER, member_type);
  assert(term);

  term->data.record_member.record_term = record_term;
  term->data.record_member.member_name = strdup(member_name);

  assert(term->data.record_member.member_name);

  return term;
}

cn_term* cn_smt_record_update(
    cn_term* record_term, const char* member_name, cn_term* new_value) {
  assert(record_term && member_name && new_value);

  cn_term* term = cn_term_alloc(CN_TERM_RECORD_UPDATE, record_term->base_type);
  assert(term);

  term->data.record_update.record_term = record_term;
  term->data.record_update.member_name = strdup(member_name);
  term->data.record_update.new_value = new_value;

  assert(term->data.record_update.member_name);

  return term;
}

cn_term* cn_smt_constructor(const char* constructor_name,
    size_t arg_count,
    const char** arg_names,
    cn_term** arg_values) {
  assert(constructor_name);

  // Create the constructor term with a default datatype base type
  cn_base_type constructor_type =
      cn_base_type_simple(CN_BASE_INTEGER);  // Default - should be improved
  cn_term* term = cn_term_alloc(CN_TERM_CONSTRUCTOR, constructor_type);
  assert(term);

  term->data.constructor.constructor_name = strdup(constructor_name);
  assert(term->data.constructor.constructor_name);

  // Initialize the arguments vector
  bennet_vector_init(cn_member_pair)(&term->data.constructor.args);

  // Add arguments to the vector
  if (arg_names && arg_values) {
    for (size_t i = 0; i < arg_count; i++) {
      assert(arg_names[i] && arg_values[i]);

      char* name_copy = strdup(arg_names[i]);
      assert(name_copy);

      cn_member_pair pair = {.name = name_copy, .value = arg_values[i]};
      bennet_vector_push(cn_member_pair)(&term->data.constructor.args, pair);
    }
  }

  return term;
}

char* cn_term_to_string(cn_term* term) {
  return "";
}
