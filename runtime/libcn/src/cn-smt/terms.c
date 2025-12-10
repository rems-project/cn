#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/bump_alloc.h>
#include <cn-smt/context.h>
#include <cn-smt/subst.h>  // for bennet_optional_cn_term_ptr
#include <cn-smt/terms.h>

// Generate vector implementations
BENNET_VECTOR_IMPL(const_char_ptr)
BENNET_VECTOR_IMPL(cn_term_ptr)
BENNET_VECTOR_IMPL(cn_member_pair)
BENNET_VECTOR_IMPL(cn_match_case)

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
    assert(cn_base_type_eq(left->base_type, right->base_type));                          \
                                                                                         \
    cn_term* term = cn_term_alloc(CN_TERM_BINOP, left->base_type);                       \
    assert(term);                                                                        \
                                                                                         \
    term->data.binop.op = op_enum;                                                       \
    term->data.binop.left = left;                                                        \
    term->data.binop.right = right;                                                      \
                                                                                         \
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
  cn_term* term = cn_bump_malloc(sizeof(cn_term));
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

cn_term* cn_smt_num(cn_base_type bt, intmax_t value) {
  if (bt.tag == CN_BASE_BITS) {
    return cn_smt_bits(bt.data.bits.is_signed, bt.data.bits.size_bits, value);
  } else if (bt.tag == CN_BASE_LOC) {
    return cn_smt_pointer(value);
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

cn_term* cn_smt_map_get(cn_term* map, cn_term* key) {
  assert(map && key);

  cn_term* term = cn_term_alloc(CN_TERM_MAP_GET, *map->base_type.data.map.value_type);
  assert(term);

  term->data.map_get.map = map;
  term->data.map_get.key = key;
  return term;
}

cn_term* cn_smt_map_set(cn_term* map, cn_term* key, cn_term* value) {
  assert(map && key && value);

  // Propagate the map type from the input map
  cn_term* term = cn_term_alloc(CN_TERM_MAP_SET, map->base_type);
  assert(term);

  term->data.map_set.map = map;
  term->data.map_set.key = key;
  term->data.map_set.value = value;
  return term;
}

// Function application
cn_term* cn_smt_apply(const char* function_name,
    cn_base_type result_type,
    cn_term** args,
    size_t arg_count) {
  assert(function_name);
  assert(args || arg_count == 0);

  cn_term* term = cn_term_alloc(CN_TERM_APPLY, result_type);
  assert(term);

  term->data.apply.function_name = function_name;

  // Initialize the vector and copy from input array
  bennet_vector_init(cn_term_ptr)(&term->data.apply.args);

  for (size_t i = 0; i < arg_count; i++) {
    bennet_vector_push(cn_term_ptr)(&term->data.apply.args, args[i]);
  }

  return term;
}

// Let binding
cn_term* cn_smt_let(cn_sym var, cn_term* value, cn_term* body) {
  assert(value && body);

  cn_term* term = cn_term_alloc(CN_TERM_LET, body->base_type);
  assert(term);

  term->data.let.var = var;
  term->data.let.value = value;
  term->data.let.body = body;

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

  term->data.struct_val.tag = tag;

  // Initialize the vector
  bennet_vector_init(cn_member_pair)(&term->data.struct_val.members);

  // Add each member to the vector
  for (size_t i = 0; i < member_count; i++) {
    assert(member_names[i] && member_values[i]);

    cn_member_pair pair = {.name = member_names[i], .value = member_values[i]};
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
  term->data.struct_member.member_name = member_name;

  return term;
}

cn_term* cn_smt_struct_update(
    cn_term* struct_term, const char* member_name, cn_term* new_value) {
  assert(struct_term && member_name && new_value);

  cn_term* term = cn_term_alloc(CN_TERM_STRUCT_UPDATE, struct_term->base_type);
  assert(term);

  term->data.struct_update.struct_term = struct_term;
  term->data.struct_update.member_name = member_name;
  term->data.struct_update.new_value = new_value;

  return term;
}

// Record operations
cn_term* cn_smt_record(
    size_t member_count, const char** member_names, cn_term** member_values) {
  assert(member_names && member_values);

  // Create a record base type with the field names and inferred types
  // Extract base types from the member values
  cn_base_type* field_types = cn_bump_malloc(member_count * sizeof(cn_base_type));
  assert(field_types);
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

    cn_member_pair pair = {.name = member_names[i], .value = member_values[i]};
    bennet_vector_push(cn_member_pair)(&term->data.record.members, pair);
  }

  return term;
}

cn_term* cn_smt_record_member(cn_term* record_term, const char* member_name) {
  assert(record_term && member_name);
  assert(record_term->base_type.tag == CN_BASE_RECORD);

  // Look up the member type from the record's base type
  size_t count = record_term->base_type.data.record.count;
  const char** names = record_term->base_type.data.record.names;
  cn_base_type* types = record_term->base_type.data.record.types;

  for (size_t i = 0; i < count; i++) {
    if (strcmp(names[i], member_name) == 0) {
      cn_base_type member_type = types[i];
      cn_term* term = cn_term_alloc(CN_TERM_RECORD_MEMBER, member_type);
      assert(term);

      term->data.record_member.record_term = record_term;
      term->data.record_member.member_name = member_name;

      return term;
    }
  }

  // Member not found - this is a bug
  fprintf(stderr,
      "ERROR: cn_smt_record_member could not find member '%s' in record\n",
      member_name);
  fprintf(stderr, "  Record has %zu members: ", count);
  for (size_t i = 0; i < count; i++) {
    fprintf(stderr, "%s (tag=%d)%s", names[i], types[i].tag, i < count - 1 ? ", " : "");
  }
  fprintf(stderr, "\n");
  fflush(stderr);
  assert(false && "Record member not found");
  return NULL;  // Unreachable
}

cn_term* cn_smt_record_update(
    cn_term* record_term, const char* member_name, cn_term* new_value) {
  assert(record_term && member_name && new_value);

  cn_term* term = cn_term_alloc(CN_TERM_RECORD_UPDATE, record_term->base_type);
  assert(term);

  term->data.record_update.record_term = record_term;
  term->data.record_update.member_name = member_name;
  term->data.record_update.new_value = new_value;

  return term;
}

cn_term* cn_smt_constructor(cn_base_type base_type,
    const char* constructor_name,
    size_t arg_count,
    const char** arg_names,
    cn_term** arg_values) {
  assert(constructor_name);

  // Create the constructor term with the provided base type
  cn_term* term = cn_term_alloc(CN_TERM_CONSTRUCTOR, base_type);
  assert(term);

  term->data.constructor.constructor_name = constructor_name;

  // Initialize the arguments vector
  bennet_vector_init(cn_member_pair)(&term->data.constructor.args);

  // Add arguments to the vector
  if (arg_names && arg_values) {
    for (size_t i = 0; i < arg_count; i++) {
      assert(arg_names[i] && arg_values[i]);

      cn_member_pair pair = {.name = arg_names[i], .value = arg_values[i]};
      bennet_vector_push(cn_member_pair)(&term->data.constructor.args, pair);
    }
  }

  return term;
}

// Pattern matching
cn_term* cn_smt_match(cn_term* scrutinee,
    size_t case_count,
    const char** constructor_tags,
    cn_sym** pattern_vars_arrays,
    size_t* pattern_var_counts,
    cn_term** body_terms) {
  assert(scrutinee);
  assert(case_count > 0);
  assert(constructor_tags && pattern_vars_arrays && pattern_var_counts && body_terms);

  // Determine result type from first body term
  cn_base_type result_type = body_terms[0]->base_type;

  // Assert all body terms have the same type
  for (size_t i = 1; i < case_count; i++) {
    assert(body_terms[i]->base_type.tag == result_type.tag);
  }

  cn_term* term = cn_term_alloc(CN_TERM_MATCH, result_type);
  assert(term);

  term->data.match_data.scrutinee = scrutinee;

  // Initialize the cases vector
  bennet_vector_init(cn_match_case)(&term->data.match_data.cases);

  // Build each case
  for (size_t i = 0; i < case_count; i++) {
    assert(constructor_tags[i] && body_terms[i]);

    cn_match_case match_case;
    match_case.constructor_tag = constructor_tags[i];
    match_case.pattern_var_count = pattern_var_counts[i];

    if (match_case.pattern_var_count > 0) {
      assert(pattern_vars_arrays[i]);

      // Allocate array for pattern variables
      match_case.pattern_vars =
          cn_bump_malloc(match_case.pattern_var_count * sizeof(cn_sym));
      assert(match_case.pattern_vars);

      // Copy each pattern variable
      for (size_t j = 0; j < match_case.pattern_var_count; j++) {
        // Handle NULL names for wildcard patterns
        if (pattern_vars_arrays[i][j].name != NULL) {
          match_case.pattern_vars[j].name = pattern_vars_arrays[i][j].name;
        } else {
          match_case.pattern_vars[j].name = NULL;
        }
        match_case.pattern_vars[j].id = pattern_vars_arrays[i][j].id;
      }
    } else {
      match_case.pattern_vars = NULL;
    }

    // Set body term
    match_case.body_term = body_terms[i];

    bennet_vector_push(cn_match_case)(&term->data.match_data.cases, match_case);
  }

  return term;
}

// Equality function for cn_base_type
bool cn_base_type_eq(cn_base_type a, cn_base_type b) {
  // First check if tags are equal
  if (a.tag != b.tag) {
    return false;
  }

  // Then check tag-specific data
  switch (a.tag) {
    case CN_BASE_BOOL:
    case CN_BASE_INTEGER:
    case CN_BASE_REAL:
    case CN_BASE_LOC:
    case CN_BASE_UNIT:
    case CN_BASE_CTYPE:
    case CN_BASE_MAP:
      // Simple types with no additional data
      return true;

    case CN_BASE_BITS:
      return a.data.bits.is_signed == b.data.bits.is_signed &&
             a.data.bits.size_bits == b.data.bits.size_bits;

    case CN_BASE_LIST:
      assert(a.data.list.element_type && b.data.list.element_type);
      return cn_base_type_eq(*a.data.list.element_type, *b.data.list.element_type);

    case CN_BASE_SET:
      assert(a.data.set.element_type && b.data.set.element_type);
      return cn_base_type_eq(*a.data.set.element_type, *b.data.set.element_type);

    case CN_BASE_OPTION:
      assert(a.data.option.element_type && b.data.option.element_type);
      return cn_base_type_eq(*a.data.option.element_type, *b.data.option.element_type);

    case CN_BASE_STRUCT:
      assert(a.data.struct_tag.tag && b.data.struct_tag.tag);
      return strcmp(a.data.struct_tag.tag, b.data.struct_tag.tag) == 0;

    case CN_BASE_DATATYPE:
      assert(a.data.datatype_tag.tag && b.data.datatype_tag.tag);
      return strcmp(a.data.datatype_tag.tag, b.data.datatype_tag.tag) == 0;

    case CN_BASE_TUPLE:
      if (a.data.tuple.count != b.data.tuple.count) {
        return false;
      }
      for (size_t i = 0; i < a.data.tuple.count; i++) {
        if (!cn_base_type_eq(a.data.tuple.types[i], b.data.tuple.types[i])) {
          return false;
        }
      }
      return true;

    case CN_BASE_RECORD:
      if (a.data.record.count != b.data.record.count) {
        return false;
      }
      // Check that field names and types match
      for (size_t i = 0; i < a.data.record.count; i++) {
        assert(a.data.record.names[i] && b.data.record.names[i]);
        if (strcmp(a.data.record.names[i], b.data.record.names[i]) != 0) {
          return false;
        }
        if (!cn_base_type_eq(a.data.record.types[i], b.data.record.types[i])) {
          return false;
        }
      }
      return true;

    default:
      assert(false);  // Unknown tag
      return false;
  }
}

char* cn_term_to_string(cn_term* term) {
  return "";
}
