#ifndef CN_SMT_TERMS_H
#define CN_SMT_TERMS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>
#include <bennet/utils/vector.h>

#ifdef __cplusplus
extern "C" {
#endif

// Forward declarations
struct cn_term;
typedef struct cn_term cn_term;

// Type aliases for vector types
typedef const char* const_char_ptr;
typedef cn_term* cn_term_ptr;

// Vector type declarations for arrays in cn_term
BENNET_VECTOR_DECL(const_char_ptr)
BENNET_VECTOR_DECL(cn_term_ptr)

// Optional type for cn_term_ptr is declared in subst.h

// Hash table type declarations for name-value mappings
BENNET_HASH_TABLE_DECL(const_char_ptr, cn_term_ptr)

// String hash and equality functions for const_char_ptr
static inline size_t bennet_hash_const_char_ptr(const_char_ptr str) {
  if (!str)
    return 0;
  size_t hash = 5381;
  for (const char* p = str; *p; p++) {
    hash = ((hash << 5) + hash) + (unsigned char)*p;
  }
  return hash;
}

static inline bool bennet_eq_const_char_ptr(const_char_ptr a, const_char_ptr b) {
  if (a == b)
    return true;
  if (!a || !b)
    return false;
  return strcmp(a, b) == 0;
}

// Symbol structure with name and unique ID
typedef struct {
  const char* name;
  uint64_t id;
} cn_sym;

// Bitvector info (sign and size)
typedef struct {
  bool is_signed;
  int size_bits;
} cn_bits_info;

// Base type tags
typedef enum {
  CN_BASE_BOOL,
  CN_BASE_INTEGER,
  CN_BASE_REAL,
  CN_BASE_LOC,   // pointer type
  CN_BASE_BITS,  // bitvector type
  CN_BASE_UNIT,
  CN_BASE_CTYPE,
  CN_BASE_STRUCT,
  CN_BASE_LIST,
  CN_BASE_SET,
  CN_BASE_MAP,
  CN_BASE_TUPLE,
  CN_BASE_RECORD,
  CN_BASE_OPTION,
} cn_base_type_tag;

// Forward declaration for recursive types
typedef struct cn_base_type cn_base_type;

// Base type with additional data for specific cases
struct cn_base_type {
  cn_base_type_tag tag;
  union {
    cn_bits_info bits;
    struct {
      cn_base_type* element_type;
    } list;
    struct {
      cn_base_type* element_type;
    } set;
    struct {
      cn_base_type* key_type;
      cn_base_type* value_type;
    } map;
    struct {
      size_t count;
      cn_base_type* types;
    } tuple;
    struct {
      const char* tag;
    } struct_tag;
    struct {
      int tag;
    } datatype_tag;
    struct {
      cn_base_type* element_type;
    } option;
    struct {
      size_t count;
      const char** names;
      cn_base_type* types;
    } record;
  } data;
};

// Optional type declaration for cn_base_type
BENNET_OPTIONAL_DECL(cn_base_type);

// Hash table type declaration for cn_sym -> cn_base_type mapping
BENNET_HASH_TABLE_DECL(cn_sym, cn_base_type)

// Hash and equality functions for cn_sym
static inline size_t bennet_hash_cn_sym(cn_sym sym) {
  // Hash based on the unique ID
  return (size_t)sym.id;
}

static inline bool bennet_eq_cn_sym(cn_sym a, cn_sym b) {
  // Compare by unique ID
  return a.id == b.id;
}

// Helper functions for working with cn_base_type

static inline cn_base_type cn_base_type_simple(cn_base_type_tag tag) {
  assert(tag != CN_BASE_BITS);  // Assert it is not a complex type
  cn_base_type bt = {.tag = tag};
  return bt;
}

static inline cn_base_type cn_base_type_bits(bool is_signed, int size_bits) {
  cn_base_type bt;
  bt.tag = CN_BASE_BITS;
  bt.data.bits.is_signed = is_signed;
  bt.data.bits.size_bits = size_bits;
  return bt;
}

static inline bool cn_base_type_is(cn_base_type bt, cn_base_type_tag tag) {
  return bt.tag == tag;
}

static inline cn_bits_info cn_base_type_get_bits_info(cn_base_type bt) {
  assert(bt.tag == CN_BASE_BITS);
  cn_bits_info info;
  info.is_signed = bt.data.bits.is_signed;
  info.size_bits = bt.data.bits.size_bits;
  return info;
}

// Helper functions for complex types
static inline cn_base_type cn_base_type_list(cn_base_type* element_type) {
  cn_base_type bt;
  bt.tag = CN_BASE_LIST;
  bt.data.list.element_type = element_type;
  return bt;
}

static inline cn_base_type cn_base_type_set(cn_base_type* element_type) {
  cn_base_type bt;
  bt.tag = CN_BASE_SET;
  bt.data.set.element_type = element_type;
  return bt;
}

static inline cn_base_type cn_base_type_map(
    cn_base_type* key_type, cn_base_type* value_type) {
  cn_base_type bt;
  bt.tag = CN_BASE_MAP;
  bt.data.map.key_type = key_type;
  bt.data.map.value_type = value_type;
  return bt;
}

static inline cn_base_type cn_base_type_tuple(cn_base_type* types, size_t count) {
  cn_base_type bt;
  bt.tag = CN_BASE_TUPLE;
  bt.data.tuple.types = types;
  bt.data.tuple.count = count;
  return bt;
}

static inline cn_base_type cn_base_type_struct(const char* struct_tag) {
  cn_base_type bt;
  bt.tag = CN_BASE_STRUCT;
  bt.data.struct_tag.tag = struct_tag;
  return bt;
}

static inline cn_base_type cn_base_type_option(cn_base_type* element_type) {
  cn_base_type bt;
  bt.tag = CN_BASE_OPTION;
  bt.data.option.element_type = element_type;
  return bt;
}

static inline cn_base_type cn_base_type_record(
    const char** names, cn_base_type* types, size_t count) {
  cn_base_type bt;
  bt.tag = CN_BASE_RECORD;
  bt.data.record.names = names;
  bt.data.record.types = types;
  bt.data.record.count = count;
  return bt;
}

// Helper function to create record types with proper memory allocation
static inline cn_base_type create_record_type(
    size_t count, const char** field_names, cn_base_type* field_types) {
  // Allocate memory for permanent storage (memory leak is acceptable for test generation)
  const char** names = (const char**)malloc(count * sizeof(const char*));
  cn_base_type* types = (cn_base_type*)malloc(count * sizeof(cn_base_type));
  for (size_t i = 0; i < count; i++) {
    names[i] = field_names[i];
    types[i] = field_types[i];
  }
  return cn_base_type_record(names, types, count);
}

// Constant types
typedef enum {
  CN_CONST_Z,            // integer
  CN_CONST_BITS,         // bitvector
  CN_CONST_Q,            // rational (simplified to double)
  CN_CONST_POINTER,      // pointer
  CN_CONST_BOOL,         // boolean
  CN_CONST_UNIT,         // unit value
  CN_CONST_NULL,         // null pointer
  CN_CONST_CTYPE_CONST,  // C type constant
  CN_CONST_DEFAULT,      // default value
} cn_const_type;

// Unary operators
typedef enum {
  CN_UNOP_NOT,
  CN_UNOP_NEGATE,
  CN_UNOP_BW_CLZ_NOSMT,
  CN_UNOP_BW_CTZ_NOSMT,
  CN_UNOP_BW_FFS_NOSMT,
  CN_UNOP_BW_FLS_NOSMT,
  CN_UNOP_BW_COMPL,
} cn_unop;

// Binary operators
typedef enum {
  CN_BINOP_AND,
  CN_BINOP_OR,
  CN_BINOP_IMPLIES,
  CN_BINOP_ADD,
  CN_BINOP_SUB,
  CN_BINOP_MUL,
  CN_BINOP_MULNOSMT,
  CN_BINOP_DIV,
  CN_BINOP_DIVNOSMT,
  CN_BINOP_EXP,
  CN_BINOP_EXPNOSMT,
  CN_BINOP_REM,
  CN_BINOP_REMNOSMT,
  CN_BINOP_MOD,
  CN_BINOP_MODNOSMT,
  CN_BINOP_BW_XOR,
  CN_BINOP_BW_AND,
  CN_BINOP_BW_OR,
  CN_BINOP_SHIFT_LEFT,
  CN_BINOP_SHIFT_RIGHT,
  CN_BINOP_LT,
  CN_BINOP_LE,
  CN_BINOP_MIN,
  CN_BINOP_MAX,
  CN_BINOP_EQ,
  CN_BINOP_LT_POINTER,
  CN_BINOP_LE_POINTER,
  CN_BINOP_SET_UNION,
  CN_BINOP_SET_INTERSECTION,
  CN_BINOP_SET_DIFFERENCE,
  CN_BINOP_SET_MEMBER,
  CN_BINOP_SUBSET,
} cn_binop;

// Term types
typedef enum {
  CN_TERM_CONST,
  CN_TERM_SYM,
  CN_TERM_UNOP,
  CN_TERM_BINOP,
  CN_TERM_ITE,    // if-then-else
  CN_TERM_EACHI,  // universal quantification over integer range
  CN_TERM_STRUCT,
  CN_TERM_STRUCT_MEMBER,
  CN_TERM_STRUCT_UPDATE,
  CN_TERM_RECORD,
  CN_TERM_RECORD_MEMBER,
  CN_TERM_CONSTRUCTOR,
  CN_TERM_MEMBER_SHIFT,
  CN_TERM_ARRAY_SHIFT,
  CN_TERM_WRAPI,
  CN_TERM_MAP_SET,
  CN_TERM_MAP_GET,
  CN_TERM_APPLY,
  CN_TERM_LET,
  CN_TERM_MATCH,
  CN_TERM_CAST,
} cn_term_type;

// Constant value union
typedef struct {
  cn_const_type type;
  union {
    int64_t z;  // CN_CONST_Z
    struct {    // CN_CONST_BITS
      cn_bits_info info;
      int64_t value;
    } bits;
    double q;           // CN_CONST_Q (simplified from rational)
    uintptr_t pointer;  // CN_CONST_POINTER
    bool boolean;       // CN_CONST_BOOL
    // CN_CONST_UNIT has no data
    // CN_CONST_NULL has no data
    const char* ctype_name;     // CN_CONST_CTYPE_CONST (simplified)
    cn_base_type default_type;  // CN_CONST_DEFAULT
  } data;
} cn_const;

// Main term structure
struct cn_term {
  cn_term_type type;
  cn_base_type base_type;

  union {
    cn_const const_val;  // CN_TERM_CONST

    cn_sym sym;  // CN_TERM_SYM

    struct {  // CN_TERM_UNOP
      cn_unop op;
      cn_term* operand;
    } unop;

    struct {  // CN_TERM_BINOP
      cn_binop op;
      cn_term* left;
      cn_term* right;
    } binop;

    struct {  // CN_TERM_ITE
      cn_term* cond;
      cn_term* then_term;
      cn_term* else_term;
    } ite;

    struct {  // CN_TERM_EACHI
      int start;
      const char* var_name;
      cn_base_type var_type;
      int end;
      cn_term* body;
    } eachi;

    struct {  // CN_TERM_STRUCT
      const char* tag;
      bennet_hash_table(const_char_ptr, cn_term_ptr) members;
    } struct_val;

    struct {  // CN_TERM_STRUCT_MEMBER
      cn_term* struct_term;
      const char* member_name;
    } struct_member;

    struct {  // CN_TERM_STRUCT_UPDATE
      cn_term* struct_term;
      const char* member_name;
      cn_term* new_value;
    } struct_update;

    struct {  // CN_TERM_RECORD
      bennet_hash_table(const_char_ptr, cn_term_ptr) members;
    } record;

    struct {  // CN_TERM_RECORD_MEMBER
      cn_term* record_term;
      const char* member_name;
    } record_member;

    struct {  // CN_TERM_CONSTRUCTOR
      const char* constructor_name;
      bennet_hash_table(const_char_ptr, cn_term_ptr) args;
    } constructor;

    struct {  // CN_TERM_MEMBER_SHIFT
      cn_term* base;
      size_t offset;
    } member_shift;

    struct {  // CN_TERM_ARRAY_SHIFT
      cn_term* base;
      size_t element_size;  // constant element size in bytes
      cn_term* index;
    } array_shift;

    struct {                 // CN_TERM_WRAPI
      const char* int_type;  // simplified integer type
      cn_term* value;
    } wrapi;

    struct {  // CN_TERM_MAP_SET
      cn_term* map;
      cn_term* key;
      cn_term* value;
    } map_set;

    struct {  // CN_TERM_MAP_GET
      cn_term* map;
      cn_term* key;
    } map_get;

    struct {  // CN_TERM_APPLY
      const char* function_name;
      bennet_vector(cn_term_ptr) args;
    } apply;

    struct {  // CN_TERM_LET
      const char* var_name;
      cn_term* value;
      cn_term* body;
    } let;

    // CN_TERM_MATCH - skipping for now (complex pattern matching)

    struct {  // CN_TERM_CAST
      cn_base_type target_type;
      cn_term* value;
    } cast;
  } data;
};

// Helper functions
cn_term* cn_term_alloc(cn_term_type type, cn_base_type base_type);

// Smart constructor function declarations
cn_term* cn_smt_z(int64_t value);
cn_term* cn_smt_bool(bool value);
cn_term* cn_smt_sym(cn_sym sym, cn_base_type type);

// Symbol counter management
void reset_cn_sym_counter(void);

// Helper function to create cn_sym from string (for testing)
static inline cn_sym cn_sym_from_string(const char* name) {
  static uint64_t counter = 0;
  cn_sym sym = {.name = name, .id = counter++};
  return sym;
}

// Convenience function for tests
static inline cn_term* cn_smt_sym_string(const char* name, cn_base_type type) {
  if (!name)
    return NULL;
  return cn_smt_sym(cn_sym_from_string(name), type);
}
cn_term* cn_smt_null(void);
cn_term* cn_smt_unit(void);
cn_term* cn_smt_bits(bool is_signed, int size_bits, intmax_t value);
cn_term* cn_smt_num(cn_base_type bt, intmax_t value);
cn_term* cn_smt_rational(double value);
cn_term* cn_smt_pointer(uintptr_t ptr);
cn_term* cn_smt_default(cn_base_type type);

// Logical operators
cn_term* cn_smt_not(cn_term* operand);
cn_term* cn_smt_and(cn_term* left, cn_term* right);
cn_term* cn_smt_or(cn_term* left, cn_term* right);
cn_term* cn_smt_implies(cn_term* left, cn_term* right);

// Comparison operators
cn_term* cn_smt_eq(cn_term* left, cn_term* right);
cn_term* cn_smt_lt(cn_term* left, cn_term* right);
cn_term* cn_smt_le(cn_term* left, cn_term* right);
cn_term* cn_smt_gt(cn_term* left, cn_term* right);
cn_term* cn_smt_ge(cn_term* left, cn_term* right);

// Arithmetic operators
cn_term* cn_smt_add(cn_term* left, cn_term* right);
cn_term* cn_smt_sub(cn_term* left, cn_term* right);
cn_term* cn_smt_mul(cn_term* left, cn_term* right);
cn_term* cn_smt_div(cn_term* left, cn_term* right);
cn_term* cn_smt_rem(cn_term* left, cn_term* right);
cn_term* cn_smt_mod(cn_term* left, cn_term* right);
cn_term* cn_smt_min(cn_term* left, cn_term* right);
cn_term* cn_smt_max(cn_term* left, cn_term* right);

// Bitwise operators
cn_term* cn_smt_bw_and(cn_term* left, cn_term* right);
cn_term* cn_smt_bw_or(cn_term* left, cn_term* right);
cn_term* cn_smt_bw_xor(cn_term* left, cn_term* right);
cn_term* cn_smt_bw_compl(cn_term* operand);
cn_term* cn_smt_shift_left(cn_term* left, cn_term* right);
cn_term* cn_smt_shift_right(cn_term* left, cn_term* right);

// Control flow
cn_term* cn_smt_ite(cn_term* cond, cn_term* then_term, cn_term* else_term);

// Pointer operations
cn_term* cn_smt_member_shift(cn_term* base, size_t offset);
cn_term* cn_smt_array_shift(cn_term* base, size_t element_size, cn_term* index);

// Type operations
cn_term* cn_smt_cast(cn_base_type target_type, cn_term* value);

// Map operations
cn_term* cn_smt_map_get(cn_term* map, cn_term* key, cn_base_type result_type);
cn_term* cn_smt_map_set(cn_term* map, cn_term* key, cn_term* value);

// Function application
cn_term* cn_smt_apply(const char* function_name,
    cn_base_type result_type,
    bennet_vector(cn_term_ptr) * args);

// Let binding
cn_term* cn_smt_let(const char* var_name, cn_term* value, cn_term* body);

// Record operations
cn_term* cn_smt_record(
    size_t member_count, const char** member_names, cn_term** member_values);
cn_term* cn_smt_record_member(cn_term* record_term, const char* member_name);

// Pretty-print
char* cn_term_to_string(cn_term* term);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_H
