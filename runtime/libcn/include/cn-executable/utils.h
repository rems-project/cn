
#ifndef CN_UTILS
#define CN_UTILS

#include "bump_alloc.h"
#include "fulminate_alloc.h"
#include "hash_table.h"
#include "rts_deps.h"
#include "stack.h"

#define fallthrough __attribute__((__fallthrough__))

#define cn_printf(level, ...)                                                            \
  if (get_cn_logging_level() >= level) {                                                 \
    printf(__VA_ARGS__);                                                                 \
  }

// XXX: things used by injected code
#define true  1
#define false 0

enum spec_mode {
  PRE = 1,
  POST = 2,
  LOOP = 3,
  STATEMENT = 4,
  C_ACCESS = 5,
  NON_SPEC = 6
};

enum region_owned {
  NO_WILDCARD,
  SOME_WILDCARD,
  FULL_WILDCARD,
};

struct fulm_init_flags {
  bool with_ghost_args;
  bool source_locs_mode;
  bool correct_missing_ownership;
  bool ownership_stack_mode;
};

struct fulm_init_config {
  size_t max_bump_blocks;
  size_t bump_block_size;
  struct fulm_init_flags flags;
};

void fulminate_destroy(bool with_ghost_args);
void fulminate_init(struct fulm_init_config flags);

/* Error handlers */
enum cn_logging_level {
  CN_LOGGING_NONE = 0,
  CN_LOGGING_ERROR = 1,
  CN_LOGGING_INFO = 2
};

enum cn_logging_level get_cn_logging_level(void);

/** Sets the logging level, returning the previous one */
enum cn_logging_level set_cn_logging_level(enum cn_logging_level new_level);

enum cn_trace_granularity {
  CN_TRACE_NONE = 0,
  CN_TRACE_ENDS = 1,
  CN_TRACE_ALL = 2,
};

enum cn_trace_granularity get_cn_trace_granularity(void);

/** Sets the trace granularity, returning the previous one */
enum cn_trace_granularity set_cn_trace_granularity(
    enum cn_trace_granularity new_granularity);

void cn_print_nr_owned_predicates(void);

struct cn_error_message_info {
  const char* function_name;
  char* file_name;
  int line_number;
  char* cn_source_loc;
  struct cn_error_message_info* parent;
  struct cn_error_message_info* child;
};

void init_error_msg_info_(const char* function_name, char* file_name, int line_number);

#define init_error_msg_info() init_error_msg_info_(__func__, __FILE__, __LINE__)

void reset_error_msg_info(void);
void free_error_msg_info(void);

void update_error_message_info_(
    const char* function_name, char* file_name, int line_number, char* cn_source_loc);

void cn_pop_msg_info(void);

#define update_cn_error_message_info(x)                                                  \
  update_error_message_info_(__func__, __FILE__, __LINE__ + 1, x)

#define update_cn_error_message_info_access_check(x)                                     \
  update_error_message_info_(__func__, __FILE__, __LINE__, x)

/* CN base types: plain C scalars, passed by value.

   A CN bitvector, integer, boolean, pointer or allocation id is represented
   by the C scalar of the same width; nothing is heap-allocated for them.
   Only aggregates (maps, and the generated structs for C structs, records and
   datatypes) are still bump-allocated and passed as pointers. Where a
   generic container needs a `void*` (map values, ghost-argument frames), a
   scalar is boxed with `CNTYPE_box` and unboxed on the way out. */

/* Signed bitvectors */

typedef int8_t cn_bits_i8;
typedef int16_t cn_bits_i16;
typedef int32_t cn_bits_i32;
typedef int64_t cn_bits_i64;

/* Unsigned bitvectors */

typedef uint8_t cn_bits_u8;
typedef uint16_t cn_bits_u16;
typedef uint32_t cn_bits_u32;
typedef uint64_t cn_bits_u64;

typedef int64_t cn_integer;

typedef void* cn_pointer;

typedef bool cn_bool;

typedef uint8_t cn_alloc_id;

typedef hash_table cn_map;

void init_ownership_ghost_state(void);
void free_ownership_ghost_state(void);
void init_ghost_stack_depth(void);
void init_source_locs_mode(bool flag);
void init_correct_missing_ownership(bool flag);
void init_ownership_stack_mode(bool flag);
signed long get_cn_stack_depth(void);
void ghost_stack_depth_incr(void);
void ghost_stack_depth_decr(void);
void cn_postcondition_leak_check(void);

struct loop_ownership* init_loop_ownership_state(void);
void cn_loop_leak_check(void);
void cn_loop_put_back_ownership(struct loop_ownership* loop_ownership);

/* malloc, free */
void* cn_aligned_alloc(size_t align, size_t size);
void* cn_unsafe_aligned_alloc(size_t align, size_t size);
void* cn_malloc(size_t size);
void* cn_unsafe_malloc(size_t size);
void* cn_calloc(size_t num, size_t size);
void* cn_unsafe_calloc(size_t num, size_t size);
void cn_free_sized(void*, size_t len);

void cn_print_nr_u64(int i, unsigned long u);
void cn_print_u64(const char* str, unsigned long u);
void dump_ownership_ghost_state(int stack_depth);
bool is_mapped(void* ptr);

/* cn_failure callbacks */
enum cn_failure_mode {
  CN_FAILURE_ASSERT = 1,
  CN_FAILURE_CHECK_OWNERSHIP,
  CN_FAILURE_OWNERSHIP_LEAK,
  CN_FAILURE_FULM_ALLOC,
  CN_FAILURE_USER_ALLOC,
  CN_FAILURE_GHOST_ARGS
};

typedef void (*cn_failure_callback)(enum cn_failure_mode, enum spec_mode);
void set_cn_failure_cb(cn_failure_callback callback);
void reset_cn_failure_cb(void);
void cn_failure(enum cn_failure_mode failure_mode, enum spec_mode spec_mode);

/* Booleans */

static inline cn_bool convert_to_cn_bool(bool b) {
  return b;
}

static inline bool convert_from_cn_bool(cn_bool b) {
  return b;
}

void cn_assert(cn_bool cn_b, enum spec_mode spec_mode);

static inline cn_bool cn_bool_and(cn_bool b1, cn_bool b2) {
  return b1 && b2;
}

static inline cn_bool cn_bool_or(cn_bool b1, cn_bool b2) {
  return b1 || b2;
}

static inline cn_bool cn_bool_not(cn_bool b) {
  return !b;
}

static inline cn_bool cn_bool_implies(cn_bool b1, cn_bool b2) {
  return !b1 || b2;
}

static inline cn_bool cn_bool_equality(cn_bool b1, cn_bool b2) {
  return b1 == b2;
}

/* `void*` equality on boxed booleans, for cn_map_equality */
static inline cn_bool cn_bool_ptr_equality(void* b1, void* b2) {
  return *(cn_bool*)b1 == *(cn_bool*)b2;
}

static inline cn_bool default_cn_bool(void) {
  return false;
}

/* Maps: a hash table from cn_integer keys to boxed values (`void*`) */

cn_map* map_create(void);
cn_map* cn_map_set(cn_map* m, cn_integer key, void* value);
cn_map* cn_map_deep_copy(cn_map* m1);
cn_bool cn_map_equality(
    cn_map* m1, cn_map* m2, cn_bool(value_equality_fun)(void*, void*));
cn_map* default_cn_map(void);

/* Converts the C array `c_ptr` of `num_elements` elements to a CN map, with
   `set_fn` the map setter for the element type (`cn_map_set_<cntype>`, or
   `cn_map_set` for struct values) and `conversion_fn` its `convert_to_`. */
#define convert_to_cn_map(c_ptr, set_fn, conversion_fn, num_elements)                    \
  ({                                                                                     \
    cn_map* m = map_create();                                                            \
    for (int i = 0; i < num_elements; i++) {                                             \
      set_fn(m, convert_to_cn_integer(i), conversion_fn((c_ptr)[i]));                    \
    }                                                                                    \
    m;                                                                                   \
  })
#define convert_from_cn_map(arr, m, cntype, num_elements)                                \
  for (int i = 0; i < num_elements; i++) {                                               \
    (arr)[i] = convert_from_##cntype(cn_map_get_##cntype(m, convert_to_cn_integer(i)));  \
  }

/* Pointers */

static inline cn_pointer convert_to_cn_pointer(const void* ptr) {
  return (cn_pointer)ptr;
}

static inline void* convert_from_cn_pointer(cn_pointer cn_ptr) {
  return cn_ptr;
}

static inline cn_bool cn_pointer_equality(cn_pointer p1, cn_pointer p2) {
  return p1 == p2;
}

static inline cn_bool cn_pointer_ptr_equality(void* p1, void* p2) {
  return *(cn_pointer*)p1 == *(cn_pointer*)p2;
}

static inline cn_bool cn_pointer_is_null(cn_pointer p) {
  return p == (void*)0;
}

static inline cn_bool cn_pointer_lt(cn_pointer p1, cn_pointer p2) {
  return (uintptr_t)p1 < (uintptr_t)p2;
}

static inline cn_bool cn_pointer_le(cn_pointer p1, cn_pointer p2) {
  return (uintptr_t)p1 <= (uintptr_t)p2;
}

static inline cn_bool cn_pointer_gt(cn_pointer p1, cn_pointer p2) {
  return (uintptr_t)p1 > (uintptr_t)p2;
}

static inline cn_bool cn_pointer_ge(cn_pointer p1, cn_pointer p2) {
  return (uintptr_t)p1 >= (uintptr_t)p2;
}

static inline cn_pointer cn_pointer_min(cn_pointer p, cn_pointer q) {
  return (uintptr_t)p < (uintptr_t)q ? p : q;
}

static inline cn_pointer cn_pointer_max(cn_pointer p, cn_pointer q) {
  return (uintptr_t)p > (uintptr_t)q ? p : q;
}

static inline cn_pointer cn_pointer_mod(cn_pointer ptr, cn_pointer n) {
  return (cn_pointer)((uintptr_t)ptr % (uintptr_t)n);
}

static inline cn_pointer cast_cn_pointer_to_cn_pointer(cn_pointer p) {
  return p;
}

static inline cn_pointer default_cn_pointer(void) {
  return (void*)0;
}

#define cn_pointer_deref(CN_PTR, CTYPE) (*((CTYPE*)(CN_PTR)))

#define cn_array_shift(cn_ptr, size, index)                                              \
  ((cn_pointer)((uintptr_t)(cn_ptr) + (uintptr_t)(index) * (uintptr_t)(size)))

#define cn_member_shift(cn_ptr, tag, member_name)                                        \
  ((cn_pointer)((uintptr_t)(cn_ptr) + offsetof(struct tag, member_name)))

/* CN integer type auxilliary functions */

/* Boxing: a bump-allocated copy, for the `void*` slots of maps and ghost frames */
#define CN_GEN_BOX(CNTYPE)                                                               \
  static inline CNTYPE* CNTYPE##_box(CNTYPE i) {                                         \
    CNTYPE* ret = (CNTYPE*)cn_bump_aligned_alloc(alignof(CNTYPE), sizeof(CNTYPE));       \
    *ret = i;                                                                            \
    return ret;                                                                          \
  }

#define CN_GEN_EQUALITY(CNTYPE) CN_GEN_EQUALITY_(CNTYPE)

#define CN_GEN_EQUALITY_(CNTYPE)                                                         \
  static inline cn_bool CNTYPE##_equality(CNTYPE i1, CNTYPE i2) {                        \
    return i1 == i2;                                                                     \
  }                                                                                      \
  static inline cn_bool CNTYPE##_ptr_equality(void* i1, void* i2) {                      \
    return *(CNTYPE*)i1 == *(CNTYPE*)i2;                                                 \
  }

#define CN_GEN_CONVERT(CTYPE, CNTYPE)                                                    \
  static inline CNTYPE convert_to_##CNTYPE(CTYPE i) {                                    \
    return (CNTYPE)i;                                                                    \
  }

#define CN_GEN_CONVERT_FROM(CTYPE, CNTYPE)                                               \
  static inline CTYPE convert_from_##CNTYPE(CNTYPE i) {                                  \
    return (CTYPE)i;                                                                     \
  }

/* Arithmetic operators */

#define CN_GEN_LT(CNTYPE)                                                                \
  static inline cn_bool CNTYPE##_lt(CNTYPE i1, CNTYPE i2) {                              \
    return i1 < i2;                                                                      \
  }

#define CN_GEN_LE(CNTYPE)                                                                \
  static inline cn_bool CNTYPE##_le(CNTYPE i1, CNTYPE i2) {                              \
    return i1 <= i2;                                                                     \
  }

#define CN_GEN_GT(CNTYPE)                                                                \
  static inline cn_bool CNTYPE##_gt(CNTYPE i1, CNTYPE i2) {                              \
    return i1 > i2;                                                                      \
  }

#define CN_GEN_GE(CNTYPE)                                                                \
  static inline cn_bool CNTYPE##_ge(CNTYPE i1, CNTYPE i2) {                              \
    return i1 >= i2;                                                                     \
  }

#define CN_GEN_NEGATE(CNTYPE)                                                            \
  static inline CNTYPE CNTYPE##_negate(CNTYPE i) {                                       \
    return (CNTYPE)(-i);                                                                 \
  }

#define CN_GEN_BW_COMPL(CNTYPE)                                                          \
  static inline CNTYPE CNTYPE##_bw_compl(CNTYPE i) {                                     \
    return (CNTYPE)(~i);                                                                 \
  }

/* Addition and subtraction go through the unsigned type so that signed
   overflow wraps instead of being undefined behaviour */
#define CN_GEN_ADD(UCTYPE, CTYPE, CNTYPE)                                                \
  static inline CNTYPE CNTYPE##_add(CNTYPE i1, CNTYPE i2) {                              \
    return (CNTYPE)((UCTYPE)i1 + (UCTYPE)i2);                                            \
  }

#define CN_GEN_SUB(UCTYPE, CTYPE, CNTYPE)                                                \
  static inline CNTYPE CNTYPE##_sub(CNTYPE i1, CNTYPE i2) {                              \
    return (CNTYPE)((UCTYPE)i1 - (UCTYPE)i2);                                            \
  }

#define CN_GEN_MUL(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE CNTYPE##_multiply(CNTYPE i1, CNTYPE i2) {                         \
    return (CNTYPE)(i1 * i2);                                                            \
  }

#define CN_GEN_DIV(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE CNTYPE##_divide(CNTYPE i1, CNTYPE i2) {                           \
    return (CNTYPE)(i1 / i2);                                                            \
  }

#define CN_GEN_SHIFT_LEFT(CTYPE, CNTYPE)                                                 \
  static inline CNTYPE CNTYPE##_shift_left(CNTYPE i1, CNTYPE i2) {                       \
    return (CNTYPE)(i1 << i2);                                                           \
  }

#define CN_GEN_SHIFT_RIGHT(CTYPE, CNTYPE)                                                \
  static inline CNTYPE CNTYPE##_shift_right(CNTYPE i1, CNTYPE i2) {                      \
    return (CNTYPE)(i1 >> i2);                                                           \
  }

#define CN_GEN_MIN(CNTYPE)                                                               \
  static inline CNTYPE CNTYPE##_min(CNTYPE i1, CNTYPE i2) {                              \
    return i1 < i2 ? i1 : i2;                                                            \
  }

#define CN_GEN_MAX(CNTYPE)                                                               \
  static inline CNTYPE CNTYPE##_max(CNTYPE i1, CNTYPE i2) {                              \
    return i1 > i2 ? i1 : i2;                                                            \
  }

/* TODO: Account for UB: https://stackoverflow.com/a/20638659 */
#define CN_GEN_MOD(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE CNTYPE##_mod(CNTYPE i1, CNTYPE i2) {                              \
    CNTYPE res = (CNTYPE)(i1 % i2);                                                      \
    if (res < 0) {                                                                       \
      res = (CNTYPE)((i2 < 0) ? res - i2 : res + i2);                                    \
    }                                                                                    \
    return res;                                                                          \
  }

#define CN_GEN_REM(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE CNTYPE##_rem(CNTYPE i1, CNTYPE i2) {                              \
    return (CNTYPE)(i1 % i2);                                                            \
  }

#define CN_GEN_XOR(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE CNTYPE##_xor(CNTYPE i1, CNTYPE i2) {                              \
    return (CNTYPE)(i1 ^ i2);                                                            \
  }

#define CN_GEN_BWAND(CTYPE, CNTYPE)                                                      \
  static inline CNTYPE CNTYPE##_bwand(CNTYPE i1, CNTYPE i2) {                            \
    return (CNTYPE)(i1 & i2);                                                            \
  }

#define CN_GEN_BWOR(CTYPE, CNTYPE)                                                       \
  static inline CNTYPE CNTYPE##_bwor(CNTYPE i1, CNTYPE i2) {                             \
    return (CNTYPE)(i1 | i2);                                                            \
  }

cn_bits_u32 cn_bits_u32_fls(cn_bits_u32 i1);
cn_bits_u64 cn_bits_u64_flsl(cn_bits_u64 i1);

static inline int ipow(int base, int exp) {
  int result = 1;
  for (;;) {
    if (exp & 1)
      result *= base;
    exp >>= 1;
    if (!exp)
      break;
    base *= base;
  }

  return result;
}

#define CN_GEN_POW(CTYPE, CNTYPE)                                                        \
  static inline CNTYPE CNTYPE##_pow(CNTYPE i1, CNTYPE i2) {                              \
    return (CNTYPE)ipow(i1, i2);                                                         \
  }

/* Returns the successor; the caller assigns it (`i = cn_bits_i32_increment(i)`) */
#define CN_GEN_INCREMENT(CNTYPE)                                                         \
  static inline CNTYPE CNTYPE##_increment(CNTYPE i) {                                    \
    return (CNTYPE)(i + 1);                                                              \
  }

#define CN_GEN_PTR_ADD(CNTYPE)                                                           \
  static inline cn_pointer cn_pointer_add_##CNTYPE(cn_pointer ptr, CNTYPE i) {           \
    return (cn_pointer)((char*)ptr + i);                                                 \
  }

/* Casting functions */

#define CN_GEN_CAST_TO_PTR(CNTYPE, INTPTR_TYPE)                                          \
  static inline cn_pointer cast_##CNTYPE##_to_cn_pointer(CNTYPE i) {                     \
    return (cn_pointer)(INTPTR_TYPE)i;                                                   \
  }

#define CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, INTPTR_TYPE)                                 \
  static inline CNTYPE cast_cn_pointer_to_##CNTYPE(cn_pointer ptr) {                     \
    return (CNTYPE)(CTYPE)(INTPTR_TYPE)ptr;                                              \
  }

#define CN_GEN_CAST_INT_TYPES(CNTYPE1, CTYPE2, CNTYPE2)                                  \
  static inline CNTYPE2 cast_##CNTYPE1##_to_##CNTYPE2(CNTYPE1 i) {                       \
    return (CNTYPE2)(CTYPE2)i;                                                           \
  }

#define CN_GEN_DEFAULT(CNTYPE)                                                           \
  static inline CNTYPE default_##CNTYPE(void) {                                          \
    return (CNTYPE)0;                                                                    \
  }

/* Map access for a scalar value type: the value is boxed on the way in and
   unboxed on the way out; a missing key reads as the type's default */
#define CN_GEN_MAP_SET(CNTYPE)                                                           \
  static inline cn_map* cn_map_set_##CNTYPE(cn_map* m, cn_integer key, CNTYPE value) {   \
    return cn_map_set(m, key, CNTYPE##_box(value));                                      \
  }

#define CN_GEN_MAP_GET(CNTYPE)                                                           \
  static inline CNTYPE cn_map_get_##CNTYPE(cn_map* m, cn_integer key) {                  \
    void* res = ht_get(m, &key);                                                         \
    if (!res) {                                                                          \
      return default_##CNTYPE();                                                         \
    }                                                                                    \
    return *(CNTYPE*)res;                                                                \
  }

/* Ghost arguments of a scalar type: boxed into the frame, unboxed on load */
#define CN_GEN_GHOST_ARG(CNTYPE)                                                         \
  static inline void add_##CNTYPE##_to_ghost_frame(int i, CNTYPE value) {                \
    add_arg_to_ghost_frame(i, CNTYPE##_box(value));                                      \
  }                                                                                      \
  static inline CNTYPE load_##CNTYPE##_from_ghost_frame(int i) {                         \
    return *(CNTYPE*)load_arg_from_ghost_frame(i);                                       \
  }

#define CN_GEN_CASTS_INNER(CTYPE, CNTYPE)                                                \
  CN_GEN_CAST_INT_TYPES(cn_bits_i8, CTYPE, CNTYPE)                                       \
  CN_GEN_CAST_INT_TYPES(cn_bits_i16, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_i32, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_i64, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_u8, CTYPE, CNTYPE)                                       \
  CN_GEN_CAST_INT_TYPES(cn_bits_u16, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_u32, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_bits_u64, CTYPE, CNTYPE)                                      \
  CN_GEN_CAST_INT_TYPES(cn_integer, CTYPE, CNTYPE)

#define CN_GEN_PTR_CASTS_UNSIGNED(CTYPE, CNTYPE)                                         \
  CN_GEN_CAST_TO_PTR(CNTYPE, uintptr_t)                                                  \
  CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, uintptr_t)

#define CN_GEN_PTR_CASTS_SIGNED(CTYPE, CNTYPE)                                           \
  CN_GEN_CAST_TO_PTR(CNTYPE, intptr_t)                                                   \
  CN_GEN_CAST_FROM_PTR(CTYPE, CNTYPE, intptr_t)

#define CN_GEN_ALL(UCTYPE, CTYPE, CNTYPE) CN_GEN_ALL_(UCTYPE, CTYPE, CNTYPE)

#define CN_GEN_ALL_(UCTYPE, CTYPE, CNTYPE)                                               \
  CN_GEN_BOX(CNTYPE)                                                                     \
  CN_GEN_CONVERT(CTYPE, CNTYPE)                                                          \
  CN_GEN_CONVERT_FROM(CTYPE, CNTYPE)                                                     \
  CN_GEN_EQUALITY(CNTYPE)                                                                \
  CN_GEN_LT(CNTYPE)                                                                      \
  CN_GEN_LE(CNTYPE)                                                                      \
  CN_GEN_GT(CNTYPE)                                                                      \
  CN_GEN_GE(CNTYPE)                                                                      \
  CN_GEN_NEGATE(CNTYPE)                                                                  \
  CN_GEN_BW_COMPL(CNTYPE)                                                                \
  CN_GEN_ADD(UCTYPE, CTYPE, CNTYPE)                                                      \
  CN_GEN_SUB(UCTYPE, CTYPE, CNTYPE)                                                      \
  CN_GEN_MUL(CTYPE, CNTYPE)                                                              \
  CN_GEN_DIV(CTYPE, CNTYPE)                                                              \
  CN_GEN_SHIFT_LEFT(CTYPE, CNTYPE)                                                       \
  CN_GEN_SHIFT_RIGHT(CTYPE, CNTYPE)                                                      \
  CN_GEN_MIN(CNTYPE)                                                                     \
  CN_GEN_MAX(CNTYPE)                                                                     \
  CN_GEN_MOD(CTYPE, CNTYPE)                                                              \
  CN_GEN_REM(CTYPE, CNTYPE)                                                              \
  CN_GEN_XOR(CTYPE, CNTYPE)                                                              \
  CN_GEN_BWAND(CTYPE, CNTYPE)                                                            \
  CN_GEN_BWOR(CTYPE, CNTYPE)                                                             \
  CN_GEN_POW(CTYPE, CNTYPE)                                                              \
  CN_GEN_INCREMENT(CNTYPE)                                                               \
  CN_GEN_PTR_ADD(CNTYPE)                                                                 \
  CN_GEN_CASTS_INNER(CTYPE, CNTYPE)                                                      \
  CN_GEN_DEFAULT(CNTYPE)                                                                 \
  CN_GEN_MAP_SET(CNTYPE)                                                                 \
  CN_GEN_MAP_GET(CNTYPE)                                                                 \
  CN_GEN_GHOST_ARG(CNTYPE)

/* Ghost arguments (declared before the CN_GEN_ALL instances, which wrap them) */
void init_ghost_frame_stack(void);
void push_ghost_frame(int tag, int size);
void add_arg_to_ghost_frame(int i, void* ptr_to_ghost_arg);
void* load_arg_from_ghost_frame(int i);
int top_ghost_frame_tag(void);
void pop_ghost_frame(void);
void free_ghost_frame_stack(void);
void cn_ghost_arg_failure(void);

CN_GEN_ALL(uint8_t, int8_t, cn_bits_i8)
CN_GEN_ALL(uint16_t, int16_t, cn_bits_i16)
CN_GEN_ALL(uint32_t, int32_t, cn_bits_i32)
CN_GEN_ALL(uint64_t, int64_t, cn_bits_i64)
CN_GEN_ALL(uint8_t, uint8_t, cn_bits_u8)
CN_GEN_ALL(uint16_t, uint16_t, cn_bits_u16)
CN_GEN_ALL(uint32_t, uint32_t, cn_bits_u32)
CN_GEN_ALL(uint64_t, uint64_t, cn_bits_u64)
CN_GEN_ALL(unsigned long, signed long, cn_integer)

CN_GEN_PTR_CASTS_SIGNED(int8_t, cn_bits_i8)
CN_GEN_PTR_CASTS_SIGNED(int16_t, cn_bits_i16)
CN_GEN_PTR_CASTS_SIGNED(int32_t, cn_bits_i32)
CN_GEN_PTR_CASTS_SIGNED(int64_t, cn_bits_i64)
CN_GEN_PTR_CASTS_UNSIGNED(uint8_t, cn_bits_u8)
CN_GEN_PTR_CASTS_UNSIGNED(uint16_t, cn_bits_u16)
CN_GEN_PTR_CASTS_UNSIGNED(uint32_t, cn_bits_u32)
CN_GEN_PTR_CASTS_UNSIGNED(uint64_t, cn_bits_u64)
CN_GEN_PTR_CASTS_SIGNED(signed long, cn_integer)

CN_GEN_BOX(cn_alloc_id)
CN_GEN_CONVERT(uint8_t, cn_alloc_id)
CN_GEN_CONVERT_FROM(uint8_t, cn_alloc_id)
CN_GEN_EQUALITY(cn_alloc_id)
CN_GEN_DEFAULT(cn_alloc_id)
CN_GEN_MAP_SET(cn_alloc_id)
CN_GEN_MAP_GET(cn_alloc_id)
CN_GEN_GHOST_ARG(cn_alloc_id)

CN_GEN_BOX(cn_bool)
CN_GEN_MAP_SET(cn_bool)
CN_GEN_MAP_GET(cn_bool)
CN_GEN_GHOST_ARG(cn_bool)

CN_GEN_BOX(cn_pointer)
CN_GEN_MAP_SET(cn_pointer)
CN_GEN_MAP_GET(cn_pointer)
CN_GEN_GHOST_ARG(cn_pointer)

/* Map-valued maps store the inner map's pointer directly */
static inline cn_map* cn_map_get_cn_map(cn_map* m, cn_integer key) {
  void* res = ht_get(m, &key);
  if (!res) {
    return default_cn_map();
  }
  return (cn_map*)res;
}

/* OWNERSHIP */

// GEN_ALL_STACK(cn_source_location, char*);

// typedef struct ownership_ghost_info {
//   int depth;
//   cn_source_location_stack*
//       source_loc_stack;  // set to null if ownership_stack_mode is disabled
// } ownership_ghost_info;

// enum STACK_OP {
//   PUSH,
//   POP,
//   NO_OP
// };

int ownership_ghost_state_get_depth(int64_t address);
void ownership_ghost_state_set(int64_t address,
    size_t size,
    int stack_depth_val,
    struct cn_error_message_info* error_msg_info);
void ownership_ghost_state_remove(int64_t address, size_t size);

/* CN ownership checking */
void cn_assume_ownership(void* generic_c_ptr, unsigned long size, char* fun);
void cn_get_or_put_ownership(enum spec_mode spec_mode,
    void* generic_c_ptr,
    size_t size,
    struct loop_ownership* loop_ownership);

/* C ownership checking */
void c_add_to_ghost_state(void* ptr_to_local, size_t size, signed long stack_depth);
void c_remove_from_ghost_state(void* ptr_to_local, size_t size);
enum region_owned c_ownership_check(char* access_kind,
    void* generic_c_ptr,
    int size,
    signed long expected_stack_depth,
    enum spec_mode spec_mode);

// Unused
#define c_concat_with_mapping_stat(STAT, CTYPE, VAR_NAME, GHOST_STATE, STACK_DEPTH)      \
  STAT;                                                                                  \
  c_add_to_ghost_state(&VAR_NAME, GHOST_STATE, sizeof(CTYPE), STACK_DEPTH);

#define c_declare_and_map_local(CTYPE, VAR_NAME)                                         \
  c_concat_with_mapping_stat(CTYPE VAR_NAME, CTYPE, VAR_NAME)

#define c_declare_init_and_map_local(CTYPE, VAR_NAME, EXPR)                              \
  c_concat_with_mapping_stat(CTYPE VAR_NAME = EXPR, CTYPE, VAR_NAME)
// /Unused

static inline void cn_load(void* ptr, size_t size) {
  //   cn_printf(CN_LOGGING_INFO, "  \x1b[31mLOAD\x1b[0m[%lu] - ptr: %p\n", size, ptr);
}
static inline void cn_store(void* ptr, size_t size) {
  //   cn_printf(CN_LOGGING_INFO, "  \x1b[31mSTORE\x1b[0m[%lu] - ptr: %p\n", size, ptr);
}
static inline void cn_postfix(void* ptr, size_t size) {
  //   cn_printf(CN_LOGGING_INFO, "  \x1b[31mPOSTFIX\x1b[0m[%lu] - ptr: %p\n", size, ptr);
}

// use this macro to wrap an argument to another macro that contains commas
#define CN_IGNORE_COMMA(...) __VA_ARGS__

#define CN_LOAD(LV)                                                                      \
  ({                                                                                     \
    typeof(LV)* __tmp = &(LV);                                                           \
    update_cn_error_message_info_access_check(0);                                        \
    c_ownership_check(                                                                   \
        "Load", __tmp, sizeof(typeof(LV)), get_cn_stack_depth(), C_ACCESS);              \
    cn_load(__tmp, sizeof(typeof(LV)));                                                  \
    cn_pop_msg_info();                                                                   \
    *__tmp;                                                                              \
  })

#define CN_STORE_OP(LV, op, X)                                                           \
  ({                                                                                     \
    typeof(LV)* __tmp;                                                                   \
    __tmp = &(LV);                                                                       \
    update_cn_error_message_info_access_check(0);                                        \
    c_ownership_check(                                                                   \
        "Store", __tmp, sizeof(typeof(LV)), get_cn_stack_depth(), C_ACCESS);             \
    cn_store(__tmp, sizeof(typeof(LV)));                                                 \
    cn_pop_msg_info();                                                                   \
    *__tmp op## = (X);                                                                   \
  })

#define CN_STORE(LV, X) CN_STORE_OP(LV, , X)

#define CN_POSTFIX(LV, OP)                                                               \
  ({                                                                                     \
    typeof(LV)* __tmp;                                                                   \
    __tmp = &(LV);                                                                       \
    update_cn_error_message_info_access_check(0);                                        \
    c_ownership_check(                                                                   \
        "Postfix operation", __tmp, sizeof(typeof(LV)), get_cn_stack_depth(), C_ACCESS); \
    cn_postfix(__tmp, sizeof(typeof(LV)));                                               \
    cn_pop_msg_info();                                                                   \
    (*__tmp) OP;                                                                         \
  })

#ifdef __CN_INSTRUMENT
  #undef size_t
  #undef uint8_t
  #undef uint16_t
  #undef uint32_t
  #undef uint64_t
  #undef uintptr_t
  #undef int8_t
  #undef int16_t
  #undef int32_t
  #undef int64_t
  #undef intptr_t
  #undef alignof
#endif

#endif  // CN_UTILS
