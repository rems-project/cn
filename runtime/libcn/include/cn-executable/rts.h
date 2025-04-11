#ifndef CN_RTS
#define CN_RTS

#include "cerb_types.h"
#include "rts_deps.h"
#include "alloc.h"
#include "hash_table.h"

// XXX: things used by injected code
#define true       1
#define false      0
#define NULL       0

#ifdef __cplusplus
extern "C" {
#endif

void reset_fulminate(void);

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
  const char *function_name;
  char *file_name;
  int line_number;
  char *cn_source_loc;
  struct cn_error_message_info *parent;
  struct cn_error_message_info *child;
};

void initialise_error_msg_info_(
    const char *function_name, char *file_name, int line_number);

void reset_error_msg_info();
void free_error_msg_info();

/* TODO: Implement */
/*struct cn_error_messages {
    struct cn_error_message_info *top_level_error_msg_info;
    struct cn_error_message_info *nested_error_msg_info;
};*/

void update_error_message_info_(
    const char *function_name, char *file_name, int line_number, char *cn_source_loc);

void cn_pop_msg_info();


/* Wrappers for C types */

/* Signed bitvectors */

typedef struct cn_bits_i8 {
  int8_t val;
} cn_bits_i8;

typedef struct cn_bits_i16 {
  int16_t val;
} cn_bits_i16;

typedef struct cn_bits_i32 {
  int32_t val;
} cn_bits_i32;

typedef struct cn_bits_i64 {
  int64_t val;
} cn_bits_i64;

/* Unsigned bitvectors */

typedef struct cn_bits_u8 {
  uint8_t val;
} cn_bits_u8;

typedef struct cn_bits_u16 {
  uint16_t val;
} cn_bits_u16;

typedef struct cn_bits_u32 {
  uint32_t val;
} cn_bits_u32;

typedef struct cn_bits_u64 {
  uint64_t val;
} cn_bits_u64;

typedef struct cn_integer {
  int64_t val;
} cn_integer;

typedef struct cn_pointer {
  void *ptr;
} cn_pointer;

typedef struct cn_bool {
  _Bool val;
} cn_bool;

typedef struct cn_alloc_id {
  uint8_t val;
} cn_alloc_id;

typedef hash_table cn_map;

void initialise_ownership_ghost_state(void);
void free_ownership_ghost_state(void);
void initialise_ghost_stack_depth(void);
signed long get_cn_stack_depth(void);
void ghost_stack_depth_incr(void);
void ghost_stack_depth_decr(void);
void cn_postcondition_leak_check(void);
void cn_loop_put_back_ownership(void);
void cn_loop_leak_check_and_put_back_ownership(void);

/* malloc, free */
void *cn_aligned_alloc(size_t align, size_t size);
void *cn_malloc(unsigned long size);
void *cn_calloc(size_t num, size_t size);
void cn_free_sized(void *, size_t len);

void cn_print_nr_u64(int i, unsigned long u);
void cn_print_u64(const char *str, unsigned long u);

/* cn_failure callbacks */
enum cn_failure_mode {
  CN_FAILURE_ASSERT = 1,
  CN_FAILURE_CHECK_OWNERSHIP,
  CN_FAILURE_OWNERSHIP_LEAK,
  CN_FAILURE_ALLOC
};

typedef void (*cn_failure_callback)(enum cn_failure_mode);
void set_cn_failure_cb(cn_failure_callback callback);
void reset_cn_failure_cb(void);
void cn_failure(enum cn_failure_mode mode);

/* Conversion functions */

cn_bool *convert_to_cn_bool(_Bool b);
_Bool convert_from_cn_bool(cn_bool *b);
void cn_assert(cn_bool *cn_b);
cn_bool *cn_bool_and(cn_bool *b1, cn_bool *b2);
cn_bool *cn_bool_or(cn_bool *b1, cn_bool *b2);
cn_bool *cn_bool_not(cn_bool *b);
cn_bool *cn_bool_implies(cn_bool *b1, cn_bool *b2);
cn_bool *cn_bool_equality(cn_bool *b1, cn_bool *b2);
void *cn_ite(cn_bool *b, void *e1, void *e2);

cn_map *map_create(void);
cn_map *cn_map_set(cn_map *m, cn_integer *key, void *value);
cn_map *cn_map_deep_copy(cn_map *m1);
cn_bool *cn_map_equality(
    cn_map *m1, cn_map *m2, cn_bool *(value_equality_fun)(void *, void *));
// TODO (RB) does this need to be in here, or should it be auto-generated?
// See https://github.com/rems-project/cerberus/pull/652 for details
cn_bool *void_pointer_equality(void *p1, void *p2);

cn_bool *cn_pointer_equality(void *i1, void *i2);
cn_bool *cn_pointer_is_null(cn_pointer *);
cn_bool *cn_pointer_le(cn_pointer *i1, cn_pointer *i2);
cn_bool *cn_pointer_lt(cn_pointer *i1, cn_pointer *i2);
cn_bool *cn_pointer_ge(cn_pointer *i1, cn_pointer *i2);
cn_bool *cn_pointer_(cn_pointer *i1, cn_pointer *i2);


#define PRIM_DEF(x) ;
#include "prims.h"
#undef PRIM_DEF


cn_bits_u32 *cn_bits_u32_fls(cn_bits_u32 *i1);
cn_bits_u64 *cn_bits_u64_flsl(cn_bits_u64 *i1);

cn_map *default_cn_map(void);
cn_bool *default_cn_bool(void);

cn_pointer *convert_to_cn_pointer(void *ptr);
void *convert_from_cn_pointer(cn_pointer *cn_ptr);
cn_pointer *cn_pointer_add(cn_pointer *ptr, cn_integer *i);
cn_pointer *cast_cn_pointer_to_cn_pointer(cn_pointer *p);


/* OWNERSHIP */

enum OWNERSHIP {
  GET,
  PUT,
  LOOP
};

int ownership_ghost_state_get(int64_t *address_key);
void ownership_ghost_state_set(int64_t *address_key, int stack_depth_val);
void ownership_ghost_state_remove(int64_t *address_key);

/* CN ownership checking */
void cn_get_ownership(uintptr_t generic_c_ptr, size_t size, char *check_msg);
void cn_put_ownership(uintptr_t generic_c_ptr, size_t size);
void cn_assume_ownership(void *generic_c_ptr, unsigned long size, char *fun);
void cn_get_or_put_ownership(
    enum OWNERSHIP owned_enum, uintptr_t generic_c_ptr, size_t size);

/* C ownership checking */
void c_add_to_ghost_state(uintptr_t ptr_to_local, size_t size, signed long stack_depth);
void c_remove_from_ghost_state(uintptr_t ptr_to_local, size_t size);
void c_ownership_check(char *access_kind,
    uintptr_t generic_c_ptr,
    int offset,
    signed long expected_stack_depth);

    #define initialise_error_msg_info()                                                      \
    initialise_error_msg_info_(__func__, __FILE__, __LINE__)
  
  #define update_cn_error_message_info(x)                                                  \
    update_error_message_info_(__func__, __FILE__, __LINE__ + 1, x)
  
  #define update_cn_error_message_info_access_check(x)                                     \
    update_error_message_info_(__func__, __FILE__, __LINE__, x)
  
  #define convert_to_cn_map(c_ptr, cntype_conversion_fn, num_elements)                     \
    ({                                                                                     \
      cn_map *m = map_create();                                                            \
      for (int i = 0; i < num_elements; i++) {                                             \
        cn_map_set(m, convert_to_cn_integer(i), cntype_conversion_fn(c_ptr[i]));           \
      }                                                                                    \
      m;                                                                                   \
    })
  #define convert_from_cn_map(arr, m, cntype, num_elements)                                \
    for (int i = 0; i < num_elements; i++) {                                               \
      arr[i] = convert_from_##cntype(cn_map_get_##cntype(m, convert_to_cn_integer(i)));    \
    }
  
  
  #define cn_pointer_deref(CN_PTR, CTYPE) *((CTYPE *)CN_PTR->ptr)
  
  #define cn_array_shift(cn_ptr, size, index)                                              \
    convert_to_cn_pointer((char *)cn_ptr->ptr + (index->val * size))
  
  #define cn_member_shift(cn_ptr, tag, member_name)                                        \
    convert_to_cn_pointer(&(((struct tag *)cn_ptr->ptr)->member_name))
  
  // use this macro to wrap an argument to another macro that contains commas
  #define CN_IGNORE_COMMA(...) __VA_ARGS__
  
  #define CN_LOAD(LV)                                                                      \
    ({                                                                                     \
      typeof(LV) *__tmp = &(LV);                                                           \
      update_cn_error_message_info_access_check(NULL);                                     \
      c_ownership_check(                                                                   \
          "Load", (uintptr_t)__tmp, sizeof(typeof(LV)), get_cn_stack_depth());             \
      cn_load(__tmp, sizeof(typeof(LV)));                                                  \
      *__tmp;                                                                              \
    })
  
  #define CN_STORE_OP(LV, op, X)                                                           \
    ({                                                                                     \
      typeof(LV) *__tmp;                                                                   \
      __tmp = &(LV);                                                                       \
      update_cn_error_message_info_access_check(NULL);                                     \
      c_ownership_check(                                                                   \
          "Store", (uintptr_t)__tmp, sizeof(typeof(LV)), get_cn_stack_depth());            \
      cn_store(__tmp, sizeof(typeof(LV)));                                                 \
      *__tmp op## = (X);                                                                   \
    })
  
  #define CN_STORE(LV, X) CN_STORE_OP(LV, , X)
  
  #define CN_POSTFIX(LV, OP)                                                               \
    ({                                                                                     \
      typeof(LV) *__tmp;                                                                   \
      __tmp = &(LV);                                                                       \
      update_cn_error_message_info_access_check(NULL);                                     \
      c_ownership_check("Postfix operation",                                               \
          (uintptr_t)__tmp,                                                                \
          sizeof(typeof(LV)),                                                              \
          get_cn_stack_depth());                                                           \
      cn_postfix(__tmp, sizeof(typeof(LV)));                                               \
      (*__tmp) OP;                                                                         \
    })


#ifdef CN_INSTRUMENTATION_MODE
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
#endif

#ifdef __cplusplus
}
#endif


/* IMPORTANT: This should be the last declaration in the module.
It is used to determine where to begin injecting CN declarations. */
extern void cn_end_of_header();

#endif  // CN_UTILS
