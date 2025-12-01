#ifndef FULMINATE_INTERNALS_H
#define FULMINATE_INTERNALS_H

#include "api.h"
#include "errors.h"
#include "rmap.h"
#include "stack.h"
#include "user.h"

#include <cn-executable/bump_alloc.h>
#include <cn-executable/eval.h>
#include <cn-executable/fulminate_alloc.h>
#include <cn-executable/hash_table.h>

enum region_owned {
  NO_WILDCARD,
  SOME_WILDCARD,
  FULL_WILDCARD,
};

void cn_print_nr_owned_predicates(void);

void initialise_ownership_ghost_state(void);
void free_ownership_ghost_state(void);
void initialise_ghost_stack_depth(void);
void initialise_exec_c_locs_mode(bool flag);
void initialise_ownership_stack_mode(bool flag);
signed long get_cn_stack_depth(void);
void ghost_stack_depth_incr(void);
void ghost_stack_depth_decr(void);
void cn_postcondition_leak_check(void);

struct loop_ownership* initialise_loop_ownership_state(void);
void cn_loop_leak_check(void);
void cn_loop_put_back_ownership(struct loop_ownership* loop_ownership);

void cn_print_nr_u64(int i, unsigned long u);
void cn_print_u64(const char* str, unsigned long u);
void dump_ownership_ghost_state(int stack_depth);
bool is_mapped(void* ptr);

void cn_assert(cn_bool* cn_b, enum spec_mode spec_mode);

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
enum region_owned c_ownership_check(
    char* access_kind, void* generic_c_ptr, int size, signed long expected_stack_depth);

/* Ghost arguments */
void alloc_ghost_array(int ghost_array_size);
void add_to_ghost_array(int i, void* ptr_to_ghost_arg);
void free_ghost_array(void);
void* load_from_ghost_array(int i);
void cn_ghost_arg_failure(void);

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
    c_ownership_check("Load", __tmp, sizeof(typeof(LV)), get_cn_stack_depth());          \
    cn_load(__tmp, sizeof(typeof(LV)));                                                  \
    *__tmp;                                                                              \
  })

#define CN_STORE_OP(LV, op, X)                                                           \
  ({                                                                                     \
    typeof(LV)* __tmp;                                                                   \
    __tmp = &(LV);                                                                       \
    update_cn_error_message_info_access_check(0);                                        \
    c_ownership_check("Store", __tmp, sizeof(typeof(LV)), get_cn_stack_depth());         \
    cn_store(__tmp, sizeof(typeof(LV)));                                                 \
    *__tmp op## = (X);                                                                   \
  })

#define CN_STORE(LV, X) CN_STORE_OP(LV, , X)

#define CN_POSTFIX(LV, OP)                                                               \
  ({                                                                                     \
    typeof(LV)* __tmp;                                                                   \
    __tmp = &(LV);                                                                       \
    update_cn_error_message_info_access_check(0);                                        \
    c_ownership_check(                                                                   \
        "Postfix operation", __tmp, sizeof(typeof(LV)), get_cn_stack_depth());           \
    cn_postfix(__tmp, sizeof(typeof(LV)));                                               \
    (*__tmp) OP;                                                                         \
  })

#endif  // FULMINATE_INTERNALS_H
