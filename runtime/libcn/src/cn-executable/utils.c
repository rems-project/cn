#include <inttypes.h>
#include <limits.h>
#include <signal.h>  // for SIGABRT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <cn-executable/utils.h>

typedef hash_table ownership_ghost_state;

ownership_ghost_state* cn_ownership_global_ghost_state;

struct cn_error_message_info* global_error_msg_info;

struct cn_error_message_info* no_error_msg_info = 0;

signed long cn_stack_depth;

signed long nr_owned_predicates;

_Bool exec_c_locs_mode;
_Bool ownership_stack_mode;

static signed long UNMAPPED_VAL = -1;
static signed long WILDCARD_DEPTH = INT_MIN + 1;

static allocator bump_alloc = (allocator){
    .malloc = &cn_bump_malloc, .calloc = &cn_bump_calloc, .free = &cn_bump_free};

void reset_fulminate(void) {
  cn_bump_free_all();
  free_ownership_ghost_state();
  reset_error_msg_info();
  initialise_ownership_ghost_state();
  initialise_ghost_stack_depth();
  initialise_exec_c_locs_mode(0);
  initialise_ownership_stack_mode(0);
}

static enum cn_logging_level logging_level = CN_LOGGING_INFO;

enum cn_logging_level get_cn_logging_level(void) {
  return logging_level;
}

enum cn_logging_level set_cn_logging_level(enum cn_logging_level new_level) {
  enum cn_logging_level old_level = logging_level;
  logging_level = new_level;
  return old_level;
}

void cn_failure_default(enum cn_failure_mode failure_mode, enum spec_mode spec_mode) {
  int exit_code =
      spec_mode;  // Might need to differentiate between failure modes via this exit code in the future
  switch (failure_mode) {
    case CN_FAILURE_ALLOC:
      printf("Out of memory!");
    case CN_FAILURE_ASSERT:
    case CN_FAILURE_CHECK_OWNERSHIP:
    case CN_FAILURE_OWNERSHIP_LEAK:
    case CN_FAILURE_GHOST_ARGS:
      exit(exit_code);
  }
}

static cn_failure_callback cn_failure_aux = &cn_failure_default;

void cn_failure(enum cn_failure_mode failure_mode, enum spec_mode spec_mode) {
  cn_failure_aux(failure_mode, spec_mode);
}

void set_cn_failure_cb(cn_failure_callback callback) {
  cn_failure_aux = callback;
}

void reset_cn_failure_cb(void) {
  cn_failure_aux = &cn_failure_default;
}

static enum cn_trace_granularity trace_granularity = CN_TRACE_NONE;

enum cn_trace_granularity get_cn_trace_granularity(void) {
  return trace_granularity;
}

enum cn_trace_granularity set_cn_trace_granularity(
    enum cn_trace_granularity new_granularity) {
  enum cn_trace_granularity old_granularity = trace_granularity;
  trace_granularity = new_granularity;
  return old_granularity;
}

void print_error_msg_info_single(struct cn_error_message_info* info) {
  if (exec_c_locs_mode) {
    cn_printf(CN_LOGGING_ERROR,
        "function %s, file %s, line %d\n",
        info->function_name,
        info->file_name,
        info->line_number);
  } else {
    if (info->cn_source_loc) {
      cn_printf(
          CN_LOGGING_ERROR, "original source location: \n%s\n\n", info->cn_source_loc);
    } else {
      cn_printf(CN_LOGGING_ERROR,
          "no source location found (try running with --exec-c-locs-mode enabled)")
    }
  }
}

void print_error_msg_info(struct cn_error_message_info* info) {
  if (info) {
    enum cn_trace_granularity granularity = get_cn_trace_granularity();
    if (granularity != CN_TRACE_NONE && info->parent != NULL) {
      struct cn_error_message_info* curr = info;
      while (curr->parent != NULL) {
        curr = curr->parent;
      }

      cn_printf(CN_LOGGING_ERROR,
          "********************* Originated from **********************\n");
      print_error_msg_info_single(curr);
      curr = curr->child;

      while (granularity > CN_TRACE_ENDS && curr->child != NULL) {
        cn_printf(CN_LOGGING_ERROR,
            "************************************************************\n");
        print_error_msg_info_single(curr);
        curr = curr->child;
      }
    }

    cn_printf(CN_LOGGING_ERROR,
        "************************ Failed at *************************\n");
    print_error_msg_info_single(info);
  } else {
    cn_printf(CN_LOGGING_ERROR, "Internal error: no error_msg_info available.");
    exit(SIGABRT);
  }
}

void print_owned_calls_stack(ownership_ghost_info* entry_maybe) {
  if (entry_maybe && entry_maybe->source_loc_stack) {
    cn_printf(CN_LOGGING_ERROR, "Owned locations stack:\n");
    char* popped_elem =
        cn_source_location_stack_pop(entry_maybe->source_loc_stack, &fulm_default_alloc);
    while (popped_elem) {
      cn_printf(CN_LOGGING_ERROR, "%s\n", popped_elem);
      cn_printf(CN_LOGGING_ERROR, "==========================\n");
      popped_elem = cn_source_location_stack_pop(
          entry_maybe->source_loc_stack, &fulm_default_alloc);
    }
  }
}

ownership_ghost_info* create_ownership_ghost_state_entry(int depth) {
  ownership_ghost_info* ghost_state_entry =
      fulm_malloc(sizeof(ownership_ghost_info), &fulm_default_alloc);
  ghost_state_entry->depth = depth;
  return ghost_state_entry;
}

cn_bool* convert_to_cn_bool(_Bool b) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  if (!res)
    exit(1);
  res->val = b;
  return res;
}

_Bool convert_from_cn_bool(cn_bool* b) {
  return b->val;
}

void cn_assert(cn_bool* cn_b, enum spec_mode spec_mode) {
  // cn_printf(CN_LOGGING_INFO, "[CN: assertion] function %s, file %s, line %d\n", error_msg_info.function_name, error_msg_info.file_name, error_msg_info.line_number);
  if (!(cn_b->val)) {
    print_error_msg_info(global_error_msg_info);
    cn_failure(CN_FAILURE_ASSERT, spec_mode);
  }
}

cn_bool* cn_bool_and(cn_bool* b1, cn_bool* b2) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = b1->val && b2->val;
  return res;
}

cn_bool* cn_bool_or(cn_bool* b1, cn_bool* b2) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = b1->val || b2->val;
  return res;
}

cn_bool* cn_bool_implies(cn_bool* b1, cn_bool* b2) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = !b1->val || b2->val;
  return res;
}

cn_bool* cn_bool_not(cn_bool* b) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = !(b->val);
  return res;
}

cn_bool* cn_bool_equality(cn_bool* b1, cn_bool* b2) {
  return convert_to_cn_bool(b1->val == b2->val);
}

void* cn_ite(cn_bool* b, void* e1, void* e2) {
  return b->val ? e1 : e2;
}

cn_map* map_create(void) {
  return ht_create(&bump_alloc);
}

void initialise_ownership_ghost_state(void) {
  nr_owned_predicates = 0;
  cn_ownership_global_ghost_state = ht_create(&fulm_default_alloc);
}

void free_ownership_ghost_state(void) {
  nr_owned_predicates = 0;
  ht_destroy(cn_ownership_global_ghost_state);
}

void initialise_ghost_stack_depth(void) {
  cn_stack_depth = 0;
}

void initialise_exec_c_locs_mode(_Bool flag) {
  exec_c_locs_mode = flag;
}

void initialise_ownership_stack_mode(_Bool flag) {
  ownership_stack_mode = flag;
}

signed long get_cn_stack_depth(void) {
  return cn_stack_depth;
}

void ghost_stack_depth_incr(void) {
  cn_stack_depth++;
}

// TODO: one of these should maybe go away
#define FMT_PTR "\x1b[33m%#" PRIxPTR "\x1b[0m"
// #define KMAG  "\x1B[35m"
#define FMT_PTR_2 "\x1B[35m%#" PRIxPTR "\x1B[0m"

void ghost_stack_depth_decr(void) {
  cn_stack_depth--;
  // update_error_message_info(0);
  // print_error_msg_info();

  // cn_printf(CN_LOGGING_INFO, "\n");
}

/* TODO: DAVID
1. Uncomment + add max value lookups to first 2 functions
2. Delete the 2 functions after that (old implementations)
*/

// void cn_postcondition_leak_check(void) {
//   signed long max_depth_in_ghost_state =
//       /* get_max_value(cn_ownership_global_ghost_state) */ INT_MAX - 1;  // TODO: Change
//   if (max_depth_in_ghost_state > cn_stack_depth) {
//     print_error_msg_info(error_msg_info);
//     // XXX: This appears to print the *hashed* pointer?
//     cn_printf(CN_LOGGING_ERROR,
//         "Postcondition leak check failed, ownership leaked for pointer " FMT_PTR "\n",
//         (uintptr_t)*key);
//     cn_failure(CN_FAILURE_OWNERSHIP_LEAK, POST);
//   }
// }
//
// void cn_loop_leak_check(void) {
//   signed long max_depth_in_ghost_state =
//       /* get_max_value(cn_ownership_global_ghost_state) */ INT_MAX - 1;  // TODO: Change
//   if (max_depth_in_ghost_state == cn_stack_depth) {
//     print_error_msg_info(error_msg_info);
//     // XXX: This appears to print the *hashed* pointer?
//     cn_printf(CN_LOGGING_ERROR,
//         "Loop invariant leak check failed, ownership leaked for pointer " FMT_PTR "\n",
//         (uintptr_t)*key);
//     cn_failure(CN_FAILURE_OWNERSHIP_LEAK, LOOP);
//   }
// }

void cn_postcondition_leak_check(void) {
  // leak checking
  hash_table_iterator it = ht_iterator(cn_ownership_global_ghost_state);
  // cn_printf(CN_LOGGING_INFO, "CN pointers leaked at (%ld) stack-depth: ", cn_stack_depth);
  while (ht_next(&it)) {
    int64_t* key = it.key;
    ownership_ghost_info* info = (ownership_ghost_info*)it.value;
    if (info->depth != WILDCARD_DEPTH && info->depth > cn_stack_depth) {
      if (ownership_stack_mode) {
        print_owned_calls_stack(info);
      }
      print_error_msg_info(global_error_msg_info);
      // XXX: This appears to print the *hashed* pointer?
      cn_printf(CN_LOGGING_ERROR,
          "Postcondition leak check failed, ownership leaked for pointer " FMT_PTR "\n",
          (uintptr_t)*key);
      cn_failure(CN_FAILURE_OWNERSHIP_LEAK, POST);
      // cn_printf(CN_LOGGING_INFO, FMT_PTR_2 " (%d),", *key, *depth);
    }
  }
}

void cn_loop_leak_check(void) {
  hash_table_iterator it = ht_iterator(cn_ownership_global_ghost_state);

  while (ht_next(&it)) {
    int64_t* key = it.key;
    ownership_ghost_info* info = (ownership_ghost_info*)it.value;
    /* Everything mapped to the function stack depth should have been bumped up by calls to Owned in invariant */
    if (info->depth != WILDCARD_DEPTH && info->depth == cn_stack_depth) {
      if (ownership_stack_mode) {
        print_owned_calls_stack(info);
      }
      print_error_msg_info(global_error_msg_info);
      // XXX: This appears to print the *hashed* pointer?
      cn_printf(CN_LOGGING_ERROR,
          "Loop invariant leak check failed, ownership leaked for pointer " FMT_PTR "\n",
          (uintptr_t)*key);
      cn_failure(CN_FAILURE_OWNERSHIP_LEAK, LOOP);
      // cn_printf(CN_LOGGING_INFO, FMT_PTR_2 " (%d),", *key, *depth);
    }
  }
}

/* / TODO: DAVID */

typedef struct loop_ownership_nd {
  uintptr_t addr;
  size_t size;
  struct loop_ownership_nd* next;
} loop_ownership_nd;

struct loop_ownership {
  loop_ownership_nd* head;
};

// No destructors: expects to be dropped by the bump allocator _in a timely
// manner_.
struct loop_ownership* initialise_loop_ownership_state(void) {
  struct loop_ownership* loop_ownership =
      bump_alloc.malloc(sizeof(struct loop_ownership));
  // assert(loop_ownership);
  *loop_ownership = (struct loop_ownership){.head = NULL};
  return loop_ownership;
}

// XXX: Compresses explicit ranges. Catches iteratively fed ranges.
// To improve: coalesce with any known range, rather than just the last.
void cn_add_to_loop_ownership_state(
    void* generic_c_ptr, size_t size, struct loop_ownership* loop_ownership) {
  uintptr_t addr = (uintptr_t)generic_c_ptr;
  loop_ownership_nd* hd = loop_ownership->head;
  if (hd && addr == (hd->addr + hd->size))
    hd->size += size;
  else {
    loop_ownership_nd* newhd = bump_alloc.malloc(sizeof(loop_ownership_nd));
    *newhd = (loop_ownership_nd){.addr = addr, .size = size, .next = hd};
    loop_ownership->head = newhd;
  }
}

void cn_loop_put_back_ownership(struct loop_ownership* loop_ownership) {
  for (loop_ownership_nd* nd = loop_ownership->head; nd; nd = nd->next) {
    ownership_ghost_state_set(nd->addr, nd->size, cn_stack_depth, no_error_msg_info, POP);
  }
}

ownership_ghost_info* ownership_ghost_state_get(int64_t address) {
  return (ownership_ghost_info*)ht_get(cn_ownership_global_ghost_state, &address);
}

int ownership_ghost_state_get_depth(int64_t address) {
  ownership_ghost_info* entry_maybe = ownership_ghost_state_get(address);
  return entry_maybe ? entry_maybe->depth : UNMAPPED_VAL;
}

void ownership_ghost_state_set(int64_t address,
    size_t size,
    int stack_depth_val,
    struct cn_error_message_info* error_msg_info,
    enum STACK_OP op) {
  for (int64_t k = address; k < address + size; k++) {
    ownership_ghost_info* entry =
        (ownership_ghost_info*)ht_get(cn_ownership_global_ghost_state, &k);
    if (!entry) {
      entry = fulm_malloc(sizeof(ownership_ghost_info), &fulm_default_alloc);
      if (ownership_stack_mode) {
        entry->source_loc_stack = cn_source_location_stack_init(&fulm_default_alloc);
      } else {
        entry->source_loc_stack = 0;
      }
    }
    entry->depth = stack_depth_val;
    if (ownership_stack_mode) {
      switch (op) {
        case NO_OP:
          break;
        case PUSH: {
          if (error_msg_info) {
            cn_source_location_stack_push(entry->source_loc_stack,
                error_msg_info->cn_source_loc,
                &fulm_default_alloc);
          }
          break;
        }
        case POP: {
          cn_source_location_stack_pop(entry->source_loc_stack, &fulm_default_alloc);
          break;
        }
      }
    }
    ht_set(cn_ownership_global_ghost_state, &k, entry);
  }
}

void ownership_ghost_state_remove(int64_t address, size_t size) {
  ownership_ghost_state_set(address, size, UNMAPPED_VAL, no_error_msg_info, NO_OP);
}

_Bool is_wildcard(void* generic_c_ptr, int offset) {
  int64_t address_key = 0;
  // cn_printf(CN_LOGGING_INFO, "C: Checking ownership for [ " FMT_PTR " .. " FMT_PTR " ] -- ", generic_c_ptr, generic_c_ptr + offset);
  for (int i = 0; i < offset; i++) {
    address_key = (uintptr_t)generic_c_ptr + i;
    int depth = ownership_ghost_state_get_depth(address_key);
    if (depth != WILDCARD_DEPTH) {
      return 0;
    }
  }
  return 1;
}

/* Calls to this generated by Fulminate for stack-local variable ownership checking */
void c_add_to_ghost_state(void* ptr_to_local, size_t size, signed long stack_depth) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] add local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_set(
      (uintptr_t)ptr_to_local, size, stack_depth, global_error_msg_info, PUSH);
}

/* Only called internally for tracking ownership of heap addresses */
void c_add_to_ghost_state_internal(void* ptr_to_local,
    size_t size,
    signed long stack_depth,
    struct cn_error_message_info* error_msg_info,
    enum STACK_OP op) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] add local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_set(
      (uintptr_t)ptr_to_local, size, stack_depth, error_msg_info, op);
}

void c_remove_from_ghost_state(void* ptr_to_local, size_t size) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] remove local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_remove((uintptr_t)ptr_to_local, size);
}

void cn_get_ownership(void* generic_c_ptr, size_t size) {
  /* Used for precondition and loop invariant taking/getting of ownership */
  c_ownership_check(
      "Precondition ownership check", generic_c_ptr, (int)size, cn_stack_depth - 1);
  c_add_to_ghost_state_internal(
      generic_c_ptr, size, cn_stack_depth, global_error_msg_info, PUSH);
}

void cn_loop_get_ownership(
    void* generic_c_ptr, size_t size, struct loop_ownership* loop_ownership) {
  c_ownership_check(
      "Loop invariant ownership check", generic_c_ptr, (int)size, cn_stack_depth);
  c_add_to_ghost_state_internal(
      generic_c_ptr, size, cn_stack_depth - 1, global_error_msg_info, PUSH);
  if (loop_ownership) {
    cn_add_to_loop_ownership_state(generic_c_ptr, size, loop_ownership);
  } else {
    // Should never be triggered if Fulminate implementation is correct
    cn_printf(CN_LOGGING_ERROR, "******** FULMINATE ERROR *********\n");
    cn_printf(CN_LOGGING_ERROR, "No loop ownership state found.\n");
    cn_printf(CN_LOGGING_ERROR,
        "Please open a pull request at https://github.com/rems-project/cn\n");
    exit(1);
  }
}

void cn_put_ownership(void* generic_c_ptr, size_t size) {
  // cn_printf(CN_LOGGING_INFO, "[CN: returning ownership] " FMT_PTR_2 ", size: %lu\n", generic_c_ptr, size);
  //// print_error_msg_info();
  c_ownership_check(
      "Postcondition ownership check", generic_c_ptr, (int)size, cn_stack_depth);
  c_add_to_ghost_state_internal(
      generic_c_ptr, size, cn_stack_depth - 1, no_error_msg_info, POP);
}

void fulminate_assume_ownership(
    void* generic_c_ptr, unsigned long size, char* fun, _Bool wildcard) {
  signed long depth = wildcard ? WILDCARD_DEPTH : cn_stack_depth;
  ownership_ghost_state_set(
      (uintptr_t)generic_c_ptr, size, depth, global_error_msg_info, PUSH);
  /* // cn_printf(CN_LOGGING_INFO, "CN: Assuming ownership for %lu (function: %s)\n",  */
  /*        ((uintptr_t) generic_c_ptr) + i, fun); */
}

void cn_assume_ownership(void* generic_c_ptr, unsigned long size, char* fun) {
  // cn_printf(CN_LOGGING_INFO, "[CN: assuming ownership (%s)] " FMT_PTR_2 ", size: %lu\n", fun, (uintptr_t) generic_c_ptr, size);
  //// print_error_msg_info();
  fulminate_assume_ownership(generic_c_ptr, size, fun, false);
}

void cn_get_or_put_ownership(enum spec_mode spec_mode,
    void* generic_c_ptr,
    size_t size,
    struct loop_ownership* loop_ownership) {
  nr_owned_predicates++;
  if (!is_wildcard(generic_c_ptr, (int)size)) {
    switch (spec_mode) {
      case PRE: {
        cn_get_ownership(generic_c_ptr, size);
        break;
      }
      case POST: {
        cn_put_ownership(generic_c_ptr, size);
        break;
      }
      case LOOP: {
        cn_loop_get_ownership(generic_c_ptr, size, loop_ownership);
      }
      default: {
        break;
      }
    }
  }
}

void c_ownership_check(char* access_kind,
    void* generic_c_ptr,
    int offset,
    signed long expected_stack_depth) {
  // cn_printf(CN_LOGGING_INFO, "C: Checking ownership for [ " FMT_PTR " .. " FMT_PTR " ] -- ", generic_c_ptr, generic_c_ptr + offset);
  for (int i = 0; i < offset; i++) {
    ownership_ghost_info* entry_maybe =
        ownership_ghost_state_get((uintptr_t)generic_c_ptr + i);
    int curr_depth = entry_maybe ? entry_maybe->depth : UNMAPPED_VAL;
    if (curr_depth != WILDCARD_DEPTH && curr_depth != expected_stack_depth) {
      if (ownership_stack_mode) {
        print_owned_calls_stack(entry_maybe);
      }
      print_error_msg_info(global_error_msg_info);
      cn_printf(CN_LOGGING_ERROR, "%s failed.\n", access_kind);
      if (curr_depth == UNMAPPED_VAL) {
        cn_printf(CN_LOGGING_ERROR,
            "  ==> " FMT_PTR "[%d] (" FMT_PTR ") not owned\n",
            (uintptr_t)generic_c_ptr,
            i,
            (uintptr_t)((char*)generic_c_ptr + i));
      } else {
        cn_printf(CN_LOGGING_ERROR,
            "  ==> " FMT_PTR "[%d] (" FMT_PTR
            ") not owned at expected function call stack depth %ld\n",
            (uintptr_t)generic_c_ptr,
            i,
            (uintptr_t)((char*)generic_c_ptr + i),
            expected_stack_depth);
        cn_printf(CN_LOGGING_ERROR, "  ==> (owned at stack depth: %d)\n", curr_depth);
      }
      cn_failure(CN_FAILURE_CHECK_OWNERSHIP, C_ACCESS);
    }
  }
  // cn_printf(CN_LOGGING_INFO, "\n");
}

// TODO: Reimplement for new ownership ghost state data structure
void dump_ownership_ghost_state(int stack_depth) {
  // hash_table_iterator it = ht_iterator(cn_ownership_global_ghost_state);
  // cn_printf(CN_LOGGING_INFO, "ADDRESS \t\t\tCN STACK DEPTH\n")
  //     cn_printf(CN_LOGGING_INFO, "====================\n") while (ht_next(&it)) {
  //   int64_t* key = it.key;
  //   int* depth = it.value;
  //   if (*depth == stack_depth) {
  //     cn_printf(CN_LOGGING_INFO, "[%p] => depth: %d\n", (void*)*key, *depth);
  //   }
  // }
}

// TODO: Reimplement for new ownership ghost state data structure
_Bool is_mapped(void* ptr) {
  // hash_table_iterator it = ht_iterator(cn_ownership_global_ghost_state);
  // while (ht_next(&it)) {
  //   int64_t* key = it.key;
  //   if (*key == (int64_t)ptr) {
  //     int* depth = it.value;
  //     cn_printf(CN_LOGGING_INFO, "[%p] => depth: %d\n", (void*)*key, *depth);
  //     return true;
  //   }
  // }
  return false;
}

/* TODO: Need address of and size of every stack-allocated variable - could store in struct and pass through. But this is an optimisation */
// void c_map_locals_to_stack_depth(ownership_ghost_state *cn_ownership_global_ghost_state, size_t size, int cn_stack_depth, ...) {
//     va_list args;

//     va_start(args, n);

//     for (int i = 0; i < n; i++) {
//         uintptr_t fn_local_ptr = va_arg(args, uintptr_t);
//         signed long address_key = fn_local_ptr;
//         ghost_state_set(cn_ownership_global_ghost_state, &address_key, cn_stack_depth);
//         fulminate_free(address_key);
//     }

//     va_end(args);
// }

cn_map* cn_map_set(cn_map* m, cn_integer* key, void* value) {
  ht_set(m, &key->val, value);
  return m;
}

cn_map* cn_map_deep_copy(cn_map* m1) {
  cn_map* m2 = map_create();

  hash_table_iterator hti = ht_iterator(m1);

  while (ht_next(&hti)) {
    int64_t* curr_key = hti.key;
    void* val = ht_get(m1, curr_key);
    ht_set(m2, curr_key, val);
  }

  return m2;
}

cn_map* default_cn_map(void) {
  return map_create();
}

cn_bool* default_cn_bool(void) {
  return convert_to_cn_bool(false);
}

cn_bool* cn_pointer_equality(void* i1, void* i2) {
  return convert_to_cn_bool((((cn_pointer*)i1)->ptr) == (((cn_pointer*)i2)->ptr));
}

cn_bool* cn_pointer_is_null(cn_pointer* p) {
  return convert_to_cn_bool(p->ptr == NULL);
}

cn_bool* cn_pointer_lt(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr < p2->ptr);
}

cn_bool* cn_pointer_le(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr <= p2->ptr);
}

cn_bool* cn_pointer_gt(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr > p2->ptr);
}

cn_bool* cn_pointer_ge(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr >= p2->ptr);
}

cn_pointer* cast_cn_pointer_to_cn_pointer(cn_pointer* p) {
  return p;
}

// Check if m2 is a subset of m1
cn_bool* cn_map_subset(
    cn_map* m1, cn_map* m2, cn_bool*(value_equality_fun)(void*, void*)) {
  if (ht_size(m1) != ht_size(m2))
    return convert_to_cn_bool(0);

  hash_table_iterator hti1 = ht_iterator(m1);

  while (ht_next(&hti1)) {
    int64_t* curr_key = hti1.key;
    void* val1 = ht_get(m1, curr_key);
    void* val2 = ht_get(m2, curr_key);

    /* Check if other map has this key at all */
    if (!val2)
      return convert_to_cn_bool(0);

    if (convert_from_cn_bool(cn_bool_not(value_equality_fun(val1, val2)))) {
      // cn_printf(CN_LOGGING_INFO, "Values not equal!\n");
      return convert_to_cn_bool(0);
    }
  }

  return convert_to_cn_bool(1);
}

cn_bool* cn_map_equality(
    cn_map* m1, cn_map* m2, cn_bool*(value_equality_fun)(void*, void*)) {
  return cn_bool_and(cn_map_subset(m1, m2, value_equality_fun),
      cn_map_subset(m2, m1, value_equality_fun));
}

cn_pointer* convert_to_cn_pointer(const void* ptr) {
  cn_pointer* res = (cn_pointer*)cn_bump_malloc(sizeof(cn_pointer));
  res->ptr = (void*)ptr;  // Carries around an address
  return res;
}

void* convert_from_cn_pointer(cn_pointer* cn_ptr) {
  return cn_ptr->ptr;
}

cn_pointer* cn_pointer_min(cn_pointer* p, cn_pointer* q) {
  uintptr_t p_raw = (uintptr_t)convert_from_cn_pointer(p);
  uintptr_t q_raw = (uintptr_t)convert_from_cn_pointer(q);
  return convert_to_cn_pointer((void*)(p_raw < q_raw ? p_raw : q_raw));
}

cn_pointer* cn_pointer_max(cn_pointer* p, cn_pointer* q) {
  uintptr_t p_raw = (uintptr_t)convert_from_cn_pointer(p);
  uintptr_t q_raw = (uintptr_t)convert_from_cn_pointer(q);
  return convert_to_cn_pointer((void*)(p_raw > q_raw ? p_raw : q_raw));
}

cn_pointer* cn_pointer_mod(cn_pointer* ptr, cn_pointer* n) {
  uintptr_t ptr_raw = (uintptr_t)convert_from_cn_pointer(ptr);
  uintptr_t n_raw = (uintptr_t)convert_from_cn_pointer(n);
  return convert_to_cn_pointer((void*)(ptr_raw % n_raw));
}

struct cn_error_message_info* make_error_message_info_entry(const char* function_name,
    char* file_name,
    int line_number,
    char* cn_source_loc,
    struct cn_error_message_info* parent) {
  struct cn_error_message_info* entry =
      fulm_malloc(sizeof(struct cn_error_message_info), &fulm_default_alloc);
  entry->function_name = function_name;
  entry->file_name = file_name;
  entry->line_number = line_number;
  entry->cn_source_loc = cn_source_loc;
  entry->parent = parent;
  entry->child = NULL;
  if (parent) {
    parent->child = entry;
  }
  return entry;
}

void update_error_message_info_(
    const char* function_name, char* file_name, int line_number, char* cn_source_loc) {
  global_error_msg_info = make_error_message_info_entry(
      function_name, file_name, line_number, cn_source_loc, global_error_msg_info);
}

void initialise_error_msg_info_(
    const char* function_name, char* file_name, int line_number) {
  // cn_printf(CN_LOGGING_INFO, "Initialising error message info\n");
  global_error_msg_info =
      make_error_message_info_entry(function_name, file_name, line_number, 0, NULL);
}

void reset_error_msg_info() {
  global_error_msg_info = NULL;
}

void free_error_msg_info() {
  while (global_error_msg_info != NULL) {
    cn_pop_msg_info();
  }
}

void cn_pop_msg_info() {
  struct cn_error_message_info* old = global_error_msg_info;
  global_error_msg_info = old->parent;
  if (global_error_msg_info) {
    global_error_msg_info->child = NULL;
  }
  fulm_free(old, &fulm_default_alloc);
}

static uint32_t cn_fls(uint32_t x) {
  return x ? sizeof(x) * 8 - __builtin_clz(x) : 0;
}

static uint64_t cn_flsl(uint64_t x) {
  return x ? sizeof(x) * 8 - __builtin_clzl(x) : 0;
}

cn_bits_u32* cn_bits_u32_fls(cn_bits_u32* i1) {
  cn_bits_u32* res = (cn_bits_u32*)cn_bump_malloc(sizeof(cn_bits_u32));
  res->val = cn_fls(i1->val);
  return res;
}

cn_bits_u64* cn_bits_u64_flsl(cn_bits_u64* i1) {
  cn_bits_u64* res = (cn_bits_u64*)cn_bump_malloc(sizeof(cn_bits_u64));
  res->val = cn_flsl(i1->val);
  return res;
}

void* cn_aligned_alloc_aux(size_t align, size_t size, _Bool wildcard) {
  void* p = aligned_alloc(align, size);
  char str[] = "cn_aligned_alloc";
  if (p) {
    fulminate_assume_ownership((void*)p, size, str, wildcard);
    return p;
  } else {
    cn_printf(CN_LOGGING_INFO, "aligned_alloc failed\n");
    return p;
  }
}

void* cn_aligned_alloc(size_t align, size_t size) {
  return cn_aligned_alloc_aux(align, size, false);
}

void* cn_unsafe_aligned_alloc(size_t align, size_t size) {
  return cn_aligned_alloc_aux(align, size, true);
}

void* cn_malloc_aux(unsigned long size, _Bool wildcard) {
  void* p = malloc(size);
  char str[] = "cn_malloc";
  if (p) {
    fulminate_assume_ownership((void*)p, size, str, wildcard);
    return p;
  } else {
    cn_printf(CN_LOGGING_INFO, "malloc failed\n");
    return p;
  }
}

void* cn_malloc(unsigned long size) {
  return cn_malloc_aux(size, false);
}

void* cn_unsafe_malloc(unsigned long size) {
  return cn_malloc_aux(size, true);
}

void* cn_calloc_aux(size_t num, size_t size, _Bool wildcard) {
  void* p = calloc(num, size);
  char str[] = "cn_calloc";
  if (p) {
    fulminate_assume_ownership((void*)p, num * size, str, wildcard);
    return p;
  } else {
    cn_printf(CN_LOGGING_INFO, "calloc failed\n");
    return p;
  }
}

void* cn_calloc(size_t num, size_t size) {
  return cn_calloc_aux(num, size, false);
}

void* cn_unsafe_calloc(size_t num, size_t size) {
  return cn_calloc_aux(num, size, true);
}

void cn_free_sized(void* malloced_ptr, size_t size) {
  // cn_printf(CN_LOGGING_INFO, "[CN: freeing ownership] " FMT_PTR ", size: %lu\n", (uintptr_t) malloced_ptr, size);
  if (malloced_ptr != NULL) {
    c_ownership_check("Free", malloced_ptr, (int)size, cn_stack_depth);
    c_remove_from_ghost_state(malloced_ptr, size);
  }
}

void cn_print_nr_owned_predicates(void) {
  printf("Owned predicates £%lu\n", nr_owned_predicates);
}

void cn_print_u64(const char* str, unsigned long u) {
  // cn_printf(CN_LOGGING_INFO, "\n\nprint %s: %20lx,  %20lu\n\n", str, u, u);
}

void cn_print_nr_u64(int i, unsigned long u) {
  // cn_printf(CN_LOGGING_INFO, "\n\nprint %d: %20lx,  %20lu\n", i, u, u);
}

// ghost arguments
void** ghost_arg_array;

void alloc_ghost_array(int ghost_array_size) {
  ghost_arg_array = fulm_malloc(ghost_array_size * sizeof(void*), &fulm_default_alloc);
}

void add_to_ghost_array(int i, void* ptr_to_ghost_arg) {
  ghost_arg_array[i] = ptr_to_ghost_arg;
}

void clear_ghost_array(int ghost_array_size) {
  for (int i = 0; i < ghost_array_size; i++) {
    fulm_free(ghost_arg_array + i * sizeof(void*), &fulm_default_alloc);
  }
}

void* load_from_ghost_array(int i) {
  return ghost_arg_array[i];
}

void cn_ghost_arg_failure(void) {
  cn_printf(CN_LOGGING_ERROR, "Runtime typechecking of ghost arguments failed\n")
      cn_printf(CN_LOGGING_ERROR,
          "************************************************************\n")
          print_error_msg_info(global_error_msg_info);
  cn_failure(CN_FAILURE_GHOST_ARGS, PRE);
}
