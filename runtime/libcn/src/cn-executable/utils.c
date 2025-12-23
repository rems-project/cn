#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <signal.h>  // for SIGABRT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <cn-executable/utils.h>
#include <fulminate/internals.h>

typedef rmap ownership_ghost_state;

ownership_ghost_state cn_ownership_global_ghost_state;

extern struct cn_error_message_info* global_error_msg_info;

struct cn_error_message_info* no_error_msg_info = 0;

signed long cn_stack_depth;

signed long nr_owned_predicates;

_Bool exec_c_locs_mode;
_Bool ownership_stack_mode;

static signed long UNMAPPED_VAL = -1;
static signed long WILDCARD_DEPTH = INT_MIN + 1;

static _Bool fulminate_initialized = false;

void fulminate_destroy(void) {
  if (!fulminate_initialized) {
    return;  // Already destroyed or never initialized
  }

  cn_bump_free_all();
  free_ownership_ghost_state();
  free_error_msg_info();

  fulminate_initialized = false;
}

void fulminate_init(void) {
  assert(!fulminate_initialized && "Fulminate already initialized - destroy first");

  initialise_ownership_ghost_state();
  initialise_ghost_stack_depth();
  initialise_exec_c_locs_mode(0);
  initialise_ownership_stack_mode(0);

  fulminate_initialized = true;
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
    case CN_FAILURE_USER_ALLOC:
      cn_printf(CN_LOGGING_INFO, "User allocation failed\n");
      break;
    case CN_FAILURE_FULM_ALLOC:
      cn_printf(CN_LOGGING_INFO, "Out of memory!\n");
      fallthrough;
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

// void print_owned_calls_stack(ownership_ghost_info* entry_maybe) {
//   if (entry_maybe && entry_maybe->source_loc_stack) {
//     cn_printf(CN_LOGGING_ERROR, "Owned locations stack:\n");
//     char* popped_elem =
//         cn_source_location_stack_pop(entry_maybe->source_loc_stack, &fulm_default_alloc);
//     while (popped_elem) {
//       cn_printf(CN_LOGGING_ERROR, "%s\n", popped_elem);
//       cn_printf(CN_LOGGING_ERROR, "==========================\n");
//       popped_elem = cn_source_location_stack_pop(
//           entry_maybe->source_loc_stack, &fulm_default_alloc);
//     }
//   }
// }

int* create_ownership_ghost_state_entry(int depth) {
  int* ghost_state_entry = fulm_malloc(sizeof(int), &fulm_default_alloc);
  *ghost_state_entry = depth;
  return ghost_state_entry;
}

void cn_assert(cn_bool* cn_b, enum spec_mode spec_mode) {
  // cn_printf(CN_LOGGING_INFO, "[CN: assertion] function %s, file %s, line %d\n", error_msg_info.function_name, error_msg_info.file_name, error_msg_info.line_number);
  if (!(cn_b->val)) {
    print_error_msg_info(global_error_msg_info);
    cn_failure(CN_FAILURE_ASSERT, spec_mode);
  }
}

void initialise_ownership_ghost_state(void) {
  nr_owned_predicates = 0;
  cn_ownership_global_ghost_state =
      rmap_create(4, fulm_default_alloc.malloc, fulm_default_alloc.free);
}

void free_ownership_ghost_state(void) {
  nr_owned_predicates = 0;
  rmap_free(cn_ownership_global_ghost_state);
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

void cn_postcondition_leak_check(void) {
  rmap_range_res_t res = rmap_find_range(0UL, -1UL, cn_ownership_global_ghost_state);
  if (res.defined && res.max > cn_stack_depth) {
    print_error_msg_info(global_error_msg_info);
    // XXX TODO: scan for the failing address
    // cn_printf(CN_LOGGING_ERROR,
    //     "Postcondition leak check failed, ownership leaked for pointer " FMT_PTR "\n",
    //     (uintptr_t)*key);
    cn_failure(CN_FAILURE_OWNERSHIP_LEAK, POST);
  }
}

void cn_loop_leak_check(void) {
  rmap_range_res_t res = rmap_find_range(0UL, -1UL, cn_ownership_global_ghost_state);
  if (res.defined && res.max == cn_stack_depth) {
    print_error_msg_info(global_error_msg_info);
    // XXX TODO: scan for the failing address
    // cn_printf(CN_LOGGING_ERROR,
    //     "Loop invariant leak check failed, ownership leaked for pointer " FMT_PTR "\n",
    //     (uintptr_t)*key);
    cn_failure(CN_FAILURE_OWNERSHIP_LEAK, LOOP);
  }
}

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
  struct loop_ownership* loop_ownership = cn_bump_malloc(sizeof(struct loop_ownership));
  assert(loop_ownership);
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
    loop_ownership_nd* newhd = cn_bump_malloc(sizeof(loop_ownership_nd));
    assert(newhd);
    *newhd = (loop_ownership_nd){.addr = addr, .size = size, .next = hd};
    loop_ownership->head = newhd;
  }
}

void cn_loop_put_back_ownership(struct loop_ownership* loop_ownership) {
  for (loop_ownership_nd* nd = loop_ownership->head; nd; nd = nd->next) {
    ownership_ghost_state_set(nd->addr, nd->size, cn_stack_depth, no_error_msg_info);
  }
}

int ownership_ghost_state_get(int64_t address) {
  rmap_value_opt_t res = rmap_find(address, cn_ownership_global_ghost_state);
  return res.defined ? res.v : UNMAPPED_VAL;
}

void ownership_ghost_state_set(int64_t address,
    size_t size,
    int stack_depth_val,
    struct cn_error_message_info* error_msg_info) {
  if (size > 0)
    rmap_add(
        address, address + size - 1, stack_depth_val, cn_ownership_global_ghost_state);

  // rmap_range_res_t res = rmap_find_range(address, address + size - 1, cn_ownership_global_ghost_state);
  // if (!res.defined || res.min > WILDCARD_DEPTH)
  //   rmap_add(address, address + size - 1, stack_depth_val, cn_ownership_global_ghost_state);
  // else if (res.max > WILDCARD_DEPTH)
  //   for (int64_t addr = address; addr < address + size; addr++) {
  //     rmap_value_opt_t res2 = rmap_find(addr, cn_ownership_global_ghost_state);
  //     if (!(res2.defined && res2.v == WILDCARD_DEPTH))
  //       rmap_add(addr, addr, stack_depth_val, cn_ownership_global_ghost_state);
  // }
}

void ownership_ghost_state_remove(int64_t address, size_t size) {
  if (size > 0)
    rmap_remove(address, address + size - 1, cn_ownership_global_ghost_state);

  // rmap_range_res_t res = rmap_find_range(address, address + size - 1, cn_ownership_global_ghost_state);
  // if (!res.defined || res.min > WILDCARD_DEPTH)
  //   rmap_remove(address, address + size - 1, cn_ownership_global_ghost_state);
  // else if (res.max > WILDCARD_DEPTH)
  //   for (int64_t addr = address; addr < address + size; addr++) {
  //     rmap_value_opt_t res2 = rmap_find(addr, cn_ownership_global_ghost_state);
  //     if (!(res2.defined && res2.v == WILDCARD_DEPTH))
  //       rmap_remove(addr, addr, cn_ownership_global_ghost_state);
  //   }
}

rmap_range_res_t ownership_ghost_state_extrema(int64_t address, size_t size) {
  if (size > 0)
    return rmap_find_range(address, address + size - 1, cn_ownership_global_ghost_state);
  return (rmap_range_res_t){.defined = false};
}

_Bool is_wildcard(void* generic_c_ptr, int size) {
  int64_t address = (uintptr_t)generic_c_ptr;
  if (size > 0) {
    rmap_range_res_t res =
        rmap_find_range(address, address + size - 1, cn_ownership_global_ghost_state);
    return res.defined && res.max == WILDCARD_DEPTH;
  }
  return true;
}

/* Calls to this generated by Fulminate for stack-local variable ownership checking */
void c_add_to_ghost_state(void* ptr_to_local, size_t size, signed long stack_depth) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] add local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_set(
      (uintptr_t)ptr_to_local, size, stack_depth, global_error_msg_info);
}

/* Only called internally for tracking ownership of heap addresses */
void c_add_to_ghost_state_internal(uintptr_t ptr_to_local,
    size_t size,
    signed long stack_depth,
    struct cn_error_message_info* error_msg_info) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] add local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_set(ptr_to_local, size, stack_depth, error_msg_info);
}

void c_remove_from_ghost_state(void* ptr_to_local, size_t size) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] remove local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_remove((uintptr_t)ptr_to_local, size);
}

void c_remove_from_ghost_state_internal(uintptr_t ptr_to_local, size_t size) {
  // cn_printf(CN_LOGGING_INFO, "[C access checking] remove local:" FMT_PTR ", size: %lu\n", ptr_to_local, size);
  ownership_ghost_state_remove(ptr_to_local, size);
}

void cn_get_ownership(void* generic_c_ptr, size_t size) {
  /* Used for precondition and loop invariant taking/getting of ownership */
  enum region_owned owned = c_ownership_check(
      "Precondition ownership check", generic_c_ptr, size, cn_stack_depth - 1);
  if (owned == NO_WILDCARD)
    c_add_to_ghost_state_internal(
        (uintptr_t)generic_c_ptr, size, cn_stack_depth, global_error_msg_info);
  else
    assert(owned == FULL_WILDCARD);  // INCOMPLETE
}

void cn_loop_get_ownership(
    void* generic_c_ptr, size_t size, struct loop_ownership* loop_ownership) {
  enum region_owned owned =
      c_ownership_check("Loop ownership check", generic_c_ptr, size, cn_stack_depth);
  if (owned == NO_WILDCARD) {
    c_add_to_ghost_state_internal(
        (uintptr_t)generic_c_ptr, size, cn_stack_depth - 1, global_error_msg_info);
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
  } else
    assert(owned == FULL_WILDCARD);  // INCOMPLETE
}

void cn_put_ownership(void* generic_c_ptr, size_t size) {
  // cn_printf(CN_LOGGING_INFO, "[CN: returning ownership] " FMT_PTR_2 ", size: %lu\n", generic_c_ptr, size);
  //// print_error_msg_info();
  enum region_owned owned = c_ownership_check(
      "Postcondition ownership check", generic_c_ptr, size, cn_stack_depth);
  if (owned == NO_WILDCARD)
    c_add_to_ghost_state_internal(
        (uintptr_t)generic_c_ptr, size, cn_stack_depth - 1, global_error_msg_info);
  else
    assert(owned == FULL_WILDCARD);  // INCOMPLETE
}

void fulminate_assume_ownership(
    void* generic_c_ptr, unsigned long size, char* fun, _Bool wildcard) {
  signed long depth = wildcard ? WILDCARD_DEPTH : cn_stack_depth;
  ownership_ghost_state_set((uintptr_t)generic_c_ptr, size, depth, global_error_msg_info);
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
      case PRE:
        cn_get_ownership(generic_c_ptr, size);
        break;
      case POST:
        cn_put_ownership(generic_c_ptr, size);
        break;
      case LOOP:
        cn_loop_get_ownership(generic_c_ptr, size, loop_ownership);
        fallthrough;
      default:
        break;
    }
  }
}

enum region_owned c_ownership_check(
    char* access_kind, void* generic_c_ptr, int size, signed long expected_stack_depth) {
  if (size < 1)
    return FULL_WILDCARD;  // Arbitrary...

  uintptr_t address = (uintptr_t)generic_c_ptr;
  rmap_range_res_t res = ownership_ghost_state_extrema(address, size);

  if (res.defined && res.min == res.max) {
    if (res.max == WILDCARD_DEPTH)
      return FULL_WILDCARD;
    else if (res.max == expected_stack_depth)
      return NO_WILDCARD;
  }

  for (size_t addr = address; addr < address + size; addr++) {
    int depth = ownership_ghost_state_get(addr);
    if (depth != expected_stack_depth && depth != WILDCARD_DEPTH) {
      print_error_msg_info(global_error_msg_info);
      cn_printf(CN_LOGGING_ERROR, "%s failed.\n", access_kind);
      if (depth == UNMAPPED_VAL) {
        cn_printf(CN_LOGGING_ERROR,
            "  ==> " FMT_PTR "[%d] (" FMT_PTR ") not owned\n",
            addr,
            0,
            addr);
      } else {
        cn_printf(CN_LOGGING_ERROR,
            "  ==> " FMT_PTR "[%d] (" FMT_PTR
            ") not owned at expected function call stack depth %ld\n",
            addr,
            0,
            addr,
            expected_stack_depth);
        cn_printf(CN_LOGGING_ERROR, "  ==> (owned at stack depth: %d)\n", depth);
      }
      cn_failure(CN_FAILURE_CHECK_OWNERSHIP, C_ACCESS);
    }
  }

  return SOME_WILDCARD;
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

void* cn_aligned_alloc_aux(size_t align, size_t size, _Bool wildcard) {
  void* p = aligned_alloc(align, size);
  if (p) {
    char str[] = "cn_aligned_alloc";
    fulminate_assume_ownership((void*)p, size, str, wildcard);
  } else if (size != 0) {
    cn_failure(CN_FAILURE_USER_ALLOC, NON_SPEC);
  }

  return p;
}

void* cn_aligned_alloc(size_t align, size_t size) {
  return cn_aligned_alloc_aux(align, size, false);
}

void* cn_unsafe_aligned_alloc(size_t align, size_t size) {
  return cn_aligned_alloc_aux(align, size, true);
}

void* cn_malloc_aux(unsigned long size, _Bool wildcard) {
  void* p = malloc(size);
  if (p) {
    char str[] = "cn_malloc";
    fulminate_assume_ownership((void*)p, size, str, wildcard);
  } else if (size != 0) {
    cn_failure(CN_FAILURE_USER_ALLOC, NON_SPEC);
  }

  return p;
}

void* cn_malloc(unsigned long size) {
  return cn_malloc_aux(size, false);
}

void* cn_unsafe_malloc(unsigned long size) {
  return cn_malloc_aux(size, true);
}

void* cn_calloc_aux(size_t num, size_t size, _Bool wildcard) {
  void* p = calloc(num, size);
  if (p) {
    char str[] = "cn_calloc";
    fulminate_assume_ownership((void*)p, num * size, str, wildcard);
  } else if (size != 0) {
    cn_failure(CN_FAILURE_USER_ALLOC, NON_SPEC);
  }

  return p;
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
    enum region_owned owned =
        c_ownership_check("Free", malloced_ptr, size, cn_stack_depth);
    if (owned == NO_WILDCARD)
      c_remove_from_ghost_state_internal((uintptr_t)malloced_ptr, size);
    else
      assert(owned == FULL_WILDCARD);  // INCOMPLETE
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

void free_ghost_array(void) {
  fulm_free(ghost_arg_array, &fulm_default_alloc);
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
