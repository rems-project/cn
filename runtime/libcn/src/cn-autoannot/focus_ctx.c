#include <cn-autoannot/auto_annot.h>

#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <signal.h>  // for SIGABRT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/fulminate_alloc.h>
#include <cn-executable/utils.h>

// Note(HK):
// we don't have to care about the difference between Owned/Block here
// because they are handled by the standard Fulminate machinery.
// i.e., if there is a discrepancy, it will be caught by Fulminate.

struct focus_context *cn_focus_global_context;  // top of the stack

// should happen simultaneously with ghost_stack_depth_incr
void push_focus_context(void) {
  struct focus_context *new_context =
      fulm_default_alloc.malloc(sizeof(struct focus_context));
  new_context->indices = NULL;
  new_context->prev = cn_focus_global_context;
  cn_focus_global_context = new_context;
}

void initialise_focus_context() {
  push_focus_context();
}

void pop_focus_context(void) {
  struct focus_context *old_context = cn_focus_global_context;
  cn_focus_global_context = cn_focus_global_context->prev;
  // free
  focus_set *cur_focus = old_context->indices;
  while (cur_focus) {
    focus_set *next = cur_focus->next;
    fulm_free(cur_focus, &fulm_default_alloc);
    cur_focus = next;
  }

}

void insert_focus(int64_t index, type_sig sig) {
  struct focus_set *new_set = fulm_malloc(sizeof(struct focus_set), &fulm_default_alloc);
  new_set->info.index = index;
  new_set->info.sig = sig;
  new_set->next = cn_focus_global_context->indices;
  cn_focus_global_context->indices = new_set;
}

void clear_focus() {
  focus_set *cur_focus = cn_focus_global_context->indices;
  while (cur_focus) {
    focus_set *next = cur_focus->next;
    fulm_free(cur_focus, &fulm_default_alloc);
    cur_focus = next;
  }
  cn_focus_global_context->indices = NULL;
}

/// Checks if the given address
///  (i) is in a iterated resource
///  (ii) is not focused
/// If (i) and (ii), it needs focus, and returns 1.
int needs_focus(uint64_t address, uint64_t size, int64_t *index_out, type_sig *sig_out) {
  assert(cn_focus_global_context != NULL);
  ownership_ghost_info * info = ownership_ghost_state_get(address);
  if (!info) {
    return 0;
  }
  struct cn_res* res_info = info->res_info_stack->top->cn_res_info;

  if (res_info->type != CN_RES_ITER) {
    return 0;
  }
  struct iter_res* res = &res_info->iter_res;

  int64_t offset = address - res->ptr;
  uint64_t index = offset / res->size;

  // Case: an appropriate iterated resource is found
  // (ii) search for focus
  focus_set *cur_focus = cn_focus_global_context->indices;
  while (cur_focus) {
    if (cur_focus->info.index == index &&
        strcmp(cur_focus->info.sig, res->sig) == 0) {
      return 0;
    }
    cur_focus = cur_focus->next;
  }
  // The index is not focused
  *index_out = index;
  *sig_out = res->sig;
  return 1;
}
