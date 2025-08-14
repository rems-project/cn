#include <cn-autoannot/focus_ctx.h>

#include <inttypes.h>
#include <limits.h>
#include <signal.h>  // for SIGABRT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <cn-executable/fulminate_alloc.h>
#include <cn-executable/utils.h>
#include <cn-autoannot/focus_ctx.h>

// Note(HK):
// we don't have to care about the difference between Owned/Block here
// because they are handled by the standard Fulminate machinery.
// i.e., if there is a discrepancy, it will be caught by Fulminate.

struct focus_context* cn_focus_global_context;  // top of the stack

// should happen simultaneously with ghost_stack_depth_incr
void push_focus_context(void) {
    struct focus_context* new_context = fulm_default_alloc.malloc(sizeof(struct focus_context));
    new_context->indices = NULL;
    new_context->iter_ress = NULL;
    new_context->prev = cn_focus_global_context;
    cn_focus_global_context = new_context;
}

void initialise_focus_context(void) {
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

    iter_res_set *cur_iter_res = old_context->iter_ress;
    while (cur_iter_res) {
        iter_res_set *next = cur_iter_res->next;
        fulm_free(cur_iter_res, &fulm_default_alloc);
        cur_iter_res = next;
    }
}

void insert_focus(int64_t index, type_sig sig) {
    struct focus_set* new_set = fulm_malloc(sizeof(struct focus_set), &fulm_default_alloc);
    new_set->info.index = index;
    new_set->info.sig = sig;
    new_set->next = cn_focus_global_context->indices;
    cn_focus_global_context->indices = new_set;
}

void insert_iter_res(uint64_t ptr, uint64_t start, uint64_t end, uint64_t size, type_sig sig) {
    struct iter_res_set* new = fulm_malloc(sizeof(iter_res_set), &fulm_default_alloc);
    new->res.ptr = ptr;
    new->res.size = size;
    new->res.start = start;
    new->res.end = end;
    new->res.sig = sig;
    new->next = cn_focus_global_context->iter_ress;
    cn_focus_global_context->iter_ress = new;
}

/// Checks if the given address
///  (i) is in a iterated resource
///  (ii) is not focused
/// If (i) and (ii), it needs focus, and returns 1.
int needs_focus(uint64_t address, uint64_t size) {
    assert(cn_focus_global_context != NULL);
    // (i) search for iterated resource
    iter_res_set *iter = cn_focus_global_context->iter_ress;
    while (iter) {
        iter_res_set *cur = iter;
        iter = iter->next;
        uint64_t start = cur->res.ptr + cur->res.start * cur->res.size;
        uint64_t end = cur->res.ptr + (cur->res.end + 1) * cur->res.size;
        uint64_t offset = address - cur->res.ptr;
        if (address < start || address + size > end) {
            continue;
        }
        if (offset % cur->res.size != 0) {
            continue;
        }

        uint64_t index = offset / cur->res.size;

        // Case: an appropriate iterated resource is found
        // (ii) search for focus
        focus_set* cur_focus = cn_focus_global_context->indices;
        while (cur_focus) {
            if (cur_focus->info.index == index &&
                strcmp(cur_focus->info.sig, cur->res.sig) == 0) {
                return 0;
            }
            cur_focus = cur_focus->next;
        }
        // The index is not focused
        return 1;
    }
    // We didn't find any appropriate iterated resource
    return 0;
}
