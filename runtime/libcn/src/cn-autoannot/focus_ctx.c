#include <inttypes.h>
#include <limits.h>
#include <signal.h>  // for SIGABRT
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <cn-executable/fulminate_alloc.h>
#include <cn-executable/utils.h>

// hashset
typedef hash_table focus_set;

struct focus_context {
    focus_set* indices;
    struct focus_context *prev;
};

struct focus_context* cn_focus_global_context;  // top of the stack

void initialise_focus_context(void) {
    cn_focus_global_context = fulm_default_alloc.malloc(sizeof(struct focus_context));
    cn_focus_global_context->indices = ht_create(&fulm_default_alloc);
    cn_focus_global_context->prev = NULL;
}

// should happen simultaneously with ghost_stack_depth_incr
void push_focus_context(void) {
    struct focus_context* new_context = fulm_default_alloc.malloc(sizeof(struct focus_context));
    new_context->indices = ht_create(&fulm_default_alloc);
    new_context->prev = cn_focus_global_context;
    cn_focus_global_context = new_context;
}

void pop_focus_context(void) {
    // Free is automatic, no?
    cn_focus_global_context = cn_focus_global_context->prev;
}

void insert_focus(int64_t index) {
    int64_t* key = fulm_malloc(sizeof(int64_t), &fulm_default_alloc);
    *key = index;
    ht_set(cn_focus_global_context->indices, key, (void*)1);  // value is not used
}

int check_focus(int64_t index) {
    if (ht_get(cn_focus_global_context->indices, &index) != NULL) {
        return 1;
    } else {
        return 0;
    }
}

