#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <bennet/internals/absint.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/terms.h>

/*-----------------------------------------------------------------------------
 * Domain-Independent Implementations
 *---------------------------------------------------------------------------*/

bennet_tagged_domain bennet_tagged_domain_create(cn_base_type* type, void* domain) {
  bennet_tagged_domain result;
  result.type = type;
  result.domain = domain;
  return result;
}

#define ABSINT_STATE_INITIAL_CAPACITY 16

bennet_absint_state* bennet_absint_state_create(void) {
  bennet_absint_state* state = std_malloc(sizeof(bennet_absint_state));
  assert(state);
  state->entries =
      std_malloc(ABSINT_STATE_INITIAL_CAPACITY * sizeof(bennet_absint_state_entry));
  assert(state->entries);
  state->count = 0;
  state->capacity = ABSINT_STATE_INITIAL_CAPACITY;
  return state;
}

void bennet_absint_state_free(bennet_absint_state* state) {
  // Note: We use std_malloc which uses arena allocation, so we don't actually free.
  // This function exists for API completeness.
  (void)state;
}

int bennet_absint_state_find(bennet_absint_state* state, uint64_t id) {
  for (size_t i = 0; i < state->count; i++) {
    if (state->entries[i].id == id) {
      return (int)i;
    }
  }
  return -1;
}

bool term_contains_sym(cn_term* term, uint64_t sym_id) {
  if (!term)
    return false;

  switch (term->type) {
    case CN_TERM_SYM:
      return term->data.sym.id == sym_id;

    case CN_TERM_UNOP:
      return term_contains_sym(term->data.unop.operand, sym_id);

    case CN_TERM_BINOP:
      return term_contains_sym(term->data.binop.left, sym_id) ||
             term_contains_sym(term->data.binop.right, sym_id);

    case CN_TERM_ITE:
      return term_contains_sym(term->data.ite.cond, sym_id) ||
             term_contains_sym(term->data.ite.then_term, sym_id) ||
             term_contains_sym(term->data.ite.else_term, sym_id);

    case CN_TERM_CAST:
      return term_contains_sym(term->data.cast.value, sym_id);

    case CN_TERM_ARRAY_SHIFT:
      return term_contains_sym(term->data.array_shift.base, sym_id) ||
             term_contains_sym(term->data.array_shift.index, sym_id);

    case CN_TERM_MEMBER_SHIFT:
      return term_contains_sym(term->data.member_shift.base, sym_id);

    default:
      return false;
  }
}
