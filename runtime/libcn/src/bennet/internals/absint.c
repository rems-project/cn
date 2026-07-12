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

bennet_absint_state* bennet_absint_state_create(void) {
  bennet_absint_state* state = std_malloc(sizeof(bennet_absint_state));
  assert(state);
  state->has_entry = false;
  state->next = NULL;
  return state;
}

void bennet_absint_state_free(bennet_absint_state* state) {
  // std_malloc'd memory is reclaimed wholesale by cn_test_free_all(); nothing
  // to release per state. This function exists for API completeness.
  (void)state;
}

bennet_absint_state* bennet_absint_state_cons(
    bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain) {
  bennet_absint_state* cell = std_malloc(sizeof(bennet_absint_state));
  assert(cell);
  cell->has_entry = true;
  cell->entry.id = sym.id;
  cell->entry.domain = domain;
  cell->next = state;
  return cell;
}

bennet_tagged_domain* bennet_absint_state_lookup(
    bennet_absint_state* state, uint64_t id) {
  for (bennet_absint_state* cell = state; cell; cell = cell->next) {
    if (cell->has_entry && cell->entry.id == id) {
      return &cell->entry.domain;
    }
  }
  return NULL;
}

/* First live cell at or after `from` whose binding is not shadowed by a newer
   cell (one closer to `head`). */
static bennet_absint_state* absint_state_next_live(
    bennet_absint_state* head, bennet_absint_state* from) {
  for (bennet_absint_state* cell = from; cell; cell = cell->next) {
    if (!cell->has_entry) {
      continue;
    }
    bool shadowed = false;
    for (bennet_absint_state* newer = head; newer != cell; newer = newer->next) {
      if (newer->has_entry && newer->entry.id == cell->entry.id) {
        shadowed = true;
        break;
      }
    }
    if (!shadowed) {
      return cell;
    }
  }
  return NULL;
}

bennet_absint_state_iter bennet_absint_state_iter_begin(bennet_absint_state* state) {
  return (bennet_absint_state_iter){
      .head = state, .cell = absint_state_next_live(state, state)};
}

bool bennet_absint_state_iter_done(const bennet_absint_state_iter* it) {
  return it->cell == NULL;
}

void bennet_absint_state_iter_next(bennet_absint_state_iter* it) {
  assert(it->cell);
  it->cell = absint_state_next_live(it->head, it->cell->next);
}

bennet_tagged_domain* bennet_absint_state_iter_domain(
    const bennet_absint_state_iter* it) {
  assert(it->cell);
  return &it->cell->entry.domain;
}

void bennet_absint_type_info(cn_base_type* type, int* width, bool* is_signed) {
  assert(type);
  if (type->tag == CN_BASE_BITS) {
    *width = type->data.bits.size_bits;
    *is_signed = type->data.bits.is_signed;
  } else if (type->tag == CN_BASE_LOC) {
    *width = 64;  // Pointer type - use 64-bit width
    *is_signed = false;
  } else {
    // Default to 64-bit unsigned for unsupported types
    *width = 64;
    *is_signed = false;
  }
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
