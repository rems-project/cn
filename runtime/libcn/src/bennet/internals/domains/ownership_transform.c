#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/internals/domain.h>
#include <bennet/internals/domains/ownership.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/terms.h>

/*-----------------------------------------------------------------------------
 * Static Helpers
 *---------------------------------------------------------------------------*/

#define OWN_T bennet_domain_ownership(uintptr_t)

static bool try_extract_const_size(cn_term* term, size_t* out) {
  if (!term || term->type != CN_TERM_CONST) {
    return false;
  }

  cn_const* c = &term->data.const_val;
  switch (c->type) {
    case CN_CONST_Z:
      if (c->data.z >= 0) {
        *out = (size_t)c->data.z;
        return true;
      }
      return false;
    case CN_CONST_BITS:
      if (c->data.bits.value >= 0) {
        *out = (size_t)c->data.bits.value;
        return true;
      }
      return false;
    default:
      return false;
  }
}

/*-----------------------------------------------------------------------------
 * Tagged Domain Functions (ownership-specific)
 *---------------------------------------------------------------------------*/

bool bennet_tagged_domain_is_bottom_ownership(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return false;
  OWN_T* own = (OWN_T*)d->domain;
  return bennet_domain_ownership_is_bottom_uintptr_t(own);
}

bool bennet_tagged_domain_is_top_ownership(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return true;
  OWN_T* own = (OWN_T*)d->domain;
  return bennet_domain_ownership_is_top_uintptr_t(own);
}

bennet_tagged_domain bennet_tagged_domain_copy_ownership(bennet_tagged_domain* d) {
  if (!d || !d->domain) {
    return bennet_tagged_domain_top_ownership(d ? d->type : NULL);
  }
  OWN_T* own = (OWN_T*)d->domain;
  OWN_T* copy = bennet_domain_ownership_copy_uintptr_t(own);
  return bennet_tagged_domain_create(d->type, copy);
}

bennet_tagged_domain bennet_tagged_domain_top_ownership(cn_base_type* type) {
  OWN_T* own = bennet_domain_ownership_top(uintptr_t);
  return bennet_tagged_domain_create(type, own);
}

bennet_tagged_domain bennet_tagged_domain_bottom_ownership(cn_base_type* type) {
  OWN_T* own = bennet_domain_ownership_bottom(uintptr_t);
  return bennet_tagged_domain_create(type, own);
}

bennet_tagged_domain bennet_tagged_domain_meet_ownership(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2);
  OWN_T* o1 = (OWN_T*)d1->domain;
  OWN_T* o2 = (OWN_T*)d2->domain;
  OWN_T* met = bennet_domain_ownership_meet_uintptr_t(o1, o2);
  return bennet_tagged_domain_create(d1->type, met);
}

bennet_tagged_domain bennet_tagged_domain_join_ownership(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2);
  OWN_T* o1 = (OWN_T*)d1->domain;
  OWN_T* o2 = (OWN_T*)d2->domain;
  OWN_T* joined = bennet_domain_ownership_join_uintptr_t(o1, o2);
  return bennet_tagged_domain_create(d1->type, joined);
}

/*-----------------------------------------------------------------------------
 * Abstract State Implementation (ownership)
 *---------------------------------------------------------------------------*/

BENNET_ABSINT_STATE_IMPL(ownership)

/*-----------------------------------------------------------------------------
 * Forward Transformer
 *---------------------------------------------------------------------------*/

bennet_tagged_domain bennet_ownership_transform_forward(
    cn_term* term, bennet_absint_state* state) {
  cn_base_type loc_bt = {.tag = CN_BASE_LOC};

  if (!term || !state) {
    return bennet_tagged_domain_create(&loc_bt, bennet_domain_ownership_top(uintptr_t));
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
      bennet_tagged_domain d =
          bennet_absint_state_get_ownership(state, sym, &term->base_type);
      if (d.type && d.type->tag == CN_BASE_LOC && d.domain) {
        return d;
      }
      return bennet_tagged_domain_create(&loc_bt, bennet_domain_ownership_top(uintptr_t));
    }

    case CN_TERM_MEMBER_SHIFT: {
      bennet_tagged_domain base_dom =
          bennet_ownership_transform_forward(term->data.member_shift.base, state);
      OWN_T* base_own = (OWN_T*)base_dom.domain;
      OWN_T* result = bennet_ownership_member_shift_uintptr_t(
          base_own, term->data.member_shift.offset);
      return bennet_tagged_domain_create(&loc_bt, result);
    }

    case CN_TERM_ARRAY_SHIFT: {
      bennet_tagged_domain base_dom =
          bennet_ownership_transform_forward(term->data.array_shift.base, state);
      size_t index_val;
      if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
        OWN_T* base_own = (OWN_T*)base_dom.domain;
        OWN_T* result = bennet_ownership_array_shift_uintptr_t(
            base_own, term->data.array_shift.element_size, index_val);
        return bennet_tagged_domain_create(&loc_bt, result);
      }
      return bennet_tagged_domain_create(&loc_bt, bennet_domain_ownership_top(uintptr_t));
    }

    case CN_TERM_CAST:
      return bennet_ownership_transform_forward(term->data.cast.value, state);

    case CN_TERM_ITE: {
      bennet_tagged_domain then_dom =
          bennet_ownership_transform_forward(term->data.ite.then_term, state);
      bennet_tagged_domain else_dom =
          bennet_ownership_transform_forward(term->data.ite.else_term, state);
      OWN_T* joined = bennet_domain_ownership_join_uintptr_t(
          (OWN_T*)then_dom.domain, (OWN_T*)else_dom.domain);
      return bennet_tagged_domain_create(&loc_bt, joined);
    }

    default:
      return bennet_tagged_domain_create(&loc_bt, bennet_domain_ownership_top(uintptr_t));
  }
}

/*-----------------------------------------------------------------------------
 * Backward Transformer
 *---------------------------------------------------------------------------*/

/**
 * Walk a term tree; at each SYM node, meet the ownership domain into state.
 * At MEMBER_SHIFT / ARRAY_SHIFT, invert the shift before recursing into base.
 * At CAST, recurse into inner (casts preserve ownership).
 */
bennet_absint_state* bennet_ownership_backward_propagate_to_syms(
    cn_term* term, OWN_T* own_dom, bennet_absint_state* state) {
  if (!term) {
    return bennet_absint_state_copy_ownership(state);
  }

  cn_base_type loc_bt = {.tag = CN_BASE_LOC};

  switch (term->type) {
    case CN_TERM_SYM: {
      bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
      bennet_tagged_domain cur =
          bennet_absint_state_get_ownership(state, sym, &term->base_type);
      OWN_T* cur_own = (OWN_T*)cur.domain;
      OWN_T* met = bennet_domain_ownership_meet_uintptr_t(cur_own, own_dom);
      bennet_tagged_domain met_tagged = bennet_tagged_domain_create(&loc_bt, met);
      return bennet_absint_state_set_ownership(state, sym, met_tagged);
    }

    case CN_TERM_MEMBER_SHIFT: {
      OWN_T* base_req = bennet_ownership_member_shift_backward_uintptr_t(
          own_dom, term->data.member_shift.offset);
      return bennet_ownership_backward_propagate_to_syms(
          term->data.member_shift.base, base_req, state);
    }

    case CN_TERM_ARRAY_SHIFT: {
      size_t index_val;
      if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
        OWN_T* base_req = bennet_ownership_array_shift_backward_uintptr_t(
            own_dom, term->data.array_shift.element_size, index_val);
        return bennet_ownership_backward_propagate_to_syms(
            term->data.array_shift.base, base_req, state);
      }
      return bennet_absint_state_copy_ownership(state);
    }

    case CN_TERM_CAST:
      return bennet_ownership_backward_propagate_to_syms(
          term->data.cast.value, own_dom, state);

    default:
      return bennet_absint_state_copy_ownership(state);
  }
}

bennet_absint_state* bennet_ownership_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state) {
  if (!term || !state) {
    return state;
  }

  OWN_T* out_own = (OWN_T*)output_domain.domain;
  assert(out_own);

  if (bennet_domain_ownership_is_bottom_uintptr_t(out_own)) {
    cn_base_type loc_bt = {.tag = CN_BASE_LOC};
    bennet_tagged_domain bot =
        bennet_tagged_domain_create(&loc_bt, bennet_domain_ownership_bottom(uintptr_t));
    return bennet_absint_state_set_ownership(
        bennet_absint_state_copy_ownership(state), target_sym, bot);
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      if (term->data.sym.id == target_sym.id) {
        cn_base_type loc_bt = {.tag = CN_BASE_LOC};
        bennet_tagged_domain cur =
            bennet_absint_state_get_ownership(state, target_sym, &term->base_type);
        OWN_T* cur_own = (OWN_T*)cur.domain;
        OWN_T* met = bennet_domain_ownership_meet_uintptr_t(cur_own, out_own);
        bennet_tagged_domain met_tagged = bennet_tagged_domain_create(&loc_bt, met);
        return bennet_absint_state_set_ownership(
            bennet_absint_state_copy_ownership(state), target_sym, met_tagged);
      }
      return bennet_absint_state_copy_ownership(state);
    }

    case CN_TERM_MEMBER_SHIFT: {
      cn_term* base = term->data.member_shift.base;
      if (!term_contains_sym(base, target_sym.id)) {
        return bennet_absint_state_copy_ownership(state);
      }
      OWN_T* base_req = bennet_ownership_member_shift_backward_uintptr_t(
          out_own, term->data.member_shift.offset);
      cn_base_type loc_bt = {.tag = CN_BASE_LOC};
      bennet_tagged_domain base_dom = bennet_tagged_domain_create(&loc_bt, base_req);
      return bennet_ownership_transform_backward(base, target_sym, base_dom, state);
    }

    case CN_TERM_ARRAY_SHIFT: {
      cn_term* base = term->data.array_shift.base;
      if (!term_contains_sym(base, target_sym.id)) {
        return bennet_absint_state_copy_ownership(state);
      }
      size_t index_val;
      if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
        OWN_T* base_req = bennet_ownership_array_shift_backward_uintptr_t(
            out_own, term->data.array_shift.element_size, index_val);
        cn_base_type loc_bt = {.tag = CN_BASE_LOC};
        bennet_tagged_domain base_dom = bennet_tagged_domain_create(&loc_bt, base_req);
        return bennet_ownership_transform_backward(base, target_sym, base_dom, state);
      }
      return bennet_absint_state_copy_ownership(state);
    }

    case CN_TERM_CAST: {
      cn_term* inner = term->data.cast.value;
      if (!term_contains_sym(inner, target_sym.id)) {
        return bennet_absint_state_copy_ownership(state);
      }
      return bennet_ownership_transform_backward(inner, target_sym, output_domain, state);
    }

    case CN_TERM_ITE: {
      bennet_absint_state* then_state = bennet_ownership_transform_backward(
          term->data.ite.then_term, target_sym, output_domain, state);
      bennet_absint_state* else_state = bennet_ownership_transform_backward(
          term->data.ite.else_term, target_sym, output_domain, state);

      if (bennet_absint_state_is_bottom_ownership(then_state)) {
        return else_state;
      }
      if (bennet_absint_state_is_bottom_ownership(else_state)) {
        return then_state;
      }

      cn_base_type loc_bt = {.tag = CN_BASE_LOC};
      bennet_tagged_domain then_dom =
          bennet_absint_state_get_ownership(then_state, target_sym, &loc_bt);
      bennet_tagged_domain else_dom =
          bennet_absint_state_get_ownership(else_state, target_sym, &loc_bt);
      OWN_T* joined = bennet_domain_ownership_join_uintptr_t(
          (OWN_T*)then_dom.domain, (OWN_T*)else_dom.domain);
      bennet_tagged_domain joined_tagged = bennet_tagged_domain_create(&loc_bt, joined);
      return bennet_absint_state_set_ownership(
          bennet_absint_state_copy_ownership(state), target_sym, joined_tagged);
    }

    default:
      return bennet_absint_state_copy_ownership(state);
  }
}

/*-----------------------------------------------------------------------------
 * Backward Assume Transformer
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_ownership_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state) {
  if (!term || !state) {
    return state;
  }

  /* NOT: flip value and recurse */
  if (term->type == CN_TERM_UNOP && term->data.unop.op == CN_UNOP_NOT) {
    return bennet_ownership_transform_backward_assume(
        term->data.unop.operand, !value, state);
  }

  if (term->type == CN_TERM_BINOP) {
    cn_term* left = term->data.binop.left;
    cn_term* right = term->data.binop.right;
    cn_binop op = term->data.binop.op;

    switch (op) {
      case CN_BINOP_AND: {
        if (value) {
          bennet_absint_state* s =
              bennet_ownership_transform_backward_assume(left, true, state);
          return bennet_ownership_transform_backward_assume(right, true, s);
        }
        return bennet_absint_state_copy_ownership(state);
      }

      case CN_BINOP_OR: {
        if (!value) {
          bennet_absint_state* s =
              bennet_ownership_transform_backward_assume(left, false, state);
          return bennet_ownership_transform_backward_assume(right, false, s);
        }
        return bennet_absint_state_copy_ownership(state);
      }

      case CN_BINOP_EQ: {
        if (value) {
          /* a == b: forward both, meet ownership, propagate back */
          bennet_tagged_domain left_dom = bennet_ownership_transform_forward(left, state);
          bennet_tagged_domain right_dom =
              bennet_ownership_transform_forward(right, state);

          OWN_T* met = bennet_domain_ownership_meet_uintptr_t(
              (OWN_T*)left_dom.domain, (OWN_T*)right_dom.domain);
          bennet_absint_state* s =
              bennet_ownership_backward_propagate_to_syms(left, met, state);
          return bennet_ownership_backward_propagate_to_syms(right, met, s);
        }
        return bennet_absint_state_copy_ownership(state);
      }

      default:
        return bennet_absint_state_copy_ownership(state);
    }
  }

  return bennet_absint_state_copy_ownership(state);
}
