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

/* All ownership results are LOC-typed. The legacy walkers tagged them with
 * function-local stack types that escaped into the persistent state; the
 * file-static carries the same tag value with a fixed lifetime. */
static cn_base_type ownership_loc_bt = {.tag = CN_BASE_LOC};

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
 * Backward propagation to all symbols (the assign.c blame channel)
 *
 * This is a thin wrapper over the engine's deposit backward
 * walk (the legacy hand-written walk it replaces was ITE-blind; blame now
 * crosses conditionals with the join of the arm-wise requirements, which
 * is the sound direction for regeneration completeness).
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_ownership_backward_propagate_to_syms(
    cn_term* term, OWN_T* own_dom, bennet_absint_state* state) {
  /* The engine's SYM deposit conses the pushed tagged domain directly into
   * the state when the binding is absent, but this channel's contract lets
   * the caller free own_dom after the call (assign.c does) - pass a copy
   * so no state binding aliases caller memory. */
  OWN_T* own_copy = bennet_domain_ownership_copy_uintptr_t(own_dom);
  bennet_tagged_domain out = bennet_tagged_domain_create(&ownership_loc_bt, own_copy);
  return bennet_ownership_transform_backward(
      term, (bennet_absint_sym){.name = NULL, .id = 0}, out, state);
}

/*-----------------------------------------------------------------------------
 * Transformer basis (consumed by the engine template, transform.inc.c)
 *
 * These are the ownership-specific transfer functions of the shared cn_term
 * walker engine; the traversal, gating, and refinement-application order
 * live in the template. Ownership is uintptr_t-only with no generic /
 * to_tagged layer: tagged payloads point directly at a
 * bennet_domain_ownership(uintptr_t), and every basis result carries the
 * file-static LOC tag.
 *
 * Deliberate behavior changes vs. the legacy walkers (straight port;
 * witness pins flipped in test/bennet/ownership_transform.cpp):
 *  - the forward SYM stored-tag guard is gone: non-LOC-tagged ownership
 *    entries (generated refine seeds every variable under its own type tag)
 *    now participate in forwards instead of reading as top;
 *  - EQ-assume applies the met requirement via the engine's collect-syms +
 *    targeted-backward walks instead of the deposit walk, so a symbol
 *    reachable through both ITE arms is now refined (join of the arm-wise
 *    inversions);
 *  - an unsatisfiable (bottom) met triggers the engine's unsat protocol
 *    (every sym of both comparison sides set to bottom) instead of the
 *    deposit walk's supported-paths-only bottom.
 *---------------------------------------------------------------------------*/

#include <bennet/internals/domains/transform_template.h>

static bennet_tagged_domain ownership_tagged_loc(OWN_T* own) {
  return bennet_tagged_domain_create(&ownership_loc_bt, own);
}

static bennet_tagged_domain ownership_basis_const(cn_term* term) {
  /* No value transfer: ownership tracks allocation extents, not values. */
  (void)term;
  return ownership_tagged_loc(bennet_domain_ownership_top(uintptr_t));
}

static bennet_tagged_domain ownership_basis_forward_unop(
    cn_unop op, bennet_tagged_domain* v, cn_base_type* result_type) {
  (void)op;
  (void)v;
  (void)result_type;
  return ownership_tagged_loc(bennet_domain_ownership_top(uintptr_t));
}

static bennet_tagged_domain ownership_basis_forward_binop(cn_binop op,
    bennet_tagged_domain* l,
    bennet_tagged_domain* r,
    cn_base_type* result_type) {
  (void)op;
  (void)l;
  (void)r;
  (void)result_type;
  return ownership_tagged_loc(bennet_domain_ownership_top(uintptr_t));
}

static bennet_tagged_domain ownership_basis_forward_cast(
    cn_base_type* to, bennet_tagged_domain* v) {
  /* Casts preserve ownership; pass the inner value through untouched. */
  (void)to;
  return *v;
}

static bennet_tagged_domain ownership_basis_shift_forward(
    cn_term* term, bennet_tagged_domain* base, bennet_tagged_domain* index_or_null) {
  /* The index transfer needs a constant term, not an abstract value. */
  (void)index_or_null;
  OWN_T* base_own = (OWN_T*)base->domain;
  assert(base_own);

  if (term->type == CN_TERM_MEMBER_SHIFT) {
    return ownership_tagged_loc(bennet_ownership_member_shift_uintptr_t(
        base_own, term->data.member_shift.offset));
  }

  size_t index_val;
  if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
    return ownership_tagged_loc(bennet_ownership_array_shift_uintptr_t(
        base_own, term->data.array_shift.element_size, index_val));
  }
  return ownership_tagged_loc(bennet_domain_ownership_top(uintptr_t));
}

static bennet_tagged_domain ownership_basis_ite_join(
    bennet_tagged_domain* then_v, bennet_tagged_domain* else_v, cn_base_type* term_type) {
  /* Ownership results are always LOC-tagged, whatever the ITE's own type. */
  (void)term_type;
  OWN_T* joined = bennet_domain_ownership_join_uintptr_t(
      (OWN_T*)then_v->domain, (OWN_T*)else_v->domain);
  return ownership_tagged_loc(joined);
}

static bennet_absint_bw_action ownership_basis_backward_unop(cn_unop op,
    bennet_tagged_domain* out,
    bennet_tagged_domain* operand_fwd,
    cn_base_type* operand_type,
    bennet_tagged_domain* down) {
  (void)op;
  (void)out;
  (void)operand_fwd;
  (void)operand_type;
  (void)down;
  /* No arithmetic transfer to invert. */
  return BENNET_ABSINT_BW_STOP;
}

static bennet_absint_bw_action ownership_basis_backward_binop(cn_binop op,
    bool target_is_left,
    bennet_tagged_domain* out,
    bennet_tagged_domain* other_fwd,
    bennet_tagged_domain* target_fwd,
    cn_base_type* target_type,
    bennet_tagged_domain* down) {
  (void)op;
  (void)target_is_left;
  (void)out;
  (void)other_fwd;
  (void)target_fwd;
  (void)target_type;
  (void)down;
  /* No arithmetic transfer to invert (comparisons are engine territory). */
  return BENNET_ABSINT_BW_STOP;
}

static bennet_absint_bw_action ownership_basis_backward_cast(cn_base_type* src_type,
    cn_base_type* dst_type,
    bennet_tagged_domain* out,
    bennet_tagged_domain* down) {
  (void)src_type;
  (void)dst_type;
  /* Casts preserve ownership: descend with the requirement unchanged. */
  *down = *out;
  return BENNET_ABSINT_BW_DESCEND;
}

static bennet_absint_bw_action ownership_basis_shift_backward(cn_term* term,
    bool target_is_base,
    bennet_tagged_domain* out,
    bennet_tagged_domain* sibling_fwd,
    bennet_tagged_domain* target_fwd,
    bennet_tagged_domain* down) {
  (void)sibling_fwd;
  (void)target_fwd;

  /* The index is never refined: ownership is a pointer requirement. */
  if (!target_is_base) {
    return BENNET_ABSINT_BW_STOP;
  }

  OWN_T* out_own = (OWN_T*)out->domain;
  assert(out_own);

  if (term->type == CN_TERM_MEMBER_SHIFT) {
    *down = ownership_tagged_loc(bennet_ownership_member_shift_backward_uintptr_t(
        out_own, term->data.member_shift.offset));
    return BENNET_ABSINT_BW_DESCEND;
  }

  size_t index_val;
  if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
    *down = ownership_tagged_loc(bennet_ownership_array_shift_backward_uintptr_t(
        out_own, term->data.array_shift.element_size, index_val));
    return BENNET_ABSINT_BW_DESCEND;
  }
  return BENNET_ABSINT_BW_STOP;
}

static bennet_absint_cmp_result ownership_basis_assume_cmp(cn_binop op,
    bool value,
    bennet_tagged_domain* l_fwd,
    bennet_tagged_domain* r_fwd,
    cn_base_type* l_ref_type,
    cn_base_type* r_ref_type,
    bennet_tagged_domain* l_ref,
    bennet_tagged_domain* r_ref) {
  bennet_absint_cmp_result res = {
      .has_rule = false, .apply_left = false, .apply_right = false};

  /* Only pointer equality refines: both sides must satisfy the met
   * requirement. LT/LE and the pointer comparisons carry no ownership
   * information. */
  if (op != CN_BINOP_EQ || !value) {
    return res;
  }

  OWN_T* met = bennet_domain_ownership_meet_uintptr_t(
      (OWN_T*)l_fwd->domain, (OWN_T*)r_fwd->domain);
  *l_ref = bennet_tagged_domain_create(l_ref_type, met);
  *r_ref = bennet_tagged_domain_create(r_ref_type, met);
  res.has_rule = true;
  res.apply_left = true;
  res.apply_right = true;
  return res;
}

/*-----------------------------------------------------------------------------
 * Engine instantiation: emits bennet_ownership_transform_{forward,backward,
 * backward_assume}
 *---------------------------------------------------------------------------*/

#define ABSINT_DOM ownership
#include <bennet/internals/domains/transform.inc.c>
