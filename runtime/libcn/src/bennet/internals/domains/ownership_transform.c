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
 * Since P6.6 this is a thin wrapper over the engine's deposit backward
 * walk (the legacy hand-written walk it replaces was ITE-blind; blame now
 * crosses conditionals with the join of the arm-wise requirements, which
 * is the sound direction for regeneration completeness).
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_ownership_backward_propagate_to_syms(
    cn_term* term, OWN_T* own_dom, bennet_absint_state* state) {
  /* The eval engine copies the pushed value into its own inline eval and
   * re-materializes a fresh heap payload at every SYM deposit, so it never
   * aliases own_dom - the caller (assign.c) may free own_dom after this
   * returns, and no copy is needed here (a copy would leak). */
  bennet_tagged_domain out = bennet_tagged_domain_create(&ownership_loc_bt, own_dom);
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
 * Deliberate behavior changes vs. the legacy walkers (P4 straight port;
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

/* Ownership has no generic/tagged conversion layer: the engine "value" IS the
 * uintptr_t ownership POD by value, canonicalize is the identity (single
 * width), and from/to_tagged copy the POD in/out of a heap cell so the
 * persistent state keeps its pointer-payload shape. The four value hooks + the
 * inline eval layer mirror the numeric domains (absint.h BENNET_ABSINT_EVAL_IMPL);
 * val_top/bottom are the structural top/bottom (before==after==0 / bottom set). */
typedef OWN_T ownership_generic;

static ownership_generic ownership_from_tagged(bennet_tagged_domain* d) {
  if (!d || !d->domain) {
    return (ownership_generic){.bottom = false, .before = 0, .after = 0};
  }
  return *(OWN_T*)d->domain;
}

static bennet_tagged_domain ownership_to_tagged(
    ownership_generic* g, cn_base_type* type) {
  OWN_T* p = std_malloc(sizeof(OWN_T));
  assert(p);
  *p = *g;
  return bennet_tagged_domain_create(type, p);
}

static ownership_generic ownership_canonicalize(ownership_generic g, cn_base_type* type) {
  (void)type;
  return g;
}

static ownership_generic ownership_val_top(cn_base_type* type) {
  (void)type;
  return (ownership_generic){.bottom = false, .before = 0, .after = 0};
}

static ownership_generic ownership_val_bottom(cn_base_type* type) {
  (void)type;
  return (ownership_generic){.bottom = true, .before = 0, .after = 0};
}

static ownership_generic ownership_val_join(ownership_generic* a, ownership_generic* b) {
  return *bennet_domain_ownership_join_uintptr_t(a, b);
}

static bool ownership_val_is_bottom(ownership_generic* g) {
  return g->bottom;
}

BENNET_ABSINT_EVAL_IMPL(ownership, ownership_generic)

static bennet_absint_eval_ownership ownership_basis_const(cn_term* term) {
  /* No value transfer: ownership tracks allocation extents, not values. */
  (void)term;
  return ownership_eval_top(&ownership_loc_bt);
}

static bennet_absint_eval_ownership ownership_basis_forward_unop(
    cn_unop op, bennet_absint_eval_ownership* v, cn_base_type* result_type) {
  (void)op;
  (void)v;
  (void)result_type;
  return ownership_eval_top(&ownership_loc_bt);
}

static bennet_absint_eval_ownership ownership_basis_forward_binop(cn_binop op,
    bennet_absint_eval_ownership* l,
    bennet_absint_eval_ownership* r,
    cn_base_type* result_type) {
  (void)op;
  (void)l;
  (void)r;
  (void)result_type;
  return ownership_eval_top(&ownership_loc_bt);
}

static bennet_absint_eval_ownership ownership_basis_forward_cast(
    cn_base_type* to, bennet_absint_eval_ownership* v) {
  /* Casts preserve ownership; pass the inner value through untouched. */
  (void)to;
  return *v;
}

static bennet_absint_eval_ownership ownership_basis_shift_forward(cn_term* term,
    bennet_absint_eval_ownership* base,
    bennet_absint_eval_ownership* index_or_null) {
  /* The index transfer needs a constant term, not an abstract value. */
  (void)index_or_null;
  OWN_T base_own = base->val;

  if (term->type == CN_TERM_MEMBER_SHIFT) {
    return ownership_eval_of(&ownership_loc_bt,
        *bennet_ownership_member_shift_uintptr_t(
            &base_own, term->data.member_shift.offset));
  }

  size_t index_val;
  if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
    return ownership_eval_of(&ownership_loc_bt,
        *bennet_ownership_array_shift_uintptr_t(
            &base_own, term->data.array_shift.element_size, index_val));
  }
  return ownership_eval_top(&ownership_loc_bt);
}

static bennet_absint_eval_ownership ownership_basis_ite_join(
    bennet_absint_eval_ownership* then_v,
    bennet_absint_eval_ownership* else_v,
    cn_base_type* term_type) {
  /* Ownership results are always LOC-tagged, whatever the ITE's own type;
   * eval_join tags with then_v->type, which is &ownership_loc_bt here. */
  (void)term_type;
  return ownership_eval_join(then_v, else_v);
}

static bennet_absint_bw_action ownership_basis_backward_unop(cn_unop op,
    bennet_absint_eval_ownership* out,
    bennet_absint_eval_ownership* operand_fwd,
    cn_base_type* operand_type,
    bennet_absint_eval_ownership* down) {
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
    bennet_absint_eval_ownership* out,
    bennet_absint_eval_ownership* other_fwd,
    bennet_absint_eval_ownership* target_fwd,
    cn_base_type* target_type,
    bennet_absint_eval_ownership* down) {
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
    bennet_absint_eval_ownership* out,
    bennet_absint_eval_ownership* down) {
  (void)src_type;
  (void)dst_type;
  /* Casts preserve ownership: descend with the requirement unchanged. */
  *down = *out;
  return BENNET_ABSINT_BW_DESCEND;
}

static bennet_absint_bw_action ownership_basis_shift_backward(cn_term* term,
    bool target_is_base,
    bennet_absint_eval_ownership* out,
    bennet_absint_eval_ownership* sibling_fwd,
    bennet_absint_eval_ownership* target_fwd,
    bennet_absint_eval_ownership* down) {
  (void)sibling_fwd;
  (void)target_fwd;

  /* The index is never refined: ownership is a pointer requirement. */
  if (!target_is_base) {
    return BENNET_ABSINT_BW_STOP;
  }

  OWN_T out_own = out->val;

  if (term->type == CN_TERM_MEMBER_SHIFT) {
    *down = ownership_eval_of(&ownership_loc_bt,
        *bennet_ownership_member_shift_backward_uintptr_t(
            &out_own, term->data.member_shift.offset));
    return BENNET_ABSINT_BW_DESCEND;
  }

  size_t index_val;
  if (try_extract_const_size(term->data.array_shift.index, &index_val)) {
    *down = ownership_eval_of(&ownership_loc_bt,
        *bennet_ownership_array_shift_backward_uintptr_t(
            &out_own, term->data.array_shift.element_size, index_val));
    return BENNET_ABSINT_BW_DESCEND;
  }
  return BENNET_ABSINT_BW_STOP;
}

static bennet_absint_cmp_result ownership_basis_assume_cmp(cn_binop op,
    bool value,
    bennet_absint_eval_ownership* l_fwd,
    bennet_absint_eval_ownership* r_fwd,
    cn_base_type* l_ref_type,
    cn_base_type* r_ref_type,
    bennet_absint_eval_ownership* l_ref,
    bennet_absint_eval_ownership* r_ref) {
  bennet_absint_cmp_result res = {
      .has_rule = false, .apply_left = false, .apply_right = false};

  /* Only pointer equality refines: both sides must satisfy the met
   * requirement. LT/LE and the pointer comparisons carry no ownership
   * information. */
  if (op != CN_BINOP_EQ || !value) {
    return res;
  }

  OWN_T lv = l_fwd->val;
  OWN_T rv = r_fwd->val;
  OWN_T met = *bennet_domain_ownership_meet_uintptr_t(&lv, &rv);
  *l_ref = ownership_eval_of(l_ref_type, met);
  *r_ref = ownership_eval_of(r_ref_type, met);
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
#define ABSINT_VAL ownership_generic
#include <bennet/internals/domains/transform.inc.c>
