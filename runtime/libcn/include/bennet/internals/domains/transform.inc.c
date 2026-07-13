/**
 * @file transform.inc.c
 * @brief cn_term transformer engine template. See transform_template.h for
 *        the parameter contract and basis-function documentation.
 *
 * This file is #included at the bottom of a domain's .c after its basis
 * functions; it is not a standalone translation unit (it lives under
 * include/ precisely so the libbennet dune rule does not compile it on its
 * own). It emits the domain's three public entry points:
 *
 *   bennet_tagged_domain bennet_<dom>_transform_forward(cn_term*,
 *                                                       bennet_absint_state*);
 *   bennet_absint_state* bennet_<dom>_transform_backward(cn_term*,
 *       bennet_absint_sym, bennet_tagged_domain, bennet_absint_state*);
 *   bennet_absint_state* bennet_<dom>_transform_backward_assume(cn_term*,
 *       bool, bennet_absint_state*);
 *
 * Engine shape (soundness gated
 * by absint_oracle.cpp and absint_fuzz.cpp):
 *  - the forward pass builds a cached tree (ftree) of tagged values, one
 *    node per supported cn_term node (ITE conditions are not evaluated;
 *    unsupported kinds are a single default arm yielding top);
 *  - the backward pass is a single deposit walk: at every node it computes
 *    the inverse image for each child from the cached sibling values
 *    (Jacobi within the node - both downs are derived from the pre-walk
 *    forward cache), descends into every invertible child, and meets the
 *    pushed value into the state at every SYM it reaches; ITE walks both
 *    arms and pointwise-joins the branch states;
 *  - assume pushes each comparison side's refinement with one deposit walk
 *    over that side's already-built forward tree (pure Jacobi across
 *    sides), replacing the legacy Gauss-Seidel walk-per-collected-symbol
 *    protocol.
 */

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#include <bennet/internals/absint.h>
#include <bennet/internals/domains/transform_template.h>
#include <cn-smt/memory/arena.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/terms.h>

#ifndef ABSINT_DOM
  #error "transform.inc.c requires ABSINT_DOM to be defined"
#endif

#define ABSINT_CAT_(a, b) a##b
#define ABSINT_CAT(a, b)  ABSINT_CAT_(a, b)

/* congr_basis_<name>, congr_engine_<name>, bennet_congr_transform_<name> */
#define ABSINT_BASIS(name)  ABSINT_CAT(ABSINT_DOM, ABSINT_CAT(_basis_, name))
#define ABSINT_ENGINE(name) ABSINT_CAT(ABSINT_DOM, ABSINT_CAT(_engine_, name))
#define ABSINT_PUBLIC(name)                                                              \
  ABSINT_CAT(bennet_, ABSINT_CAT(ABSINT_DOM, ABSINT_CAT(_transform_, name)))

/* bennet_tagged_domain_<op>_congr, bennet_absint_state_<op>_congr */
#define ABSINT_TAGGED(op)                                                                \
  ABSINT_CAT(bennet_tagged_domain_, ABSINT_CAT(op, ABSINT_CAT(_, ABSINT_DOM)))
#define ABSINT_STATE(op)                                                                 \
  ABSINT_CAT(bennet_absint_state_, ABSINT_CAT(op, ABSINT_CAT(_, ABSINT_DOM)))

#define ABSINT_FTREE ABSINT_CAT(ABSINT_DOM, _absint_ftree)

/*-----------------------------------------------------------------------------
 * Forward pass: cached tree of tagged values
 *---------------------------------------------------------------------------*/

typedef struct ABSINT_FTREE {
  cn_term* term;
  bennet_tagged_domain val;
  /* Children in term order; fanout <= 2 among evaluated children (ITE
   * conditions are not evaluated, matching the legacy walkers). */
  struct ABSINT_FTREE* kids[2];
} ABSINT_FTREE;

static ABSINT_FTREE* ABSINT_ENGINE(fwd)(cn_term* term, bennet_absint_state* state) {
  /* Node structs are arena-allocated and released per public entry via a
   * frame checkpoint (see the public entry points below): only these transient
   * nodes live in the arena; every cached abstract value's payload is in the
   * std allocator, so restoring the frame frees nothing a return value reads. */
  ABSINT_FTREE* node = cn_arena_malloc(bennet_absint_arena(), sizeof(ABSINT_FTREE));
  assert(node);
  node->term = term;
  node->kids[0] = NULL;
  node->kids[1] = NULL;

  if (!term) {
    cn_base_type bt = cn_base_type_bits(false, 64);
    node->val = ABSINT_TAGGED(top)(&bt);
    return node;
  }

  switch (term->type) {
    case CN_TERM_CONST: {
      node->val = ABSINT_BASIS(const)(term);
      break;
    }

    case CN_TERM_SYM: {
      bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
      node->val = ABSINT_STATE(get)(state, sym, &term->base_type);
      break;
    }

    case CN_TERM_UNOP: {
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.unop.operand, state);
      node->val = ABSINT_BASIS(forward_unop)(
          term->data.unop.op, &node->kids[0]->val, &term->base_type);
      break;
    }

    case CN_TERM_BINOP: {
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.binop.left, state);
      node->kids[1] = ABSINT_ENGINE(fwd)(term->data.binop.right, state);
      node->val = ABSINT_BASIS(forward_binop)(term->data.binop.op,
          &node->kids[0]->val,
          &node->kids[1]->val,
          &term->base_type);
      break;
    }

    case CN_TERM_ITE: {
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.ite.then_term, state);
      node->kids[1] = ABSINT_ENGINE(fwd)(term->data.ite.else_term, state);
      node->val = ABSINT_BASIS(ite_join)(
          &node->kids[0]->val, &node->kids[1]->val, &term->base_type);
      break;
    }

    case CN_TERM_CAST: {
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.cast.value, state);
      node->val = ABSINT_BASIS(forward_cast)(&term->base_type, &node->kids[0]->val);
      break;
    }

    case CN_TERM_WRAPI: {
      /* WRAPI == CAST for absint: the node's base_type carries the
       * wrapped-to type and the semantics are modular conversion. */
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.wrapi.value, state);
      node->val = ABSINT_BASIS(forward_cast)(&term->base_type, &node->kids[0]->val);
      break;
    }

    case CN_TERM_LET: {
      /* Forward-only binding overlay: bind the variable to the value's
       * forward value for the body walk (one cons on the persistent list;
       * the binding is discarded with the walk). Backward and assume stay
       * conservative: no refinement through binders. */
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.let.value, state);
      bennet_absint_sym var = {
          .name = term->data.let.var.name, .id = term->data.let.var.id};
      bennet_absint_state* body_state =
          ABSINT_STATE(set)(ABSINT_STATE(copy)(state), var, node->kids[0]->val);
      node->kids[1] = ABSINT_ENGINE(fwd)(term->data.let.body, body_state);
      node->val = node->kids[1]->val;
      break;
    }

    case CN_TERM_ARRAY_SHIFT: {
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.array_shift.base, state);
      node->kids[1] = ABSINT_ENGINE(fwd)(term->data.array_shift.index, state);
      node->val =
          ABSINT_BASIS(shift_forward)(term, &node->kids[0]->val, &node->kids[1]->val);
      break;
    }

    case CN_TERM_MEMBER_SHIFT: {
      node->kids[0] = ABSINT_ENGINE(fwd)(term->data.member_shift.base, state);
      node->val = ABSINT_BASIS(shift_forward)(term, &node->kids[0]->val, NULL);
      break;
    }

    default:
      /* The single unsupported-node fallback (one place to extend or log). */
      node->val = ABSINT_TAGGED(top)(&term->base_type);
      break;
  }

  return node;
}

/* Are two stored tagged types compatible for a value-space join? Branch
 * assumptions can deposit a binding at a wider type than the symbol's own
 * (cast-path refinements), and the generic joins assert equal widths. */
static bool ABSINT_ENGINE(types_joinable)(cn_base_type* a, cn_base_type* b) {
  if (a->tag != b->tag)
    return false;
  if (a->tag == CN_BASE_BITS)
    return a->data.bits.size_bits == b->data.bits.size_bits &&
           a->data.bits.is_signed == b->data.bits.is_signed;
  return true;
}

/* Pointwise join of two branch-assumption states over the syms of `term`,
 * met into `out` (a copy of the branches' common base state). Sound because
 * assume only tightens: each branch binding is <= the base binding, so the
 * join is too, and meeting it into the base equals replacing. A contradicted
 * branch deposited bottom on its syms, so join(bottom, live) recovers the
 * live branch; when both branches bottom a sym, the join keeps bottom and
 * `out` reads as bottom (the whole conjunction/disjunction is
 * unsatisfiable). Skipped syms - top on either side, incompatible stored
 * types, or syms under unsupported nodes (which assume can never refine) -
 * keep their base binding, which is always sound. Walking the term rather
 * than bennet_absint_term_collect_syms avoids the 16-symbol cap and yields
 * each sym's own base type. */
static bennet_absint_state* ABSINT_ENGINE(join_branches)(cn_term* term,
    bennet_absint_state* sa,
    bennet_absint_state* sb,
    bennet_absint_state* out) {
  if (!term)
    return out;

  switch (term->type) {
    case CN_TERM_SYM: {
      bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
      bennet_tagged_domain da = ABSINT_STATE(get)(sa, sym, &term->base_type);
      bennet_tagged_domain db = ABSINT_STATE(get)(sb, sym, &term->base_type);
      bool bot_a = ABSINT_TAGGED(is_bottom)(&da);
      bool bot_b = ABSINT_TAGGED(is_bottom)(&db);

      /* Branch bindings are meets into the common base, so each is <= the
       * base binding and `set` (replace) is sound everywhere below. `meet`
       * would be wrong here: the unsat protocol deposits bottom tagged LOC
       * (a pinned legacy quirk), which cannot meet a narrower base binding
       * (the generic meets assert equal widths). */
      if (bot_a && bot_b) {
        /* Both branches contradict: bottom at the sym's own type. */
        return ABSINT_STATE(set)(out, sym, ABSINT_TAGGED(bottom)(&term->base_type));
      }
      if (bot_a || bot_b) {
        /* One branch contradicts: keep the live branch's binding. */
        bennet_tagged_domain live = bot_a ? db : da;
        if (ABSINT_TAGGED(is_top)(&live)) {
          return out;
        }
        return ABSINT_STATE(set)(out, sym, live);
      }
      if (ABSINT_TAGGED(is_top)(&da) || ABSINT_TAGGED(is_top)(&db)) {
        return out;
      }
      if (!ABSINT_ENGINE(types_joinable)(da.type, db.type)) {
        return out;
      }
      bennet_tagged_domain joined = ABSINT_TAGGED(join)(&da, &db);
      return ABSINT_STATE(set)(out, sym, joined);
    }
    case CN_TERM_UNOP:
      return ABSINT_ENGINE(join_branches)(term->data.unop.operand, sa, sb, out);
    case CN_TERM_BINOP:
      out = ABSINT_ENGINE(join_branches)(term->data.binop.left, sa, sb, out);
      return ABSINT_ENGINE(join_branches)(term->data.binop.right, sa, sb, out);
    case CN_TERM_ITE:
      out = ABSINT_ENGINE(join_branches)(term->data.ite.cond, sa, sb, out);
      out = ABSINT_ENGINE(join_branches)(term->data.ite.then_term, sa, sb, out);
      return ABSINT_ENGINE(join_branches)(term->data.ite.else_term, sa, sb, out);
    case CN_TERM_CAST:
      return ABSINT_ENGINE(join_branches)(term->data.cast.value, sa, sb, out);
    case CN_TERM_WRAPI:
      return ABSINT_ENGINE(join_branches)(term->data.wrapi.value, sa, sb, out);
    case CN_TERM_ARRAY_SHIFT:
      out = ABSINT_ENGINE(join_branches)(term->data.array_shift.base, sa, sb, out);
      return ABSINT_ENGINE(join_branches)(term->data.array_shift.index, sa, sb, out);
    case CN_TERM_MEMBER_SHIFT:
      return ABSINT_ENGINE(join_branches)(term->data.member_shift.base, sa, sb, out);
    default:
      return out;
  }
}

/*-----------------------------------------------------------------------------
 * Backward pass: deposit walk over the cached tree
 *---------------------------------------------------------------------------*/

/* Unsat protocol for a bottom pushed value: set every collected sym of the
 * subtree to bottom (same collector cap and LOC tagging as the assume-side
 * comparison protocol). */
static bennet_absint_state* ABSINT_ENGINE(deposit_bottom)(
    cn_term* term, bennet_absint_state* state) {
  static cn_base_type absint_bwd_loc_bt = {.tag = CN_BASE_LOC};
  bennet_absint_state* bot_state = ABSINT_STATE(copy)(state);
  bennet_absint_sym syms[16];
  int n = bennet_absint_term_collect_syms(term, syms, 16);
  for (int i = 0; i < n; i++) {
    bot_state =
        ABSINT_STATE(set)(bot_state, syms[i], ABSINT_TAGGED(bottom)(&absint_bwd_loc_bt));
  }
  return bot_state;
}

/* Single deposit walk: push the output's inverse image into every child
 * whose basis can invert (both downs of a binop are computed from the
 * pre-walk forward cache - Jacobi within the node), and meet the pushed
 * value into the state at every SYM reached. Unsupported nodes and STOP
 * answers refine nothing (always sound). A bottom pushed value bottoms
 * every sym of the subtree it was pushed into. */
static bennet_absint_state* ABSINT_ENGINE(bwd)(
    ABSINT_FTREE* node, bennet_tagged_domain output_domain, bennet_absint_state* state) {
  cn_term* term = node->term;
  if (!term || !state)
    return state;

  if (ABSINT_TAGGED(is_bottom)(&output_domain)) {
    return ABSINT_ENGINE(deposit_bottom)(term, state);
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
      return ABSINT_STATE(meet)(state, sym, output_domain);
    }

    case CN_TERM_BINOP: {
      /* Comparison ops are handled by backward_assume */
      switch (term->data.binop.op) {
        case CN_BINOP_EQ:
        case CN_BINOP_LT:
        case CN_BINOP_LE:
        case CN_BINOP_LT_POINTER:
        case CN_BINOP_LE_POINTER:
          return ABSINT_STATE(copy)(state);
        default:
          break;
      }

      bennet_tagged_domain down_l;
      bennet_tagged_domain down_r;
      bennet_absint_bw_action act_l = ABSINT_BASIS(backward_binop)(term->data.binop.op,
          /*target_is_left=*/true,
          &output_domain,
          &node->kids[1]->val,
          &node->kids[0]->val,
          &term->data.binop.left->base_type,
          &down_l);
      bennet_absint_bw_action act_r = ABSINT_BASIS(backward_binop)(term->data.binop.op,
          /*target_is_left=*/false,
          &output_domain,
          &node->kids[0]->val,
          &node->kids[1]->val,
          &term->data.binop.right->base_type,
          &down_r);

      bennet_absint_state* result = state;
      if (act_l == BENNET_ABSINT_BW_DESCEND) {
        result = ABSINT_ENGINE(bwd)(node->kids[0], down_l, result);
      }
      if (act_r == BENNET_ABSINT_BW_DESCEND) {
        result = ABSINT_ENGINE(bwd)(node->kids[1], down_r, result);
      }
      return result;
    }

    case CN_TERM_UNOP: {
      bennet_tagged_domain down;
      bennet_absint_bw_action action = ABSINT_BASIS(backward_unop)(term->data.unop.op,
          &output_domain,
          &node->kids[0]->val,
          &term->data.unop.operand->base_type,
          &down);
      if (action == BENNET_ABSINT_BW_DESCEND) {
        return ABSINT_ENGINE(bwd)(node->kids[0], down, state);
      }
      return ABSINT_STATE(copy)(state);
    }

    case CN_TERM_ITE: {
      /* Walk both arms from the incoming state and pointwise-join the
       * branch refinements over the subtree's syms (the same helper the
       * AND-false/OR-true assume rules use). A bottomed arm means the
       * output is unreachable through it: keep the other arm's state. */
      bennet_absint_state* then_state =
          ABSINT_ENGINE(bwd)(node->kids[0], output_domain, state);
      bennet_absint_state* else_state =
          ABSINT_ENGINE(bwd)(node->kids[1], output_domain, state);

      if (ABSINT_STATE(is_bottom)(then_state))
        return else_state;
      if (ABSINT_STATE(is_bottom)(else_state))
        return then_state;

      return ABSINT_ENGINE(join_branches)(
          term, then_state, else_state, ABSINT_STATE(copy)(state));
    }

    case CN_TERM_ARRAY_SHIFT: {
      bennet_tagged_domain down_base;
      bennet_tagged_domain down_idx;
      bennet_absint_bw_action act_base = ABSINT_BASIS(shift_backward)(term,
          /*target_is_base=*/true,
          &output_domain,
          &node->kids[1]->val,
          &node->kids[0]->val,
          &down_base);
      bennet_absint_bw_action act_idx = ABSINT_BASIS(shift_backward)(term,
          /*target_is_base=*/false,
          &output_domain,
          &node->kids[0]->val,
          &node->kids[1]->val,
          &down_idx);

      bennet_absint_state* result = state;
      if (act_base == BENNET_ABSINT_BW_DESCEND) {
        result = ABSINT_ENGINE(bwd)(node->kids[0], down_base, result);
      }
      if (act_idx == BENNET_ABSINT_BW_DESCEND) {
        result = ABSINT_ENGINE(bwd)(node->kids[1], down_idx, result);
      }
      return result;
    }

    case CN_TERM_MEMBER_SHIFT: {
      bennet_tagged_domain down;
      bennet_absint_bw_action action = ABSINT_BASIS(shift_backward)(
          term, true, &output_domain, NULL, &node->kids[0]->val, &down);
      if (action == BENNET_ABSINT_BW_DESCEND) {
        return ABSINT_ENGINE(bwd)(node->kids[0], down, state);
      }
      return ABSINT_STATE(copy)(state);
    }

    case CN_TERM_CAST: {
      cn_term* inner = term->data.cast.value;
      bennet_tagged_domain down;
      bennet_absint_bw_action action = ABSINT_BASIS(backward_cast)(
          &inner->base_type, &term->base_type, &output_domain, &down);
      if (action == BENNET_ABSINT_BW_DESCEND) {
        return ABSINT_ENGINE(bwd)(node->kids[0], down, state);
      }
      return ABSINT_STATE(copy)(state);
    }

    case CN_TERM_WRAPI: {
      /* Mirror of CAST: invert the modular conversion via backward_cast. */
      cn_term* inner = term->data.wrapi.value;
      bennet_tagged_domain down;
      bennet_absint_bw_action action = ABSINT_BASIS(backward_cast)(
          &inner->base_type, &term->base_type, &output_domain, &down);
      if (action == BENNET_ABSINT_BW_DESCEND) {
        return ABSINT_ENGINE(bwd)(node->kids[0], down, state);
      }
      return ABSINT_STATE(copy)(state);
    }

    default:
      /* Unknown term type (incl. LET): no safe refinement possible */
      return ABSINT_STATE(copy)(state);
  }
}

/*-----------------------------------------------------------------------------
 * Assume: comparison refinement push-down
 *---------------------------------------------------------------------------*/

static bennet_absint_state* ABSINT_ENGINE(assume)(
    cn_term* term, bool value, bennet_absint_state* state) {
  if (!term || !state)
    return state;

  /* Handle NOT(expr) by recursing with flipped value */
  if (term->type == CN_TERM_UNOP && term->data.unop.op == CN_UNOP_NOT) {
    return ABSINT_ENGINE(assume)(term->data.unop.operand, !value, state);
  }

  if (term->type == CN_TERM_BINOP) {
    cn_term* left = term->data.binop.left;
    cn_term* right = term->data.binop.right;
    cn_binop op = term->data.binop.op;

    switch (op) {
      case CN_BINOP_AND: {
        if (value) {
          /* Both sides must be true */
          bennet_absint_state* result = ABSINT_ENGINE(assume)(left, true, state);
          return ABSINT_ENGINE(assume)(right, true, result);
        }
        /* At least one side is false: pointwise join of the two branch
         * assumptions (no refinement was made here before). */
        bennet_absint_state* sa = ABSINT_ENGINE(assume)(left, false, state);
        bennet_absint_state* sb = ABSINT_ENGINE(assume)(right, false, state);
        return ABSINT_ENGINE(join_branches)(term, sa, sb, ABSINT_STATE(copy)(state));
      }

      case CN_BINOP_OR: {
        if (!value) {
          /* Both sides must be false */
          bennet_absint_state* result = ABSINT_ENGINE(assume)(left, false, state);
          return ABSINT_ENGINE(assume)(right, false, result);
        }
        /* At least one side is true: join of the branch assumptions. */
        bennet_absint_state* sa = ABSINT_ENGINE(assume)(left, true, state);
        bennet_absint_state* sb = ABSINT_ENGINE(assume)(right, true, state);
        return ABSINT_ENGINE(join_branches)(term, sa, sb, ABSINT_STATE(copy)(state));
      }

      case CN_BINOP_EQ:
      case CN_BINOP_LT:
      case CN_BINOP_LE:
      case CN_BINOP_LT_POINTER:
      case CN_BINOP_LE_POINTER: {
        /* The forward trees double as the backward walks' caches (the
         * caching across the assume/backward boundary the template header
         * used to defer): both sides' refinements push over trees built
         * against the same incoming state - pure Jacobi across sides. */
        ABSINT_FTREE* l_tree = ABSINT_ENGINE(fwd)(left, state);
        ABSINT_FTREE* r_tree = ABSINT_ENGINE(fwd)(right, state);
        bennet_tagged_domain l_fwd = l_tree->val;
        bennet_tagged_domain r_fwd = r_tree->val;

        /* Pointer-comparison retagging quirk shared by the legacy walkers:
         * refinements over a LOC-typed side are tagged with the side's own
         * LOC type rather than the forward value's type. */
        cn_base_type* l_ref_type =
            left->base_type.tag == CN_BASE_LOC ? &left->base_type : l_fwd.type;
        cn_base_type* r_ref_type =
            right->base_type.tag == CN_BASE_LOC ? &right->base_type : r_fwd.type;

        bennet_tagged_domain l_ref;
        bennet_tagged_domain r_ref;
        bennet_absint_cmp_result cmp = ABSINT_BASIS(assume_cmp)(
            op, value, &l_fwd, &r_fwd, l_ref_type, r_ref_type, &l_ref, &r_ref);
        if (!cmp.has_rule) {
          return ABSINT_STATE(copy)(state);
        }

        if (ABSINT_TAGGED(is_bottom)(&l_ref) || ABSINT_TAGGED(is_bottom)(&r_ref)) {
          /* Unsatisfiable: propagate bottom to all syms. The LOC type is a
           * function-local static (the legacy walkers stored a dangling
           * stack local here). */
          static cn_base_type absint_loc_bt = {.tag = CN_BASE_LOC};
          bennet_absint_state* bot_state = ABSINT_STATE(copy)(state);
          bennet_absint_sym all_syms[16];
          int nl = bennet_absint_term_collect_syms(left, all_syms, 16);
          int nr = bennet_absint_term_collect_syms(right, all_syms + nl, 16 - nl);
          for (int i = 0; i < nl + nr; i++) {
            bot_state = ABSINT_STATE(set)(
                bot_state, all_syms[i], ABSINT_TAGGED(bottom)(&absint_loc_bt));
          }
          return bot_state;
        }

        bennet_absint_state* result = ABSINT_STATE(copy)(state);
        if (cmp.apply_left) {
          result = ABSINT_ENGINE(bwd)(l_tree, l_ref, result);
        }
        if (cmp.apply_right) {
          result = ABSINT_ENGINE(bwd)(r_tree, r_ref, result);
        }
        return result;
      }

      default:
        return ABSINT_STATE(copy)(state);
    }
  }

  return ABSINT_STATE(copy)(state);
}

/*-----------------------------------------------------------------------------
 * Public entry points (stable ABI; called from product.ml-generated code)
 *---------------------------------------------------------------------------*/

bennet_tagged_domain ABSINT_PUBLIC(forward)(cn_term* term, bennet_absint_state* state) {
  cn_arena* arena = bennet_absint_arena();
  cn_arena_frame_id frame = cn_arena_get_frame(arena);
  /* Copy the root's cached value out (its type/domain pointers live outside the
   * arena) before releasing the tree. */
  bennet_tagged_domain result = ABSINT_ENGINE(fwd)(term, state)->val;
  cn_arena_restore_frame(arena, frame);
  return result;
}

bennet_absint_state* ABSINT_PUBLIC(backward)(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state) {
  /* target_sym is ABI-frozen (product.ml/domain.ml emit per-target calls)
   * but vestigial since the deposit-walk rework: one walk refines every
   * reachable sym, and callers read back only the target - extra deposits
   * are sound (and a bottom at any sym correctly bottoms the state). */
  (void)target_sym;
  if (!term || !state)
    return state;

  cn_arena* arena = bennet_absint_arena();
  cn_arena_frame_id frame = cn_arena_get_frame(arena);
  /* The returned state (cons cells + payloads) lives in the std allocator, not
   * the arena, so it survives the frame release. */
  bennet_absint_state* result =
      ABSINT_ENGINE(bwd)(ABSINT_ENGINE(fwd)(term, state), output_domain, state);
  cn_arena_restore_frame(arena, frame);
  return result;
}

bennet_absint_state* ABSINT_PUBLIC(backward_assume)(
    cn_term* term, bool value, bennet_absint_state* state) {
  /* Local iteration (fuel-bounded re-run while the state changes). At the
   * default fuel of 1 this is exactly one single pass; pointer equality is
   * a sound "unchanged" test on the persistent cons-list (set/meet always
   * cons), though a largely ineffective early exit - a no-op meet still
   * conses, so extra fuel mostly re-runs until exhausted. */
  cn_arena* arena = bennet_absint_arena();
  cn_arena_frame_id frame = cn_arena_get_frame(arena);
  /* One frame around the whole loop: each iteration's forward trees are dead
   * once its pass completes (the carried `cur` is std-allocated cons cells), so
   * they are all released together on return. */
  bennet_absint_state* cur = state;
  int fuel = bennet_get_dynamic_local_iterations();
  for (int i = 0; i < fuel; i++) {
    bennet_absint_state* next = ABSINT_ENGINE(assume)(term, value, cur);
    if (next == cur)
      break;
    cur = next;
  }
  cn_arena_restore_frame(arena, frame);
  return cur;
}

/*-----------------------------------------------------------------------------
 * Parameter cleanup (allows a second instantiation in another TU section)
 *---------------------------------------------------------------------------*/

#undef ABSINT_FTREE
#undef ABSINT_STATE
#undef ABSINT_TAGGED
#undef ABSINT_PUBLIC
#undef ABSINT_ENGINE
#undef ABSINT_BASIS
#undef ABSINT_CAT
#undef ABSINT_CAT_
#undef ABSINT_DOM
