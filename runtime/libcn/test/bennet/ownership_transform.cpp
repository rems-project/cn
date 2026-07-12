/**
 * @file ownership_transform.cpp
 * @brief Pins for the ownership cn_term walkers: bennet_ownership_transform_
 *        {forward,backward,backward_assume} and
 *        bennet_ownership_backward_propagate_to_syms.
 *
 * These pins captured the legacy walker behavior; the
 * straight port onto the shared walker engine deliberately flipped three
 * divergence-witness pins (marked DIVERGENCE below): the forward SYM
 * stored-tag guard, EQ-assume refinement reaching a symbol through ITE arms,
 * and the unsat bottom-all protocol on a bottom met. Everything else is
 * behavior-stable across the port.
 *
 * Pins assert domain payloads (bottom/before/after) only, never tagged
 * .type: tags on miss/default/bottom paths legitimately change with the port
 * (payload-identical), and nothing outside the walkers reads ownership tags.
 * Ownership lattice reminders: top = {0,0}; join = componentwise min (weaker
 * requirement); meet = componentwise max (stronger requirement); a bottom
 * element's before/after are uninitialized, so bottom pins check the flag
 * only.
 */

#include "absint_test_utils.hpp"
#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/internals/domains/ownership.h>
#include <cn-executable/bump_alloc.h>
#include <cn-smt/terms.h>

using absint_test::asym;
using absint_test::bump_bt;
using absint_test::loc_sym;
using absint_test::tagged_own;
using absint_test::tagged_own_bottom;
using absint_test::fuzz::binop_bool;

namespace {

#define OWN_T bennet_domain_ownership(uintptr_t)

cn_term* u64_const(uint64_t v) {
  return cn_smt_bits(false, 64, (intmax_t)v);
}

cn_term* u64_sym(cn_sym s) {
  return cn_smt_sym(s, cn_base_type_bits(false, 64));
}

void expect_own(bennet_tagged_domain d, size_t before, size_t after) {
  ASSERT_NE(d.domain, nullptr);
  const auto* own = (const OWN_T*)d.domain;
  EXPECT_FALSE(own->bottom);
  EXPECT_EQ(own->before, before);
  EXPECT_EQ(own->after, after);
}

void expect_own_top(bennet_tagged_domain d) {
  ASSERT_NE(d.domain, nullptr);
  const auto* own = (const OWN_T*)d.domain;
  EXPECT_FALSE(own->bottom);
  EXPECT_TRUE(bennet_domain_ownership_is_top_uintptr_t((OWN_T*)d.domain));
}

void expect_own_bottom(bennet_tagged_domain d) {
  ASSERT_NE(d.domain, nullptr);
  EXPECT_TRUE(((const OWN_T*)d.domain)->bottom);
}

bennet_tagged_domain state_own(bennet_absint_state* state, cn_sym s) {
  cn_base_type loc_bt = cn_base_type_simple(CN_BASE_LOC);
  return bennet_absint_state_get_ownership(state, asym(s), &loc_bt);
}

void expect_state_own(bennet_absint_state* state, cn_sym s, size_t before, size_t after) {
  expect_own(state_own(state, s), before, after);
}

void expect_state_own_top(bennet_absint_state* state, cn_sym s) {
  expect_own_top(state_own(state, s));
}

void expect_state_own_bottom(bennet_absint_state* state, cn_sym s) {
  expect_own_bottom(state_own(state, s));
}

}  // namespace

// -----------------------------------------------------------------------------
// Forward transformer
// -----------------------------------------------------------------------------

TEST_F(LibBennet, OwnershipFwdSymBound) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 8));

  expect_own(bennet_ownership_transform_forward(loc_sym(p), state), 4, 8);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdSymUnboundTop) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  expect_own_top(bennet_ownership_transform_forward(loc_sym(p), state));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdSymNonLocTagged) {
  // DIVERGENCE WITNESS (flipped by the engine port): the legacy forward SYM
  // guard read a stored non-LOC-tagged entry as top. Generated refine seeds
  // the ownership state with the variable's own type tag, so int-typed vars
  // can carry non-top ownership payloads under a CN_BASE_BITS tag; the
  // engine's plain state read returns the payload {4, 8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym n = cn_sym_from_string("n");
  bennet_tagged_domain bits_tagged = bennet_tagged_domain_create(
      bump_bt(cn_base_type_bits(false, 64)), bennet_domain_ownership_of(uintptr_t, 4, 8));
  state = bennet_absint_state_set_ownership(state, asym(n), bits_tagged);

  expect_own(bennet_ownership_transform_forward(u64_sym(n), state), 4, 8);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdConstUnopBinopTop) {
  // No transfer for CONST, UNOP, or BINOP: everything lands in the default
  // top arm.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym a = cn_sym_from_string("a");

  expect_own_top(bennet_ownership_transform_forward(u64_const(42), state));
  expect_own_top(
      bennet_ownership_transform_forward(absint_test::negate_term(u64_sym(a)), state));
  expect_own_top(
      bennet_ownership_transform_forward(cn_smt_add(u64_sym(a), u64_const(1)), state));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdMemberShift) {
  // q = p + 3 with p owning {4, 8}: [p-4, p+8) = [q-7, q+5).
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 8));

  expect_own(
      bennet_ownership_transform_forward(cn_smt_member_shift(loc_sym(p), 3), state),
      7,
      5);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdMemberShiftPastEndBottom) {
  // Shifting past the owned range (offset 9 > after 8) is unsatisfiable.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 8));

  expect_own_bottom(
      bennet_ownership_transform_forward(cn_smt_member_shift(loc_sym(p), 9), state));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdMemberShiftTopBaseTop) {
  // Top short-circuits the shift arithmetic: no info to adjust.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  expect_own_top(
      bennet_ownership_transform_forward(cn_smt_member_shift(loc_sym(p), 8), state));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdArrayShiftConstIdx) {
  // p owns {0, 40}; &p[3] with elem_size 4 owns {12, 28}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(0, 40));

  expect_own(bennet_ownership_transform_forward(
                 cn_smt_array_shift(loc_sym(p), 4, u64_const(3)), state),
      12,
      28);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdArrayShiftSymIdxTop) {
  // A non-constant index defeats the shift transfer.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(0, 40));

  expect_own_top(bennet_ownership_transform_forward(
      cn_smt_array_shift(loc_sym(p), 4, u64_sym(i)), state));

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdCastPassThrough) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 8));

  expect_own(bennet_ownership_transform_forward(
                 cn_smt_cast(cn_base_type_simple(CN_BASE_LOC), loc_sym(p)), state),
      4,
      8);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipFwdIteJoins) {
  // join = componentwise min: {4,8} v {2,16} = {2,8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 8));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(2, 16));

  expect_own(bennet_ownership_transform_forward(
                 cn_smt_ite(cn_smt_bool(true), loc_sym(p), loc_sym(q)), state),
      2,
      8);

  bennet_absint_state_free(state);
  cn_bump_free_after(frame);
}

// -----------------------------------------------------------------------------
// Targeted backward transformer
// -----------------------------------------------------------------------------

TEST_F(LibBennet, OwnershipBwdSymMeets) {
  // meet = componentwise max: {4,2} ^ {0,8} = {4,8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 2));

  auto* refined =
      bennet_ownership_transform_backward(loc_sym(p), asym(p), tagged_own(0, 8), state);
  expect_state_own(refined, p, 4, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdSymUnboundSets) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  auto* refined =
      bennet_ownership_transform_backward(loc_sym(p), asym(p), tagged_own(0, 8), state);
  expect_state_own(refined, p, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdOtherSymCopy) {
  // The term mentions only p; refining target q must change nothing.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");

  auto* refined =
      bennet_ownership_transform_backward(loc_sym(p), asym(q), tagged_own(0, 8), state);
  expect_state_own_top(refined, p);
  expect_state_own_top(refined, q);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdMemberShiftInverts) {
  // Requirement {0,2} at p+8 becomes {0,10} at p.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_member_shift(loc_sym(p), 8), asym(p), tagged_own(0, 2), state);
  expect_state_own(refined, p, 0, 10);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdArrayShiftConstInverts) {
  // {0,4} at &p[3] (elem_size 4) becomes {0,16} at p.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_array_shift(loc_sym(p), 4, u64_const(3)), asym(p), tagged_own(0, 4), state);
  expect_state_own(refined, p, 0, 16);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdArrayShiftSymIdxNoRefine) {
  // Non-constant index: no inversion possible, state unchanged.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_array_shift(loc_sym(p), 4, u64_sym(i)), asym(p), tagged_own(0, 4), state);
  expect_state_own_top(refined, p);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdIndexTargetNoRefine) {
  // The index is never refined (ownership is a pointer requirement).
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_array_shift(loc_sym(p), 4, u64_sym(i)), asym(i), tagged_own(0, 4), state);
  expect_state_own_top(refined, i);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdCastDescends) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_cast(cn_base_type_simple(CN_BASE_LOC), cn_smt_member_shift(loc_sym(p), 8)),
      asym(p),
      tagged_own(0, 2),
      state);
  expect_state_own(refined, p, 0, 10);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdIteJoinsBranches) {
  // {0,4} through +4 gives {0,8}; through +8 gives {0,12}; join = {0,8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  cn_term* ite = cn_smt_ite(cn_smt_bool(true),
      cn_smt_member_shift(loc_sym(p), 4),
      cn_smt_member_shift(loc_sym(p), 8));
  auto* refined =
      bennet_ownership_transform_backward(ite, asym(p), tagged_own(0, 4), state);
  expect_state_own(refined, p, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdIteDropsBottomArm) {
  // The then-arm's inversion overflows (offset SIZE_MAX) into bottom, so the
  // then-state is dropped and the else-arm result survives alone:
  // {0,4} through +8 = {0,12}, met with the prior {0,8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(0, 8));

  cn_term* ite = cn_smt_ite(cn_smt_bool(true),
      cn_smt_member_shift(loc_sym(p), SIZE_MAX),
      cn_smt_member_shift(loc_sym(p), 8));
  auto* refined =
      bennet_ownership_transform_backward(ite, asym(p), tagged_own(0, 4), state);
  expect_state_own(refined, p, 0, 12);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdBottomOutputSetsTargetBottom) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_member_shift(loc_sym(p), 8), asym(p), tagged_own_bottom(), state);
  expect_state_own_bottom(refined, p);
  EXPECT_TRUE(bennet_absint_state_is_bottom_ownership(refined));

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipBwdComparisonCopy) {
  // Comparisons are assume territory; the targeted walk makes no refinement.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");

  auto* refined = bennet_ownership_transform_backward(
      cn_smt_eq(loc_sym(p), loc_sym(q)), asym(p), tagged_own(0, 8), state);
  expect_state_own_top(refined, p);
  expect_state_own_top(refined, q);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

// -----------------------------------------------------------------------------
// Backward assume transformer
// -----------------------------------------------------------------------------

TEST_F(LibBennet, OwnershipAssumeEqMeetsBothSides) {
  // p == q: both sides pick up the met requirement {4,8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 0));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 8));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_eq(loc_sym(p), loc_sym(q)), true, state);
  expect_state_own(refined, p, 4, 8);
  expect_state_own(refined, q, 4, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeEqThroughShift) {
  // &p->f (offset 8) == q with q owning {0,4}: the met {0,4} inverts through
  // the shift into p = {0,12}; q keeps {0,4}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 4));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_eq(cn_smt_member_shift(loc_sym(p), 8), loc_sym(q)), true, state);
  expect_state_own(refined, p, 0, 12);
  expect_state_own(refined, q, 0, 4);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeEqFalseNoRefine) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 0));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_eq(loc_sym(p), loc_sym(q)), false, state);
  expect_state_own(refined, p, 4, 0);
  expect_state_own_top(refined, q);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeNotFlips) {
  // assume(not(p == q), false) == assume(p == q, true).
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 0));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 8));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_not(cn_smt_eq(loc_sym(p), loc_sym(q))), false, state);
  expect_state_own(refined, p, 4, 8);
  expect_state_own(refined, q, 4, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeAndThreads) {
  // and(p == q, q == r) true: the second conjunct sees the first's
  // refinements (left-to-right state threading).
  // eq(p,q): met {2,4} -> p = q = {2,4}; then eq(q,r): met of {2,4} and
  // {8,8} = {8,8} -> q = r = {8,8}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  cn_sym r = cn_sym_from_string("r");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(2, 0));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 4));
  state = bennet_absint_state_set_ownership(state, asym(r), tagged_own(8, 8));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_and(cn_smt_eq(loc_sym(p), loc_sym(q)), cn_smt_eq(loc_sym(q), loc_sym(r))),
      true,
      state);
  expect_state_own(refined, p, 2, 4);
  expect_state_own(refined, q, 8, 8);
  expect_state_own(refined, r, 8, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeOrFalseThreads) {
  // or(not(p == q), not(q == r)) false: each disjunct is assumed false, the
  // NOTs flip back to EQ-true, and the second disjunct sees the first's
  // refinements (same threading as and-true).
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  cn_sym r = cn_sym_from_string("r");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(2, 0));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 4));
  state = bennet_absint_state_set_ownership(state, asym(r), tagged_own(8, 8));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_or(cn_smt_not(cn_smt_eq(loc_sym(p), loc_sym(q))),
          cn_smt_not(cn_smt_eq(loc_sym(q), loc_sym(r)))),
      false,
      state);
  expect_state_own(refined, p, 2, 4);
  expect_state_own(refined, q, 8, 8);
  expect_state_own(refined, r, 8, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeAndFalseOrTrueNoRefine) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 0));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 8));

  cn_term* eq = cn_smt_eq(loc_sym(p), loc_sym(q));
  auto* r1 = bennet_ownership_transform_backward_assume(cn_smt_and(eq, eq), false, state);
  expect_state_own(r1, p, 4, 0);
  expect_state_own(r1, q, 0, 8);

  auto* r2 = bennet_ownership_transform_backward_assume(cn_smt_or(eq, eq), true, state);
  expect_state_own(r2, p, 4, 0);
  expect_state_own(r2, q, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(r1);
  bennet_absint_state_free(r2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumePointerCmpNoRule) {
  // Ownership has no rule for LT/LE pointer comparisons.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 0));
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 8));

  auto* r1 = bennet_ownership_transform_backward_assume(
      binop_bool(CN_BINOP_LT_POINTER, loc_sym(p), loc_sym(q)), true, state);
  expect_state_own(r1, p, 4, 0);
  expect_state_own(r1, q, 0, 8);

  auto* r2 = bennet_ownership_transform_backward_assume(
      binop_bool(CN_BINOP_LE_POINTER, loc_sym(p), loc_sym(q)), true, state);
  expect_state_own(r2, p, 4, 0);
  expect_state_own(r2, q, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(r1);
  bennet_absint_state_free(r2);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeEqIteDistinctArms) {
  // ite(c, p, q) == r with r owning {0,8}: the deposit walk has no ITE case,
  // so p and q stay top and only r is refined. (Value-stable across the engine
  // port: the engine's targeted walk refines each arm's sym, but joining with
  // the unconstrained other arm yields top again.)
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  cn_sym r = cn_sym_from_string("r");
  state = bennet_absint_state_set_ownership(state, asym(r), tagged_own(0, 8));

  cn_term* ite = cn_smt_ite(cn_smt_bool(true), loc_sym(p), loc_sym(q));
  auto* refined =
      bennet_ownership_transform_backward_assume(cn_smt_eq(ite, loc_sym(r)), true, state);
  expect_state_own_top(refined, p);
  expect_state_own_top(refined, q);
  expect_state_own(refined, r, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeEqIteSameSymArms) {
  // DIVERGENCE WITNESS (flipped by the engine port): ite(c, &p->a, &p->b) == r.
  // The legacy deposit walk had no ITE case, so p was never refined. The
  // engine's targeted backward reaches p through both arms and joins the
  // inverted requirements: {0,8}+4 = {0,12} joined with {0,8}+8 = {0,16}
  // gives p = {0,12}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym r = cn_sym_from_string("r");
  state = bennet_absint_state_set_ownership(state, asym(r), tagged_own(0, 8));

  cn_term* ite = cn_smt_ite(cn_smt_bool(true),
      cn_smt_member_shift(loc_sym(p), 4),
      cn_smt_member_shift(loc_sym(p), 8));
  auto* refined =
      bennet_ownership_transform_backward_assume(cn_smt_eq(ite, loc_sym(r)), true, state);
  expect_state_own(refined, p, 0, 12);
  expect_state_own(refined, r, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeEqBottomMet) {
  // DIVERGENCE WITNESS (flipped by the engine port): ite(c, p, q) == r with r
  // bottom. The met requirement is bottom; the legacy deposit walk pushed it
  // only along supported paths (r itself; the ITE side untouched). The
  // engine's unsat protocol bottoms every sym of both sides (p, q, r).
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");
  cn_sym r = cn_sym_from_string("r");
  state = bennet_absint_state_set_ownership(state, asym(r), tagged_own_bottom());

  cn_term* ite = cn_smt_ite(cn_smt_bool(true), loc_sym(p), loc_sym(q));
  auto* refined =
      bennet_ownership_transform_backward_assume(cn_smt_eq(ite, loc_sym(r)), true, state);
  expect_state_own_bottom(refined, p);
  expect_state_own_bottom(refined, q);
  expect_state_own_bottom(refined, r);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipAssumeEqNonConstIdxNoDeposit) {
  // array_shift(p, 4, i) == q: the deposit walk stops at the non-constant
  // index, so only q picks up the met requirement.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");
  cn_sym q = cn_sym_from_string("q");
  state = bennet_absint_state_set_ownership(state, asym(q), tagged_own(0, 8));

  auto* refined = bennet_ownership_transform_backward_assume(
      cn_smt_eq(cn_smt_array_shift(loc_sym(p), 4, u64_sym(i)), loc_sym(q)), true, state);
  expect_state_own_top(refined, p);
  expect_state_own_top(refined, i);
  expect_state_own(refined, q, 0, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

// -----------------------------------------------------------------------------
// backward_propagate_to_syms (the assign.c blame channel; survives the port
// verbatim, so these pins never flip)
// -----------------------------------------------------------------------------

TEST_F(LibBennet, OwnershipPropagateIteNoDeposit) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  cn_sym q = cn_sym_from_string("q");

  cn_term* ite = cn_smt_ite(cn_smt_bool(true), loc_sym(p), loc_sym(q));
  auto* refined = bennet_ownership_backward_propagate_to_syms(
      ite, bennet_domain_ownership_of(uintptr_t, 0, 8), state);
  expect_state_own_top(refined, p);
  expect_state_own_top(refined, q);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}

TEST_F(LibBennet, OwnershipPropagateMeetsExisting) {
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  auto* state = bennet_absint_state_create();
  cn_sym p = cn_sym_from_string("p");
  state = bennet_absint_state_set_ownership(state, asym(p), tagged_own(4, 0));

  auto* refined = bennet_ownership_backward_propagate_to_syms(
      loc_sym(p), bennet_domain_ownership_of(uintptr_t, 0, 8), state);
  expect_state_own(refined, p, 4, 8);

  bennet_absint_state_free(state);
  bennet_absint_state_free(refined);
  cn_bump_free_after(frame);
}
