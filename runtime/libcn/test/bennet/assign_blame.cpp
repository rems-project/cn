/**
 * @file assign_blame.cpp
 * @brief Tests for bennet_assign_backward_blame, the backward abstract
 *        interpretation over assignment address terms used by
 *        --dynamic-absint-assign=also.
 *
 * The function backward-propagates the ownership requirement
 * {before=0, after=bytes} through the address term and reports each "other
 * var" through the failure/blame channel: vars that picked up a non-top
 * ownership domain are blamed with that domain, the rest get a plain blame.
 *
 * Domain readback caveat: bennet_failure_get_domain_uintptr_t returns
 * bennet_domain(uintptr_t)*, the generated product type. In this test binary
 * the product is the 1-field {ownership car} defined in ownership_domain.cpp,
 * so casting to bennet_domain_ownership(uintptr_t)* (offset 0) is valid here.
 * This mirrors the D6 type-pun in assign.c (doc/RUNTIME-ABSINT.md §3.6),
 * which is size-benign for this product; revisit when P4 fixes D6.
 */

#include "absint_test_utils.hpp"
#include "harness.hpp"
#include <gtest/gtest.h>

#include <bennet/dsl/assign.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/state/failure.h>
#include <cn-executable/bump_alloc.h>
#include <cn-smt/terms.h>

using absint_test::asym;

namespace {

// Stable ids for blamed "variables" (the blame channel keys on addresses).
static int slot_p;
static int slot_n;

cn_term* loc_sym(cn_sym s) {
  return cn_smt_sym(s, cn_base_type_simple(CN_BASE_LOC));
}

cn_term* u64_const(uint64_t v) {
  return cn_smt_bits(false, 64, (intmax_t)v);
}

// Read the ownership component out of a blamed product domain (see file
// header for why this cast is valid in this binary).
const bennet_domain_ownership(uintptr_t) * blamed_ownership(const void* id) {
  return (const bennet_domain_ownership(uintptr_t)*)bennet_failure_get_domain_uintptr_t(
      id);
}

void expect_blamed_ownership(const void* id, size_t before, size_t after) {
  ASSERT_TRUE(bennet_failure_is_blamed(id));
  const auto* own = blamed_ownership(id);
  ASSERT_NE(own, nullptr);
  EXPECT_FALSE(own->bottom);
  EXPECT_EQ(own->before, before);
  EXPECT_EQ(own->after, after);
}

}  // namespace

TEST_F(LibBennet, BlameArrayShiftConstIndex) {
  // &p[3] with elem_size 4, writing 4 bytes: the requirement {0, 4} at the
  // shifted address becomes {0, 4 + 4*3} = {0, 16} at p.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_term* addr = cn_smt_array_shift(loc_sym(p), 4, u64_const(3));

  const void* ids[] = {&slot_p};
  const bennet_absint_sym syms[] = {asym(p)};
  bennet_assign_backward_blame(addr, 1, ids, syms, 4);

  EXPECT_EQ(bennet_failure_get_failure_type(), BENNET_FAILURE_ASSERT);
  expect_blamed_ownership(&slot_p, 0, 16);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameMemberShift) {
  // &p->f with offset 8, writing 2 bytes: {0, 2} becomes {0, 10} at p.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_term* addr = cn_smt_member_shift(loc_sym(p), 8);

  const void* ids[] = {&slot_p};
  const bennet_absint_sym syms[] = {asym(p)};
  bennet_assign_backward_blame(addr, 1, ids, syms, 2);

  expect_blamed_ownership(&slot_p, 0, 10);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameNestedShifts) {
  // &(p->f)[3] with member offset 4 and elem_size 2, writing 2 bytes:
  // {0,2} -> array-shift backward (+6) -> {0,8} -> member-shift backward (+4)
  // -> {0,12} at p.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_term* addr = cn_smt_array_shift(cn_smt_member_shift(loc_sym(p), 4), 2, u64_const(3));

  const void* ids[] = {&slot_p};
  const bennet_absint_sym syms[] = {asym(p)};
  bennet_assign_backward_blame(addr, 1, ids, syms, 2);

  expect_blamed_ownership(&slot_p, 0, 12);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameBareSym) {
  // Writing 4 bytes through p directly: p needs {0, 4}.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_term* addr = loc_sym(p);

  const void* ids[] = {&slot_p};
  const bennet_absint_sym syms[] = {asym(p)};
  bennet_assign_backward_blame(addr, 1, ids, syms, 4);

  expect_blamed_ownership(&slot_p, 0, 4);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameCastRecurses) {
  // Backward propagation recurses through casts unchanged.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_term* addr = cn_smt_cast(cn_base_type_simple(CN_BASE_LOC), loc_sym(p));

  const void* ids[] = {&slot_p};
  const bennet_absint_sym syms[] = {asym(p)};
  bennet_assign_backward_blame(addr, 1, ids, syms, 4);

  expect_blamed_ownership(&slot_p, 0, 4);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameVarNotInTerm) {
  // A var absent from the address term picks up no domain: plain blame,
  // no stored domain.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_sym n = cn_sym_from_string("n");
  cn_term* addr = loc_sym(p);

  const void* ids[] = {&slot_p, &slot_n};
  const bennet_absint_sym syms[] = {asym(p), asym(n)};
  bennet_assign_backward_blame(addr, 2, ids, syms, 4);

  expect_blamed_ownership(&slot_p, 0, 4);
  EXPECT_TRUE(bennet_failure_is_blamed(&slot_n));
  EXPECT_EQ(bennet_failure_get_domain_uintptr_t(&slot_n), nullptr);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameSymIndexArrayShiftFallsBack) {
  // A non-constant array index stops the ownership walker (no deposit is
  // made), so both vars fall back to plain blame.
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");
  cn_sym i = cn_sym_from_string("i");
  cn_term* i_term = cn_smt_sym(i, cn_base_type_bits(false, 64));
  cn_term* addr = cn_smt_array_shift(loc_sym(p), 4, i_term);

  const void* ids[] = {&slot_p, &slot_n};
  const bennet_absint_sym syms[] = {asym(p), asym(i)};
  bennet_assign_backward_blame(addr, 2, ids, syms, 4);

  EXPECT_TRUE(bennet_failure_is_blamed(&slot_p));
  EXPECT_EQ(bennet_failure_get_domain_uintptr_t(&slot_p), nullptr);
  EXPECT_TRUE(bennet_failure_is_blamed(&slot_n));
  EXPECT_EQ(bennet_failure_get_domain_uintptr_t(&slot_n), nullptr);

  cn_bump_free_after(frame);
}

TEST_F(LibBennet, BlameRepeatMeets) {
  // Blaming the same id twice meets the stored domains; for ownership the
  // meet is the componentwise max (the stronger requirement).
  cn_bump_frame_id frame = cn_bump_get_frame_id();
  cn_sym p = cn_sym_from_string("p");

  const void* ids[] = {&slot_p};
  const bennet_absint_sym syms[] = {asym(p)};

  // {0, 2+8=10}, then {0, 4+16=20}
  bennet_assign_backward_blame(cn_smt_member_shift(loc_sym(p), 8), 1, ids, syms, 2);
  bennet_assign_backward_blame(cn_smt_member_shift(loc_sym(p), 16), 1, ids, syms, 4);

  expect_blamed_ownership(&slot_p, 0, 20);

  cn_bump_free_after(frame);
}
