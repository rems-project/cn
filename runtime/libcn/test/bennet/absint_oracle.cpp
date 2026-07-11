/**
 * @file absint_oracle.cpp
 * @brief Exhaustive 8-bit soundness oracle for the dynamic abstract domains
 *        (congr, wint, tnum) against the concrete evaluator cn_eval_term.
 *
 * For width-8 terms over at most two symbols, every concrete environment is
 * enumerated (256 or 65536), so the checks are exhaustive, not sampled:
 *
 *  - Forward soundness: for every environment drawn from the input domains,
 *    the concrete result lies in the forward transformer's output domain.
 *  - Backward-assume soundness: every environment (from the input domains)
 *    that satisfies the assumed condition survives the refinement; a bottom
 *    refined state implies the condition was unsatisfiable.
 *  - Precision: golden gamma-cardinalities for a few known-precise cases, and
 *    RecordProperty totals so silent precision regressions show up in logs.
 *
 * Terms are built through leaf-parameterized builders: the same builder is
 * called once with SYM leaves (abstract side) and once per environment with
 * constant leaves, so the concrete term is closed (cn_eval_term asserts on
 * unbound symbols) and both sides share one shape.
 */

#include "absint_test_utils.hpp"
#include <gtest/gtest.h>

#include <cn-smt/memory/std_alloc.h>

using absint_test::asym;
using absint_test::bump_bt;
using absint_test::eval_bool;
using absint_test::negate_term;
using absint_test::tagged_congr_u8;
using absint_test::tagged_tnum_u8;
using absint_test::tagged_wint_u8;
using absint_test::u8_const;

namespace {

/*-----------------------------------------------------------------------------
 * Fixture: cn_eval_term needs the std test allocator; terms and cn values are
 * bump-allocated. No bennet subsystems are used (both allocators lazy-init).
 *---------------------------------------------------------------------------*/

class AbsintOracle : public ::testing::Test {
 protected:
  cn_bump_frame_id frame_;

  void SetUp() override {
    std_set_default_alloc();
    frame_ = cn_bump_get_frame_id();
  }

  void TearDown() override {
    cn_bump_free_after(frame_);
    std_free_all();
  }
};

/*-----------------------------------------------------------------------------
 * Domain policy: one struct of plain function pointers per domain. (The
 * no-vtable rule is for the runtime library; test-side dispatch is fine.)
 * Concrete values are passed as their 8/16-bit pattern in a uint64_t; check()
 * re-interprets per the tagged type's width and signedness.
 *---------------------------------------------------------------------------*/

enum InputKind {
  kTop,
  kD1,
  kD2
};

struct DomainOracle {
  const char* name;
  bennet_tagged_domain (*forward)(cn_term*, bennet_absint_state*);
  bennet_absint_state* (*assume)(cn_term*, bool, bennet_absint_state*);
  bennet_absint_state* (*set)(
      bennet_absint_state*, bennet_absint_sym, bennet_tagged_domain);
  bennet_tagged_domain (*get)(bennet_absint_state*, bennet_absint_sym, cn_base_type*);
  bool (*state_is_bottom)(bennet_absint_state*);
  bool (*check)(uint64_t, bennet_tagged_domain*);
  bennet_tagged_domain (*make_input)(InputKind);
};

#define ORACLE_CHECK_IMPL(dom)                                                           \
  bool dom##_oracle_check(uint64_t v, bennet_tagged_domain* d) {                         \
    cn_base_type* t = d->type;                                                           \
    if (t->tag == CN_BASE_LOC) {                                                         \
      return bennet_domain_##dom##_check_uint64_t(                                       \
          v, (bennet_domain_##dom(uint64_t)*)d->domain);                                 \
    }                                                                                    \
    assert(t->tag == CN_BASE_BITS);                                                      \
    bool is_signed = t->data.bits.is_signed;                                             \
    switch (t->data.bits.size_bits) {                                                    \
      case 8:                                                                            \
        return is_signed ? bennet_domain_##dom##_check_int8_t(                           \
                               (int8_t)v, (bennet_domain_##dom(int8_t)*)d->domain)       \
                         : bennet_domain_##dom##_check_uint8_t(                          \
                               (uint8_t)v, (bennet_domain_##dom(uint8_t)*)d->domain);    \
      case 16:                                                                           \
        return is_signed ? bennet_domain_##dom##_check_int16_t(                          \
                               (int16_t)v, (bennet_domain_##dom(int16_t)*)d->domain)     \
                         : bennet_domain_##dom##_check_uint16_t(                         \
                               (uint16_t)v, (bennet_domain_##dom(uint16_t)*)d->domain);  \
      case 64:                                                                           \
        return is_signed ? bennet_domain_##dom##_check_int64_t(                          \
                               (int64_t)v, (bennet_domain_##dom(int64_t)*)d->domain)     \
                         : bennet_domain_##dom##_check_uint64_t(                         \
                               v, (bennet_domain_##dom(uint64_t)*)d->domain);            \
      default:                                                                           \
        ADD_FAILURE() << "unsupported width in " #dom "_oracle_check";                   \
        return true;                                                                     \
    }                                                                                    \
  }

ORACLE_CHECK_IMPL(congr)
ORACLE_CHECK_IMPL(wint)
ORACLE_CHECK_IMPL(tnum)

#undef ORACLE_CHECK_IMPL

bennet_tagged_domain congr_oracle_input(InputKind k) {
  switch (k) {
    case kD1:
      return tagged_congr_u8(4, 1);  // gamma: {1,5,...,253}, 64 values
    case kD2:
      return tagged_congr_u8(2, 0);  // evens, 128 values
    default:
      return bennet_tagged_domain_create(
          bump_bt(cn_base_type_bits(false, 8)), bennet_domain_congr_top_uint8_t());
  }
}

bennet_tagged_domain wint_oracle_input(InputKind k) {
  switch (k) {
    case kD1:
      return tagged_wint_u8(8, 29);  // 22 values
    case kD2:
      return tagged_wint_u8(250, 5);  // wraps: {250..255, 0..5}, 12 values
    default:
      return bennet_tagged_domain_create(
          bump_bt(cn_base_type_bits(false, 8)), bennet_domain_wint_top_uint8_t());
  }
}

bennet_tagged_domain tnum_oracle_input(InputKind k) {
  switch (k) {
    case kD1:
      return tagged_tnum_u8(1, 0x0C);  // {1,5,9,13}, 4 values
    case kD2:
      return tagged_tnum_u8(0, 0xF0);  // low nibble zero, 16 values
    default:
      return bennet_tagged_domain_create(
          bump_bt(cn_base_type_bits(false, 8)), bennet_domain_tnum_top_uint8_t());
  }
}

const DomainOracle kDomains[] = {
    {"congr",
        bennet_congr_transform_forward,
        bennet_congr_transform_backward_assume,
        bennet_absint_state_set_congr,
        bennet_absint_state_get_congr,
        bennet_absint_state_is_bottom_congr,
        congr_oracle_check,
        congr_oracle_input},
    {"wint",
        bennet_wint_transform_forward,
        bennet_wint_transform_backward_assume,
        bennet_absint_state_set_wint,
        bennet_absint_state_get_wint,
        bennet_absint_state_is_bottom_wint,
        wint_oracle_check,
        wint_oracle_input},
    {"tnum",
        bennet_tnum_transform_forward,
        bennet_tnum_transform_backward_assume,
        bennet_absint_state_set_tnum,
        bennet_absint_state_get_tnum,
        bennet_absint_state_is_bottom_tnum,
        tnum_oracle_check,
        tnum_oracle_input},
};
constexpr int kNumDomains = 3;

int gamma_card_u8(const DomainOracle& dom, bennet_tagged_domain* d) {
  int n = 0;
  for (int v = 0; v < 256; v++) {
    n += dom.check((uint64_t)v, d) ? 1 : 0;
  }
  return n;
}

/*-----------------------------------------------------------------------------
 * Term corpus: leaf-parameterized builders. Constant divisors are nonzero and
 * constant shifts stay in 0..7 (larger would be UB in the concrete evaluator).
 *---------------------------------------------------------------------------*/

struct TermEntry {
  const char* name;
  int arity;  // 1 or 2 symbols
  cn_term* (*build)(cn_term* x, cn_term* y);
};

uint64_t eval_numeric_u(cn_term* t) {
  assert(t->base_type.tag == CN_BASE_BITS && !t->base_type.data.bits.is_signed);
  switch (t->base_type.data.bits.size_bits) {
    case 8:
      return absint_test::eval_u8(t);
    case 16:
      return absint_test::eval_u16(t);
    default:
      ADD_FAILURE() << "unsupported eval width";
      return 0;
  }
}

const TermEntry kNumericOneSym[] = {
    {"x+3", 1, +[](cn_term* x, cn_term*) { return cn_smt_add(x, u8_const(3)); }},
    {"x-3", 1, +[](cn_term* x, cn_term*) { return cn_smt_sub(x, u8_const(3)); }},
    {"3-x", 1, +[](cn_term* x, cn_term*) { return cn_smt_sub(u8_const(3), x); }},
    {"x*3", 1, +[](cn_term* x, cn_term*) { return cn_smt_mul(x, u8_const(3)); }},
    {"x*4", 1, +[](cn_term* x, cn_term*) { return cn_smt_mul(x, u8_const(4)); }},
    {"x<<2", 1, +[](cn_term* x, cn_term*) { return cn_smt_shift_left(x, u8_const(2)); }},
    {"x>>1", 1, +[](cn_term* x, cn_term*) { return cn_smt_shift_right(x, u8_const(1)); }},
    {"x/3", 1, +[](cn_term* x, cn_term*) { return cn_smt_div(x, u8_const(3)); }},
    {"x%4", 1, +[](cn_term* x, cn_term*) { return cn_smt_mod(x, u8_const(4)); }},
    {"rem(x,4)", 1, +[](cn_term* x, cn_term*) { return cn_smt_rem(x, u8_const(4)); }},
    {"negate(x)", 1, +[](cn_term* x, cn_term*) { return negate_term(x); }},
    {"0-x", 1, +[](cn_term* x, cn_term*) { return cn_smt_sub(u8_const(0), x); }},
    {"ite(x<128,x,x+1)",
        1,
        +[](cn_term* x, cn_term*) {
          return cn_smt_ite(cn_smt_lt(x, u8_const(128)), x, cn_smt_add(x, u8_const(1)));
        }},
    {"(u16)x",
        1,
        +[](cn_term* x, cn_term*) {
          return cn_smt_cast(cn_base_type_bits(false, 16), x);
        }},
    {"x&0x0F", 1, +[](cn_term* x, cn_term*) { return cn_smt_bw_and(x, u8_const(0x0F)); }},
    {"x|0x11", 1, +[](cn_term* x, cn_term*) { return cn_smt_bw_or(x, u8_const(0x11)); }},
    {"x^0x3C", 1, +[](cn_term* x, cn_term*) { return cn_smt_bw_xor(x, u8_const(0x3C)); }},
};

const TermEntry kCondOneSym[] = {
    {"x%4==1",
        1,
        +[](cn_term* x, cn_term*) {
          return cn_smt_eq(cn_smt_mod(x, u8_const(4)), u8_const(1));
        }},
    {"x==5", 1, +[](cn_term* x, cn_term*) { return cn_smt_eq(x, u8_const(5)); }},
    {"x<100", 1, +[](cn_term* x, cn_term*) { return cn_smt_lt(x, u8_const(100)); }},
    {"x<=99", 1, +[](cn_term* x, cn_term*) { return cn_smt_le(x, u8_const(99)); }},
    {"!(x==5)",
        1,
        +[](cn_term* x, cn_term*) { return cn_smt_not(cn_smt_eq(x, u8_const(5))); }},
    {"x<200 && x%4==1",
        1,
        +[](cn_term* x, cn_term*) {
          return cn_smt_and(cn_smt_lt(x, u8_const(200)),
              cn_smt_eq(cn_smt_mod(x, u8_const(4)), u8_const(1)));
        }},
    {"x==3 || x==7",
        1,
        +[](cn_term* x, cn_term*) {
          return cn_smt_or(cn_smt_eq(x, u8_const(3)), cn_smt_eq(x, u8_const(7)));
        }},
};

const TermEntry kNumericTwoSym[] = {
    {"x+y", 2, +[](cn_term* x, cn_term* y) { return cn_smt_add(x, y); }},
    {"x-y", 2, +[](cn_term* x, cn_term* y) { return cn_smt_sub(x, y); }},
    {"x*y", 2, +[](cn_term* x, cn_term* y) { return cn_smt_mul(x, y); }},
};

const TermEntry kCondTwoSym[] = {
    {"x+3==y",
        2,
        +[](cn_term* x, cn_term* y) { return cn_smt_eq(cn_smt_add(x, u8_const(3)), y); }},
    {"x==y && x<10",
        2,
        +[](cn_term* x, cn_term* y) {
          return cn_smt_and(cn_smt_eq(x, y), cn_smt_lt(x, u8_const(10)));
        }},
};

/*-----------------------------------------------------------------------------
 * Soundness sweeps. Each environment is evaluated once and checked against
 * all domains (the eval is domain-independent).
 *---------------------------------------------------------------------------*/

void run_forward_entry(
    const TermEntry& e, InputKind kx, InputKind ky, long gamma_total[kNumDomains]) {
  SCOPED_TRACE(
      ::testing::Message() << "forward " << e.name << " kx=" << kx << " ky=" << ky);
  cn_base_type u8 = cn_base_type_bits(false, 8);
  cn_sym sx = cn_sym_from_string("x");
  cn_sym sy = cn_sym_from_string("y");
  cn_term* sym_term =
      e.build(cn_smt_sym(sx, u8), (e.arity == 2) ? cn_smt_sym(sy, u8) : nullptr);

  bennet_tagged_domain in_x[kNumDomains], in_y[kNumDomains], out[kNumDomains];
  for (int d = 0; d < kNumDomains; d++) {
    const DomainOracle& dom = kDomains[d];
    bennet_absint_state* st = bennet_absint_state_create();
    in_x[d] = dom.make_input(kx);
    if (kx != kTop) {
      st = dom.set(st, asym(sx), in_x[d]);
    }
    if (e.arity == 2) {
      in_y[d] = dom.make_input(ky);
      if (ky != kTop) {
        st = dom.set(st, asym(sy), in_y[d]);
      }
    }
    out[d] = dom.forward(sym_term, st);
  }

  long fails[kNumDomains] = {0};
  int first_vx[kNumDomains] = {0}, first_vy[kNumDomains] = {0};
  uint64_t first_res[kNumDomains] = {0};
  const int vy_count = (e.arity == 2) ? 256 : 1;

  for (int vx = 0; vx < 256; vx++) {
    for (int vy = 0; vy < vy_count; vy++) {
      cn_bump_frame_id f = cn_bump_get_frame_id();
      cn_term* conc = e.build(u8_const((uint8_t)vx), u8_const((uint8_t)vy));
      uint64_t res = eval_numeric_u(conc);
      for (int d = 0; d < kNumDomains; d++) {
        const DomainOracle& dom = kDomains[d];
        if (kx != kTop && !dom.check((uint64_t)vx, &in_x[d])) {
          continue;
        }
        if (e.arity == 2 && ky != kTop && !dom.check((uint64_t)vy, &in_y[d])) {
          continue;
        }
        if (!dom.check(res, &out[d])) {
          if (fails[d] == 0) {
            first_vx[d] = vx;
            first_vy[d] = vy;
            first_res[d] = res;
          }
          fails[d]++;
        }
      }
      cn_bump_free_after(f);
    }
  }

  for (int d = 0; d < kNumDomains; d++) {
    EXPECT_EQ(fails[d], 0) << kDomains[d].name << ": concrete result " << first_res[d]
                           << " escapes forward domain at x=" << first_vx[d]
                           << " y=" << first_vy[d] << " (+" << (fails[d] - 1) << " more)";
    gamma_total[d] += gamma_card_u8(kDomains[d], &out[d]);
  }
}

void run_assume_entry(const TermEntry& e, bool polarity, InputKind kx, InputKind ky) {
  SCOPED_TRACE(::testing::Message() << "assume " << e.name << " polarity=" << polarity
                                    << " kx=" << kx << " ky=" << ky);
  cn_base_type u8 = cn_base_type_bits(false, 8);
  cn_sym sx = cn_sym_from_string("x");
  cn_sym sy = cn_sym_from_string("y");
  cn_term* sym_term =
      e.build(cn_smt_sym(sx, u8), (e.arity == 2) ? cn_smt_sym(sy, u8) : nullptr);

  bennet_tagged_domain in_x[kNumDomains], in_y[kNumDomains];
  bennet_tagged_domain rx[kNumDomains], ry[kNumDomains];
  bennet_absint_state* refined[kNumDomains];
  for (int d = 0; d < kNumDomains; d++) {
    const DomainOracle& dom = kDomains[d];
    bennet_absint_state* st = bennet_absint_state_create();
    in_x[d] = dom.make_input(kx);
    if (kx != kTop) {
      st = dom.set(st, asym(sx), in_x[d]);
    }
    if (e.arity == 2) {
      in_y[d] = dom.make_input(ky);
      if (ky != kTop) {
        st = dom.set(st, asym(sy), in_y[d]);
      }
    }
    refined[d] = dom.assume(sym_term, polarity, st);
    rx[d] = dom.get(refined[d], asym(sx), &u8);
    if (e.arity == 2) {
      ry[d] = dom.get(refined[d], asym(sy), &u8);
    }
  }

  long fails[kNumDomains] = {0}, sat[kNumDomains] = {0};
  int first_vx[kNumDomains] = {0}, first_vy[kNumDomains] = {0};
  const int vy_count = (e.arity == 2) ? 256 : 1;

  for (int vx = 0; vx < 256; vx++) {
    for (int vy = 0; vy < vy_count; vy++) {
      cn_bump_frame_id f = cn_bump_get_frame_id();
      cn_term* conc = e.build(u8_const((uint8_t)vx), u8_const((uint8_t)vy));
      bool val = eval_bool(conc);
      cn_bump_free_after(f);
      if (val != polarity) {
        continue;
      }
      for (int d = 0; d < kNumDomains; d++) {
        const DomainOracle& dom = kDomains[d];
        if (kx != kTop && !dom.check((uint64_t)vx, &in_x[d])) {
          continue;
        }
        if (e.arity == 2 && ky != kTop && !dom.check((uint64_t)vy, &in_y[d])) {
          continue;
        }
        sat[d]++;
        bool survives = dom.check((uint64_t)vx, &rx[d]) &&
                        (e.arity < 2 || dom.check((uint64_t)vy, &ry[d]));
        if (!survives) {
          if (fails[d] == 0) {
            first_vx[d] = vx;
            first_vy[d] = vy;
          }
          fails[d]++;
        }
      }
    }
  }

  for (int d = 0; d < kNumDomains; d++) {
    EXPECT_EQ(fails[d], 0) << kDomains[d].name << ": satisfying env x=" << first_vx[d]
                           << " y=" << first_vy[d] << " does not survive refinement (+"
                           << (fails[d] - 1) << " more)";
    if (kDomains[d].state_is_bottom(refined[d])) {
      EXPECT_EQ(sat[d], 0) << kDomains[d].name
                           << ": refined state is bottom but the condition is "
                              "satisfiable within the inputs";
    }
  }
}

}  // namespace

/*-----------------------------------------------------------------------------
 * Exhaustive sweeps
 *---------------------------------------------------------------------------*/

TEST_F(AbsintOracle, ForwardOneSym) {
  long gamma_total[kNumDomains] = {0};
  for (const TermEntry& e : kNumericOneSym) {
    for (InputKind kx : {kTop, kD1}) {
      run_forward_entry(e, kx, kTop, gamma_total);
    }
  }
  for (int d = 0; d < kNumDomains; d++) {
    RecordProperty(std::string("gamma_total_") + kDomains[d].name, (int)gamma_total[d]);
  }
}

TEST_F(AbsintOracle, AssumeOneSym) {
  for (const TermEntry& e : kCondOneSym) {
    for (bool polarity : {true, false}) {
      for (InputKind kx : {kTop, kD1}) {
        run_assume_entry(e, polarity, kx, kTop);
      }
    }
  }
}

TEST_F(AbsintOracle, ForwardTwoSym) {
  long gamma_total[kNumDomains] = {0};
  for (const TermEntry& e : kNumericTwoSym) {
    run_forward_entry(e, kD1, kD2, gamma_total);
  }
  for (int d = 0; d < kNumDomains; d++) {
    RecordProperty(
        std::string("gamma_total_twosym_") + kDomains[d].name, (int)gamma_total[d]);
  }
}

TEST_F(AbsintOracle, AssumeTwoSym) {
  for (const TermEntry& e : kCondTwoSym) {
    for (bool polarity : {true, false}) {
      run_assume_entry(e, polarity, kTop, kTop);
    }
  }
}

/*-----------------------------------------------------------------------------
 * Signed probe: minimal i8 coverage, isolated so it can be dropped if the
 * evaluator's signed semantics turn out to differ.
 *---------------------------------------------------------------------------*/

TEST_F(AbsintOracle, SignedProbeI8) {
  cn_base_type i8 = cn_base_type_bits(true, 8);
  cn_sym sx = cn_sym_from_string("x");

  // Forward: x + 3 from top. Every concrete result must be in the output.
  cn_term* add_sym = cn_smt_add(cn_smt_sym(sx, i8), cn_smt_bits(true, 8, 3));
  for (int d = 0; d < kNumDomains; d++) {
    SCOPED_TRACE(kDomains[d].name);
    bennet_absint_state* st = bennet_absint_state_create();
    bennet_tagged_domain out = kDomains[d].forward(add_sym, st);
    long fails = 0;
    for (int v = 0; v < 256; v++) {
      cn_bump_frame_id f = cn_bump_get_frame_id();
      cn_term* conc =
          cn_smt_add(cn_smt_bits(true, 8, (int8_t)v), cn_smt_bits(true, 8, 3));
      uint64_t res = (uint8_t)absint_test::eval_i8(conc);
      cn_bump_free_after(f);
      if (!kDomains[d].check(res, &out)) {
        fails++;
      }
    }
    EXPECT_EQ(fails, 0);
  }

  // Assume: x < 0, both polarities, from top. Satisfying envs must survive.
  cn_term* lt_sym = cn_smt_lt(cn_smt_sym(sx, i8), cn_smt_bits(true, 8, 0));
  for (bool polarity : {true, false}) {
    for (int d = 0; d < kNumDomains; d++) {
      SCOPED_TRACE(::testing::Message() << kDomains[d].name << " polarity=" << polarity);
      bennet_absint_state* st = bennet_absint_state_create();
      bennet_absint_state* refined = kDomains[d].assume(lt_sym, polarity, st);
      bennet_tagged_domain rx = kDomains[d].get(refined, asym(sx), &i8);
      long fails = 0, sat = 0;
      for (int v = 0; v < 256; v++) {
        cn_bump_frame_id f = cn_bump_get_frame_id();
        cn_term* conc =
            cn_smt_lt(cn_smt_bits(true, 8, (int8_t)v), cn_smt_bits(true, 8, 0));
        bool val = eval_bool(conc);
        cn_bump_free_after(f);
        if (val != polarity) {
          continue;
        }
        sat++;
        if (!kDomains[d].check((uint64_t)v, &rx)) {
          fails++;
        }
      }
      EXPECT_EQ(fails, 0);
      if (kDomains[d].state_is_bottom(refined)) {
        EXPECT_EQ(sat, 0);
      }
    }
  }
}

/*-----------------------------------------------------------------------------
 * Precision goldens: exact gamma-cardinalities for known-precise cases.
 * These catch silent precision regressions the soundness sweeps cannot.
 *---------------------------------------------------------------------------*/

TEST_F(AbsintOracle, GoldenCardinalities) {
  cn_base_type u8 = cn_base_type_bits(false, 8);
  cn_sym sx = cn_sym_from_string("x");
  cn_term* x = cn_smt_sym(sx, u8);
  cn_term* add3 = cn_smt_add(x, u8_const(3));

  // congr: (4Z+1) + 3 = 4Z+0 -> 64 values
  {
    bennet_absint_state* st = bennet_absint_state_create();
    st = bennet_absint_state_set_congr(st, asym(sx), tagged_congr_u8(4, 1));
    bennet_tagged_domain out = bennet_congr_transform_forward(add3, st);
    EXPECT_EQ(gamma_card_u8(kDomains[0], &out), 64);
  }

  // wint: [8,29] + 3 = [11,32] -> 22 values
  {
    bennet_absint_state* st = bennet_absint_state_create();
    st = bennet_absint_state_set_wint(st, asym(sx), tagged_wint_u8(8, 29));
    bennet_tagged_domain out = bennet_wint_transform_forward(add3, st);
    EXPECT_EQ(gamma_card_u8(kDomains[1], &out), 22);
  }

  // tnum: {0x10, mask 0x0F} & 0x0F -> {0, mask 0x0F} -> 16 values. (A top
  // operand short-circuits tnum's binops to top, so the input must be bound.)
  {
    bennet_absint_state* st = bennet_absint_state_create();
    st = bennet_absint_state_set_tnum(st, asym(sx), tagged_tnum_u8(0x10, 0x0F));
    bennet_tagged_domain out =
        bennet_tnum_transform_forward(cn_smt_bw_and(x, u8_const(0x0F)), st);
    EXPECT_EQ(gamma_card_u8(kDomains[2], &out), 16);
  }

  // congr assume: (x % 4 == 1) from top refines x to 4Z+1 -> 64 values
  {
    bennet_absint_state* st = bennet_absint_state_create();
    cn_term* cond = cn_smt_eq(cn_smt_mod(x, u8_const(4)), u8_const(1));
    bennet_absint_state* refined = bennet_congr_transform_backward_assume(cond, true, st);
    bennet_tagged_domain rx = bennet_absint_state_get_congr(refined, asym(sx), &u8);
    EXPECT_EQ(gamma_card_u8(kDomains[0], &rx), 64);
  }
}
