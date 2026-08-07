/**
 * @file absint_fuzz.cpp
 * @brief Randomized soundness fuzzer for the absint walker engine.
 *
 * The exhaustive oracle (absint_oracle.cpp) sweeps a fixed term corpus; this
 * fuzzer sweeps randomly generated terms from the retained vocabulary
 * (absint_test_utils.hpp, namespace fuzz) so engine changes are gated on
 * shapes nobody hand-picked. It is the successor gate to the deleted P3
 * old-vs-new differential harness: with the legacy walkers gone there is no
 * reference implementation left, so the property checked is soundness
 * against the concrete evaluator, per domain:
 *
 *  - assume: every concrete environment satisfying a condition (at either
 *    polarity) survives bennet_<dom>_transform_backward_assume refinement,
 *    and a bottom refined state implies the condition is unsatisfiable;
 *  - targeted backward: every environment whose term value lies in the
 *    output domain survives per-target refinement through the public
 *    bennet_<dom>_transform_backward entry (the output is a singleton of a
 *    reference environment's value, so the sweep is never vacuous).
 *
 * Concrete truth is cn_eval_term on a clone of the term with symbols
 * substituted by constants. Draws are rejected (and redrawn) when a term
 * has pointer symbols (no concrete pointer environments), DIV/MOD/REM or
 * shift nodes (concrete UB on zero divisors / oversized shifts, per the
 * oracle's corpus rules), more than two distinct symbols (the exhaustive
 * env sweep is 8-bit, 256^nsyms), or a non-8-bit symbol.
 */
#include "absint_test_utils.hpp"
#include <gtest/gtest.h>

#include <cstdint>
#include <vector>

using absint_test::asym;
using absint_test::bump_bt;
using absint_test::eval_bool;
namespace fuzz = absint_test::fuzz;

namespace {

class AbsintFuzz : public ::testing::Test {
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

constexpr int kNumSeeds = 32;
constexpr int kGenDepth = 3;
constexpr int kMaxDraws = 64;
constexpr int kMaxSyms = 2;

/*-----------------------------------------------------------------------------
 * Domain policy table (test-side dispatch; the no-vtable rule is for the
 * runtime library).
 *---------------------------------------------------------------------------*/

struct DomainFuzz {
  const char* name;
  bennet_absint_state* (*assume)(cn_term*, bool, bennet_absint_state*);
  bennet_absint_state* (*backward)(
      cn_term*, bennet_absint_sym, bennet_tagged_domain, bennet_absint_state*);
  bennet_tagged_domain (*get)(bennet_absint_state*, bennet_absint_sym, cn_base_type*);
  bool (*state_is_bottom)(bennet_absint_state*);
  bool (*check)(uint64_t, bennet_tagged_domain*);
  /* Singleton {v} at an 8-bit type (the fuzzers only produce 8-bit syms and
   * 8-bit backward roots). */
  bennet_tagged_domain (*singleton)(const cn_base_type& bt, uint64_t v);
};

/* Width-dispatching membership check (same shape as the oracle's): a state
 * binding can carry a stored type wider than the symbol itself (e.g. a
 * 16-bit refinement met into a u8 sym's binding through a cast), so the
 * check reinterprets the value per the tagged type. Callers pass signed
 * symbol values sign-extended into the uint64_t pattern. */
#define FUZZ_CHECK_IMPL(dom)                                                             \
  bool dom##_fuzz_check(uint64_t v, bennet_tagged_domain* d) {                           \
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
        ADD_FAILURE() << "unsupported width in " #dom "_fuzz_check";                     \
        return true;                                                                     \
    }                                                                                    \
  }

FUZZ_CHECK_IMPL(congr)
FUZZ_CHECK_IMPL(wint)
FUZZ_CHECK_IMPL(tnum)

#undef FUZZ_CHECK_IMPL

bennet_tagged_domain congr_fuzz_singleton(const cn_base_type& bt, uint64_t v) {
  void* d = bt.data.bits.is_signed ? (void*)bennet_domain_congr_of_int8_t(0, (int8_t)v)
                                   : (void*)bennet_domain_congr_of_uint8_t(0, (uint8_t)v);
  return bennet_tagged_domain_create(bump_bt(bt), d);
}

bennet_tagged_domain wint_fuzz_singleton(const cn_base_type& bt, uint64_t v) {
  void* d = bt.data.bits.is_signed
                ? (void*)bennet_domain_wint_of_int8_t((int8_t)v, (int8_t)v)
                : (void*)bennet_domain_wint_of_uint8_t((uint8_t)v, (uint8_t)v);
  return bennet_tagged_domain_create(bump_bt(bt), d);
}

bennet_tagged_domain tnum_fuzz_singleton(const cn_base_type& bt, uint64_t v) {
  void* d = bt.data.bits.is_signed ? (void*)bennet_domain_tnum_of_int8_t((int8_t)v, 0)
                                   : (void*)bennet_domain_tnum_of_uint8_t((uint8_t)v, 0);
  return bennet_tagged_domain_create(bump_bt(bt), d);
}

const DomainFuzz kDomains[] = {
    {"congr",
        bennet_congr_transform_backward_assume,
        bennet_congr_transform_backward,
        bennet_absint_state_get_congr,
        bennet_absint_state_is_bottom_congr,
        congr_fuzz_check,
        congr_fuzz_singleton},
    {"wint",
        bennet_wint_transform_backward_assume,
        bennet_wint_transform_backward,
        bennet_absint_state_get_wint,
        bennet_absint_state_is_bottom_wint,
        wint_fuzz_check,
        wint_fuzz_singleton},
    {"tnum",
        bennet_tnum_transform_backward_assume,
        bennet_tnum_transform_backward,
        bennet_absint_state_get_tnum,
        bennet_absint_state_is_bottom_tnum,
        tnum_fuzz_check,
        tnum_fuzz_singleton},
};
constexpr int kNumDomains = 3;

/*-----------------------------------------------------------------------------
 * Term admission: collect the distinct symbols and reject shapes the concrete
 * evaluator cannot sweep.
 *---------------------------------------------------------------------------*/

struct SymInfo {
  cn_sym sym;
  cn_base_type bt;
};

/* Symbol value as a check() pattern: sign-extend signed 8-bit values so a
 * wider stored type reinterprets them correctly. */
uint64_t sym_pattern(const SymInfo& si, uint8_t raw) {
  return si.bt.data.bits.is_signed ? (uint64_t)(int64_t)(int8_t)raw : (uint64_t)raw;
}

bool forbidden_binop(cn_binop op) {
  switch (op) {
    case CN_BINOP_DIV:
    case CN_BINOP_DIVNOSMT:
    case CN_BINOP_MOD:
    case CN_BINOP_MODNOSMT:
    case CN_BINOP_REM:
    case CN_BINOP_REMNOSMT:
    case CN_BINOP_SHIFT_LEFT:
    case CN_BINOP_SHIFT_RIGHT:
      return true;
    default:
      return false;
  }
}

bool scan_term(cn_term* t, SymInfo* syms, int* nsyms) {
  switch (t->type) {
    case CN_TERM_CONST:
      return true;
    case CN_TERM_SYM: {
      if (t->base_type.tag != CN_BASE_BITS || t->base_type.data.bits.size_bits != 8) {
        return false; /* pointer sym or non-8-bit sym */
      }
      for (int i = 0; i < *nsyms; i++) {
        if (syms[i].sym.id == t->data.sym.id) {
          return true;
        }
      }
      if (*nsyms == kMaxSyms) {
        return false;
      }
      syms[*nsyms] = {t->data.sym, t->base_type};
      (*nsyms)++;
      return true;
    }
    case CN_TERM_UNOP:
      return scan_term(t->data.unop.operand, syms, nsyms);
    case CN_TERM_BINOP:
      if (forbidden_binop(t->data.binop.op)) {
        return false;
      }
      return scan_term(t->data.binop.left, syms, nsyms) &&
             scan_term(t->data.binop.right, syms, nsyms);
    case CN_TERM_ITE:
      return scan_term(t->data.ite.cond, syms, nsyms) &&
             scan_term(t->data.ite.then_term, syms, nsyms) &&
             scan_term(t->data.ite.else_term, syms, nsyms);
    case CN_TERM_CAST:
      return scan_term(t->data.cast.value, syms, nsyms);
    default:
      return false; /* member/array shifts etc.: pointer-only shapes */
  }
}

/*-----------------------------------------------------------------------------
 * Concrete truth: clone the term with symbols substituted by constants, then
 * cn_eval_term. Only the node kinds scan_term admits appear here.
 *---------------------------------------------------------------------------*/

cn_term* subst_syms(cn_term* t, const SymInfo* syms, const uint8_t* vals, int nsyms) {
  switch (t->type) {
    case CN_TERM_CONST:
      return t;
    case CN_TERM_SYM: {
      uint8_t v = 0;
      for (int i = 0; i < nsyms; i++) {
        if (syms[i].sym.id == t->data.sym.id) {
          v = vals[i];
          break;
        }
      }
      bool is_signed = t->base_type.data.bits.is_signed;
      return cn_smt_bits(is_signed, 8, is_signed ? (intmax_t)(int8_t)v : (intmax_t)v);
    }
    case CN_TERM_UNOP: {
      cn_term* r = cn_term_alloc(CN_TERM_UNOP, t->base_type);
      r->data.unop.op = t->data.unop.op;
      r->data.unop.operand = subst_syms(t->data.unop.operand, syms, vals, nsyms);
      return r;
    }
    case CN_TERM_BINOP: {
      cn_term* r = cn_term_alloc(CN_TERM_BINOP, t->base_type);
      r->data.binop.op = t->data.binop.op;
      r->data.binop.left = subst_syms(t->data.binop.left, syms, vals, nsyms);
      r->data.binop.right = subst_syms(t->data.binop.right, syms, vals, nsyms);
      return r;
    }
    case CN_TERM_ITE: {
      cn_term* r = cn_term_alloc(CN_TERM_ITE, t->base_type);
      r->data.ite.cond = subst_syms(t->data.ite.cond, syms, vals, nsyms);
      r->data.ite.then_term = subst_syms(t->data.ite.then_term, syms, vals, nsyms);
      r->data.ite.else_term = subst_syms(t->data.ite.else_term, syms, vals, nsyms);
      return r;
    }
    case CN_TERM_CAST: {
      cn_term* r = cn_term_alloc(CN_TERM_CAST, t->base_type);
      r->data.cast.target_type = t->data.cast.target_type;
      r->data.cast.value = subst_syms(t->data.cast.value, syms, vals, nsyms);
      return r;
    }
    default:
      ADD_FAILURE() << "subst_syms: unexpected node kind " << (int)t->type;
      return t;
  }
}

uint64_t eval_term_u8_pattern(cn_term* t) {
  return t->base_type.data.bits.is_signed ? (uint8_t)absint_test::eval_i8(t)
                                          : absint_test::eval_u8(t);
}

/* Generate an admissible condition (or arithmetic term via gen) within
 * kMaxDraws attempts; nullptr if the vocabulary kept drawing rejected
 * shapes. */
template <typename Gen>
cn_term* draw_admissible(fuzz::Rng& rng, SymInfo* syms, int* nsyms, Gen gen) {
  for (int tries = 0; tries < kMaxDraws; tries++) {
    cn_term* t = gen(rng);
    *nsyms = 0;
    if (scan_term(t, syms, nsyms)) {
      return t;
    }
  }
  return nullptr;
}

long env_count(int nsyms) {
  long n = 1;
  for (int i = 0; i < nsyms; i++) {
    n *= 256;
  }
  return n;
}

void env_vals(long e, int nsyms, uint8_t* vals) {
  for (int i = 0; i < nsyms; i++) {
    vals[i] = (uint8_t)(e >> (8 * i));
  }
}

/* Per-seed symbol typing: all three vocabulary syms at 8 bits (signedness
 * randomized) so any admitted term sweeps exhaustively at 8 bits. */
void assign_8bit_sym_types(fuzz::Rng& rng) {
  for (int i = 0; i < fuzz::kNumArithSyms; i++) {
    fuzz::g_sym_bts[i] = cn_base_type_bits(rng.chance(35), 8);
  }
}

/*-----------------------------------------------------------------------------
 * Drivers
 *---------------------------------------------------------------------------*/

TEST_F(AbsintFuzz, AssumeSoundness) {
  for (int seed = 0; seed < kNumSeeds; seed++) {
    SCOPED_TRACE(::testing::Message() << "seed=" << seed);
    cn_bump_frame_id seed_frame = cn_bump_get_frame_id();

    fuzz::Rng rng((uint64_t)seed + 1);
    assign_8bit_sym_types(rng);

    SymInfo syms[kMaxSyms];
    int nsyms = 0;
    cn_term* cond = draw_admissible(
        rng, syms, &nsyms, [](fuzz::Rng& r) { return fuzz::gen_cond(r, kGenDepth); });
    if (cond == nullptr) {
      cn_bump_free_after(seed_frame);
      continue;
    }
    /* Evaluate the condition once per environment. */
    long envs = env_count(nsyms);
    std::vector<uint8_t> truth((size_t)envs);
    for (long e = 0; e < envs; e++) {
      uint8_t vals[kMaxSyms] = {0};
      env_vals(e, nsyms, vals);
      cn_bump_frame_id f = cn_bump_get_frame_id();
      truth[(size_t)e] = eval_bool(subst_syms(cond, syms, vals, nsyms)) ? 1 : 0;
      cn_bump_free_after(f);
    }

    for (bool polarity : {true, false}) {
      for (int d = 0; d < kNumDomains; d++) {
        SCOPED_TRACE(
            ::testing::Message() << kDomains[d].name << " polarity=" << polarity);
        bennet_absint_state* refined =
            kDomains[d].assume(cond, polarity, bennet_absint_state_create());

        long sat = 0, fails = 0;
        for (long e = 0; e < envs; e++) {
          if ((truth[(size_t)e] != 0) != polarity) {
            continue;
          }
          sat++;
          uint8_t vals[kMaxSyms] = {0};
          env_vals(e, nsyms, vals);
          for (int i = 0; i < nsyms; i++) {
            bennet_tagged_domain ri =
                kDomains[d].get(refined, asym(syms[i].sym), &syms[i].bt);
            if (!kDomains[d].check(sym_pattern(syms[i], vals[i]), &ri)) {
              fails++;
            }
          }
        }
        EXPECT_EQ(fails, 0);
        if (kDomains[d].state_is_bottom(refined)) {
          EXPECT_EQ(sat, 0);
        }
      }
    }

    cn_bump_free_after(seed_frame);
  }
}

TEST_F(AbsintFuzz, TargetedBackwardSoundness) {
  for (int seed = 0; seed < kNumSeeds; seed++) {
    SCOPED_TRACE(::testing::Message() << "seed=" << seed);
    cn_bump_frame_id seed_frame = cn_bump_get_frame_id();

    fuzz::Rng rng(0x9E3779B9u ^ ((uint64_t)seed + 1));
    assign_8bit_sym_types(rng);

    cn_base_type root_bt = cn_base_type_bits(rng.chance(35), 8);
    SymInfo syms[kMaxSyms];
    int nsyms = 0;
    cn_term* term = draw_admissible(rng, syms, &nsyms, [&root_bt](fuzz::Rng& r) {
      return fuzz::gen_arith(r, root_bt, kGenDepth);
    });
    if (term == nullptr || nsyms == 0) {
      cn_bump_free_after(seed_frame);
      continue;
    }

    /* Evaluate the term once per environment. */
    long envs = env_count(nsyms);
    std::vector<uint8_t> value((size_t)envs);
    for (long e = 0; e < envs; e++) {
      uint8_t vals[kMaxSyms] = {0};
      env_vals(e, nsyms, vals);
      cn_bump_frame_id f = cn_bump_get_frame_id();
      value[(size_t)e] =
          (uint8_t)eval_term_u8_pattern(subst_syms(term, syms, vals, nsyms));
      cn_bump_free_after(f);
    }

    /* Output domain: singleton of a reference environment's value, so at
     * least that environment is in gamma(out) for every domain. */
    uint8_t ref_vals[kMaxSyms] = {0};
    long ref_env = (long)rng.below((uint64_t)envs);
    env_vals(ref_env, nsyms, ref_vals);
    uint8_t ref_value = value[(size_t)ref_env];

    for (int d = 0; d < kNumDomains; d++) {
      bennet_tagged_domain out = kDomains[d].singleton(root_bt, ref_value);
      for (int i = 0; i < nsyms; i++) {
        SCOPED_TRACE(
            ::testing::Message() << kDomains[d].name << " target=" << syms[i].sym.name);
        bennet_absint_state* refined = kDomains[d].backward(
            term, asym(syms[i].sym), out, bennet_absint_state_create());

        long in_gamma = 0, fails = 0;
        for (long e = 0; e < envs; e++) {
          if (!kDomains[d].check(value[(size_t)e], &out)) {
            continue;
          }
          in_gamma++;
          uint8_t vals[kMaxSyms] = {0};
          env_vals(e, nsyms, vals);
          bennet_tagged_domain ri =
              kDomains[d].get(refined, asym(syms[i].sym), &syms[i].bt);
          if (!kDomains[d].check(sym_pattern(syms[i], vals[i]), &ri)) {
            fails++;
          }
        }
        EXPECT_EQ(fails, 0);
        EXPECT_GE(in_gamma, 1); /* the reference env is always in gamma */
      }
    }

    cn_bump_free_after(seed_frame);
  }
}

}  // namespace
