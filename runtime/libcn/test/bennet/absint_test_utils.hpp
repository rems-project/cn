/**
 * @file absint_test_utils.hpp
 * @brief Shared helpers for abstract-interpretation transformer tests.
 *
 * Everything lives in namespace absint_test: wint.cpp/tnum.cpp/congr.cpp
 * define their own file-local helpers at global scope, so namespacing avoids
 * ODR clashes when multiple test files use these.
 */
#ifndef ABSINT_TEST_UTILS_HPP
#define ABSINT_TEST_UTILS_HPP

#include <cstdint>

#include <bennet/internals/absint.h>
#include <bennet/internals/domains/congr.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/domains/wint.h>
#include <cn-executable/bump_alloc.h>
#include <cn-executable/utils.h>
#include <cn-smt/eval.h>
#include <cn-smt/terms.h>

namespace absint_test {

// Bump-allocate a cn_base_type so tagged domains carry a type pointer that
// outlives the enclosing expression (bennet_tagged_domain_meet/join assert
// type != NULL). Freed by the caller's bump frame guard.
inline cn_base_type* bump_bt(cn_base_type bt) {
  auto* type = (cn_base_type*)cn_bump_malloc(sizeof(cn_base_type));
  *type = bt;
  return type;
}

inline bennet_absint_sym asym(cn_sym s) {
  return {s.name, s.id};
}

/*-----------------------------------------------------------------------------
 * Tagged-domain builders
 *---------------------------------------------------------------------------*/

inline bennet_tagged_domain tagged_congr_u8(uint8_t modulus, uint8_t residue) {
  return bennet_tagged_domain_create(bump_bt(cn_base_type_bits(false, 8)),
      bennet_domain_congr_of_uint8_t(modulus, residue));
}

inline bennet_tagged_domain tagged_congr_u16(uint16_t modulus, uint16_t residue) {
  return bennet_tagged_domain_create(bump_bt(cn_base_type_bits(false, 16)),
      bennet_domain_congr_of_uint16_t(modulus, residue));
}

inline bennet_tagged_domain tagged_congr_u64(uint64_t modulus, uint64_t residue) {
  return bennet_tagged_domain_create(bump_bt(cn_base_type_bits(false, 64)),
      bennet_domain_congr_of_uint64_t(modulus, residue));
}

// Pointer-typed (CN_BASE_LOC) congruence domain; congr lowers LOC to u64.
inline bennet_tagged_domain tagged_congr_loc(uint64_t modulus, uint64_t residue) {
  return bennet_tagged_domain_create(bump_bt(cn_base_type_simple(CN_BASE_LOC)),
      bennet_domain_congr_of_uint64_t(modulus, residue));
}

inline bennet_tagged_domain tagged_congr_u8_top() {
  return bennet_tagged_domain_create(
      bump_bt(cn_base_type_bits(false, 8)), bennet_domain_congr_top_uint8_t());
}

inline bennet_tagged_domain tagged_wint_u8(uint8_t start, uint8_t end) {
  return bennet_tagged_domain_create(
      bump_bt(cn_base_type_bits(false, 8)), bennet_domain_wint_of_uint8_t(start, end));
}

inline bennet_tagged_domain tagged_tnum_u8(uint8_t value, uint8_t mask) {
  return bennet_tagged_domain_create(
      bump_bt(cn_base_type_bits(false, 8)), bennet_domain_tnum_of_uint8_t(value, mask));
}

// Pointer-typed (CN_BASE_LOC) ownership domain {before, after}.
inline bennet_tagged_domain tagged_own(size_t before, size_t after) {
  return bennet_tagged_domain_create(bump_bt(cn_base_type_simple(CN_BASE_LOC)),
      bennet_domain_ownership_of(uintptr_t, before, after));
}

inline bennet_tagged_domain tagged_own_top() {
  return bennet_tagged_domain_create(
      bump_bt(cn_base_type_simple(CN_BASE_LOC)), bennet_domain_ownership_top(uintptr_t));
}

inline bennet_tagged_domain tagged_own_bottom() {
  return bennet_tagged_domain_create(bump_bt(cn_base_type_simple(CN_BASE_LOC)),
      bennet_domain_ownership_bottom(uintptr_t));
}

/*-----------------------------------------------------------------------------
 * Term builders
 *---------------------------------------------------------------------------*/

inline cn_term* u8_const(uint8_t v) {
  return cn_smt_bits(false, 8, v);
}

inline cn_term* u8_sym(cn_sym s) {
  return cn_smt_sym(s, cn_base_type_bits(false, 8));
}

inline cn_term* loc_sym(cn_sym s) {
  return cn_smt_sym(s, cn_base_type_simple(CN_BASE_LOC));
}

// There is no cn_smt_negate builder; hand-construct the CN_UNOP_NEGATE node.
inline cn_term* negate_term(cn_term* operand) {
  cn_term* t = cn_term_alloc(CN_TERM_UNOP, operand->base_type);
  t->data.unop.op = CN_UNOP_NEGATE;
  t->data.unop.operand = operand;
  return t;
}

/*-----------------------------------------------------------------------------
 * Concrete-eval readers (terms must be closed; cn_eval_term asserts on
 * unbound symbols)
 *---------------------------------------------------------------------------*/

inline uint8_t eval_u8(cn_term* t) {
  void* r = cn_eval_term(t);
  assert(r);
  return ((cn_bits_u8*)r)->val;
}

inline uint16_t eval_u16(cn_term* t) {
  void* r = cn_eval_term(t);
  assert(r);
  return ((cn_bits_u16*)r)->val;
}

inline int8_t eval_i8(cn_term* t) {
  void* r = cn_eval_term(t);
  assert(r);
  return ((cn_bits_i8*)r)->val;
}

inline bool eval_bool(cn_term* t) {
  void* r = cn_eval_term(t);
  assert(r);
  return convert_from_cn_bool((cn_bool*)r);
}

/*-----------------------------------------------------------------------------
 * Term/state fuzzing vocabulary (kept for future differential harnesses,
 * e.g. the P6 semantic-upgrade gates; the P3 old-vs-new harness that used
 * it was deleted with the legacy walker snapshots once all ports were
 * gated). See doc/RUNTIME-ABSINT.md P3.
 *---------------------------------------------------------------------------*/

namespace fuzz {

/*-----------------------------------------------------------------------------
 * Deterministic local PRNG (xorshift64*)
 *---------------------------------------------------------------------------*/

struct Rng {
  uint64_t s;

  explicit Rng(uint64_t seed)
      : s(seed * 6364136223846793005ull + 1442695040888963407ull) {}

  uint64_t next() {
    s ^= s >> 12;
    s ^= s << 25;
    s ^= s >> 27;
    return s * 2685821657736338717ull;
  }

  uint64_t below(uint64_t n) {
    return n ? next() % n : 0;
  }

  bool chance(unsigned pct) {
    return below(100) < pct;
  }
};

/*-----------------------------------------------------------------------------
 * Fuzzer vocabulary
 *---------------------------------------------------------------------------*/

const cn_base_type kWidths[] = {
    cn_base_type_bits(false, 8),
    cn_base_type_bits(true, 8),
    cn_base_type_bits(false, 16),
    cn_base_type_bits(true, 16),
};

const cn_sym kArithSyms[] = {{"dfx", 101}, {"dfy", 102}, {"dfz", 103}};
const cn_sym kPtrSyms[] = {{"dfp", 201}, {"dfq", 202}};
constexpr int kNumArithSyms = 3;
constexpr int kNumPtrSyms = 2;

/* Per-seed type assignment for the arithmetic syms. Each symbol must occur
 * at a single width within a seed: production terms are type-consistent per
 * symbol, and wint's generic meet/join assert equal widths, so a symbol
 * bound at one width but occurring at another would crash both walkers
 * (stored-type dataflow). Set at the top of every driver iteration. */
inline cn_base_type g_sym_bts[kNumArithSyms];

inline void assign_sym_types(Rng& rng) {
  for (int i = 0; i < kNumArithSyms; i++) {
    g_sym_bts[i] = kWidths[rng.below(4)];
  }
}

inline bool same_bits_type(const cn_base_type& a, const cn_base_type& b) {
  return a.tag == b.tag && a.data.bits.is_signed == b.data.bits.is_signed &&
         a.data.bits.size_bits == b.data.bits.size_bits;
}

inline uint64_t width_max(const cn_base_type& bt) {
  return bt.data.bits.size_bits == 8 ? 0xffu : 0xffffu;
}

inline cn_term* gen_const(Rng& rng, const cn_base_type& bt) {
  uint64_t m = width_max(bt);
  uint64_t v;
  switch (rng.below(7)) {
    case 0:
      v = 0;
      break;
    case 1:
      v = 1;
      break;
    case 2:
      v = 2;
      break;
    case 3:
      v = m;
      break;
    case 4:
      v = m - 1;
      break;
    case 5:
      v = 1ull << rng.below(bt.data.bits.size_bits);
      break;
    default:
      v = rng.next() & m;
      break;
  }
  return cn_smt_bits(bt.data.bits.is_signed, bt.data.bits.size_bits, (intmax_t)v);
}

inline cn_term* gen_arith_sym(Rng& rng, const cn_base_type& bt) {
  int candidates[kNumArithSyms];
  int n = 0;
  for (int i = 0; i < kNumArithSyms; i++) {
    if (same_bits_type(g_sym_bts[i], bt)) {
      candidates[n++] = i;
    }
  }
  if (n == 0) {
    return gen_const(rng, bt);
  }
  return cn_smt_sym(kArithSyms[candidates[rng.below((uint64_t)n)]], bt);
}

inline cn_term* binop_bool(cn_binop op, cn_term* l, cn_term* r) {
  cn_term* t = cn_term_alloc(CN_TERM_BINOP, cn_base_type_simple(CN_BASE_BOOL));
  t->data.binop.op = op;
  t->data.binop.left = l;
  t->data.binop.right = r;
  return t;
}

inline cn_term* gen_arith(Rng& rng, const cn_base_type& bt, int depth);

/* When set, array-shift indices in pointer terms are constants. The P3
 * difftest drivers needed this to dodge the ARRAY_SHIFT backward
 * index-fallback width crash; that fallback STOPs since P6.0
 * (WIntBackwardArrayShiftIndexFallbackStops), so new drivers can generate
 * symbolic indices freely. Kept for vocabulary completeness. */
inline bool g_ptr_const_index = false;

inline cn_term* gen_ptr(Rng& rng, int depth) {
  cn_term* t =
      cn_smt_sym(kPtrSyms[rng.below(kNumPtrSyms)], cn_base_type_simple(CN_BASE_LOC));
  static const size_t kSizes[] = {1, 2, 4, 8};
  for (int i = 0; i < depth; i++) {
    if (rng.chance(50)) {
      break;
    }
    if (rng.chance(50)) {
      t = cn_smt_member_shift(t, kSizes[rng.below(4)]);
    } else {
      const cn_base_type& idx_bt = kWidths[rng.below(4)];
      cn_term* idx =
          g_ptr_const_index ? gen_const(rng, idx_bt) : gen_arith(rng, idx_bt, depth - 1);
      t = cn_smt_array_shift(t, kSizes[rng.below(4)], idx);
    }
  }
  return t;
}

inline cn_term* gen_cmp(Rng& rng, int depth) {
  if (rng.chance(25)) {
    /* Pointer comparison; cn_smt_lt/le always emit the integer ops, so the
     * pointer variants are hand-built. */
    cn_term* l = gen_ptr(rng, depth);
    cn_term* r = gen_ptr(rng, depth);
    switch (rng.below(3)) {
      case 0:
        return cn_smt_eq(l, r);
      case 1:
        return binop_bool(CN_BINOP_LT_POINTER, l, r);
      default:
        return binop_bool(CN_BINOP_LE_POINTER, l, r);
    }
  }
  const cn_base_type& bt = kWidths[rng.below(4)];
  cn_term* l = gen_arith(rng, bt, depth);
  cn_term* r = gen_arith(rng, bt, depth);
  switch (rng.below(3)) {
    case 0:
      return cn_smt_eq(l, r);
    case 1:
      return cn_smt_lt(l, r);
    default:
      return cn_smt_le(l, r);
  }
}

inline cn_term* gen_cond(Rng& rng, int depth) {
  cn_term* c = gen_cmp(rng, depth);
  if (rng.chance(20)) {
    c = cn_smt_not(c);
  }
  if (rng.chance(25)) {
    cn_term* c2 = gen_cmp(rng, depth);
    c = rng.chance(50) ? cn_smt_and(c, c2) : cn_smt_or(c, c2);
  }
  return c;
}

inline cn_term* gen_arith(Rng& rng, const cn_base_type& bt, int depth) {
  if (depth <= 0 || rng.chance(25)) {
    return rng.chance(55) ? gen_arith_sym(rng, bt) : gen_const(rng, bt);
  }

  static const cn_binop kBinops[] = {
      CN_BINOP_ADD,
      CN_BINOP_SUB,
      CN_BINOP_MUL,
      CN_BINOP_DIV,
      CN_BINOP_MOD,
      CN_BINOP_REM,
      CN_BINOP_SHIFT_LEFT,
      CN_BINOP_SHIFT_RIGHT,
      CN_BINOP_BW_AND,
      CN_BINOP_BW_OR,
      CN_BINOP_BW_XOR,
      CN_BINOP_MIN,
      CN_BINOP_MAX,
  };

  switch (rng.below(10)) {
    case 0:
    case 1: { /* unop */
      cn_term* v = gen_arith(rng, bt, depth - 1);
      return rng.chance(60) ? negate_term(v) : cn_smt_bw_compl(v);
    }
    case 2: { /* ite */
      cn_term* c = gen_cmp(rng, depth - 1);
      cn_term* t = gen_arith(rng, bt, depth - 1);
      cn_term* e = gen_arith(rng, bt, depth - 1);
      return cn_smt_ite(c, t, e);
    }
    case 3: { /* cast */
      const cn_base_type& inner_bt = kWidths[rng.below(4)];
      return cn_smt_cast(bt, gen_arith(rng, inner_bt, depth - 1));
    }
    default: { /* binop */
      cn_binop op = kBinops[rng.below(sizeof(kBinops) / sizeof(kBinops[0]))];
      cn_term* l = gen_arith(rng, bt, depth - 1);
      cn_term* r = gen_arith(rng, bt, depth - 1);
      cn_term* t = cn_term_alloc(CN_TERM_BINOP, bt);
      t->data.binop.op = op;
      t->data.binop.left = l;
      t->data.binop.right = r;
      return t;
    }
  }
}

}  // namespace fuzz

}  // namespace absint_test

#endif  // ABSINT_TEST_UTILS_HPP
