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

#include <bennet/internals/absint.h>
#include <bennet/internals/domains/congr.h>
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

/*-----------------------------------------------------------------------------
 * Term builders
 *---------------------------------------------------------------------------*/

inline cn_term* u8_const(uint8_t v) {
  return cn_smt_bits(false, 8, v);
}

inline cn_term* u8_sym(cn_sym s) {
  return cn_smt_sym(s, cn_base_type_bits(false, 8));
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

}  // namespace absint_test

#endif  // ABSINT_TEST_UTILS_HPP
