#include <assert.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/internals/absint.h>
#include <bennet/internals/domains/congr.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/terms.h>

/*
 * Congruence domain: aZ + b (all integers congruent to b modulo a)
 *
 * Representation:
 *   modulus = a (stride), residue = b (offset)
 *   modulus == 0 means singleton {residue}
 *   top: modulus == 1 (after xi-normalization)
 *
 * For bitvectors of width w, xi-normalization ensures:
 *   modulus divides 2^w (so modulus is always a power-of-2)
 *   residue is in [0, modulus) when modulus > 0
 *
 * After xi-normalization, modulus is always a power-of-2, enabling
 * efficient operations using bitwise AND instead of modulo.
 */

/* Helper: GCD using Euclidean algorithm for unsigned types */
#define CONGR_GCD_IMPL(sm)                                                               \
  static uint##sm##_t congr_gcd_##sm(uint##sm##_t a, uint##sm##_t b) {                   \
    while (b != 0) {                                                                     \
      uint##sm##_t t = b;                                                                \
      b = a % b;                                                                         \
      a = t;                                                                             \
    }                                                                                    \
    return a;                                                                            \
  }

CONGR_GCD_IMPL(8)
CONGR_GCD_IMPL(16)
CONGR_GCD_IMPL(32)
CONGR_GCD_IMPL(64)

/* Helper: absolute difference for unsigned types */
#define CONGR_ABS_DIFF(sm)                                                               \
  static uint##sm##_t congr_abs_diff_##sm(uint##sm##_t a, uint##sm##_t b) {              \
    return (a >= b) ? (a - b) : (b - a);                                                 \
  }

CONGR_ABS_DIFF(8)
CONGR_ABS_DIFF(16)
CONGR_ABS_DIFF(32)
CONGR_ABS_DIFF(64)

/* Xi-normalization: modulus = gcd(modulus, 2^w), residue = residue mod modulus
 * Since modulus always becomes a power-of-2, we can use bitwise operations.
 * Also handles modulus == full_width (2^w), converting to singleton. */
#define CONGR_XI_NORM(sm, FULL_MASK, TWO_W)                                                \
  static void congr_xi_norm_##sm(uint##sm##_t* mod, uint##sm##_t* res) {                   \
    if (*mod == 0) {                                                                       \
      /* Singleton: just mask residue */                                                   \
      *res = *res & (uint##sm##_t)(FULL_MASK);                                             \
      return;                                                                              \
    }                                                                                      \
    /* gcd(modulus, 2^w): since 2^w is a power-of-2, the result is                       \
     * the largest power-of-2 dividing modulus, i.e. modulus & (-modulus) */ \
    uint##sm##_t m = *mod & (uint##sm##_t)(-(int##sm##_t) * mod);                          \
    /* But we also need gcd(m, 2^w) which for m being power-of-2 is just m               \
     * if m <= 2^w. Since m fits in the type, m < 2^w always holds for                   \
     * unsigned types (2^w overflows to 0). Special check: if m == 0                     \
     * (which happens if original mod was 0, already handled above). */ \
    if (m == 0 || (TWO_W != 0 && m > (uint##sm##_t)(TWO_W))) {                             \
      /* Treat as singleton */                                                             \
      *mod = 0;                                                                            \
      *res = *res & (uint##sm##_t)(FULL_MASK);                                             \
      return;                                                                              \
    }                                                                                      \
    /* m is a power-of-2, so residue mod m == residue & (m - 1) */                         \
    *mod = m;                                                                              \
    *res = *res & (m - 1);                                                                 \
  }

/* For 8/16/32-bit types, TWO_W fits in a larger type but overflows in the same type.
 * We use 0 as a sentinel for TWO_W when it equals 2^w (which overflows to 0). */
CONGR_XI_NORM(8, UINT8_MAX, 0)
CONGR_XI_NORM(16, UINT16_MAX, 0)
CONGR_XI_NORM(32, UINT32_MAX, 0)
CONGR_XI_NORM(64, UINT64_MAX, 0)

/* Arbitrary generation for congruence domains.
 * Picks a random index in [0, min(num_elements - 1, size_budget)]
 * and returns residue + index * modulus (mod 2^w). */
#define CONGR_GEN(sm)                                                                    \
  uint##sm##_t bennet_arbitrary_congr_uint##sm##_t(                                      \
      bennet_domain_congr(uint##sm##_t) * d) {                                           \
    if (d->bottom) {                                                                     \
      assert(false && "Cannot generate value from bottom congr");                        \
      return 0;                                                                          \
    }                                                                                    \
                                                                                         \
    uint##sm##_t fm = UINT##sm##_MAX;                                                    \
                                                                                         \
    /* Singleton */                                                                      \
    if (d->modulus == 0) {                                                               \
      return d->residue & fm;                                                            \
    }                                                                                    \
                                                                                         \
    if (d->top) {                                                                        \
      return bennet_arbitrary_sized_top(uint##sm##_t);                                   \
    }                                                                                    \
                                                                                         \
    /* Number of elements in the congruence class within [0, 2^w) */                     \
    /* = floor((2^w - 1 - residue) / modulus) + 1 */                                     \
    /* Overflows to 0 when the class covers the full type range. */                      \
    uint##sm##_t num_elements = (uint##sm##_t)((fm - d->residue) / d->modulus) + 1;      \
                                                                                         \
    /* max_idx wraps to UINT_MAX when num_elements == 0 (full range) */                  \
    uint##sm##_t max_idx = num_elements - 1;                                             \
                                                                                         \
    /* Use wint's arbitrary to pick the index - it automatically handles */              \
    /* extreme values when the range is the full type. */                                \
    uint##sm##_t idx = bennet_arbitrary_wint_of(uint##sm##_t, 0, max_idx);               \
    return (uint##sm##_t)((d->residue + (uint##sm##_t)(idx * d->modulus)) & fm);         \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_arbitrary_congr_int##sm##_t(bennet_domain_congr(int##sm##_t) * d) { \
    if (d->bottom) {                                                                     \
      assert(false && "Cannot generate value from bottom congr");                        \
      return 0;                                                                          \
    }                                                                                    \
                                                                                         \
    uint##sm##_t m = (uint##sm##_t)d->modulus;                                           \
    uint##sm##_t r = (uint##sm##_t)d->residue;                                           \
    uint##sm##_t fm = UINT##sm##_MAX;                                                    \
                                                                                         \
    /* Singleton */                                                                      \
    if (m == 0) {                                                                        \
      return (int##sm##_t)r;                                                             \
    }                                                                                    \
                                                                                         \
    if (d->top) {                                                                        \
      return bennet_arbitrary_sized_top(uint##sm##_t);                                   \
    }                                                                                    \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
    uint##sm##_t signed_max = (uint##sm##_t)INT##sm##_MAX;                               \
                                                                                         \
    /* Count positive elements (unsigned value <= INT_MAX) and negative elements */      \
    /* After xi-norm: r < m <= 2^(w-1), so r <= INT_MAX */                               \
    size_t num_pos = (size_t)((signed_max - r) / m) + 1;                                 \
    size_t total_elems = (size_t)((fm - r) / m) + 1;                                     \
    size_t num_neg = total_elems - num_pos;                                              \
                                                                                         \
    /* Cap by size budget to center around zero */                                       \
    size_t capped_pos = (num_pos > sz + 1) ? sz + 1 : num_pos;                           \
    size_t capped_neg = (num_neg > sz) ? sz : num_neg;                                   \
    size_t total = capped_pos + capped_neg;                                              \
                                                                                         \
    uint64_t choice = bennet_arbitrary_wint_of(uint64_t, 0, (uint64_t)(total - 1));      \
                                                                                         \
    if (choice < capped_pos) {                                                           \
      /* Positive: residue + choice * modulus (small positive values) */                 \
      return (int##sm##_t)(uint##sm##_t)(r + (uint##sm##_t)choice * m);                  \
    } else {                                                                             \
      /* Negative: count backwards from last unsigned element */                         \
      uint64_t neg_idx = choice - capped_pos;                                            \
      uint##sm##_t last_elem = (uint##sm##_t)(r + (uint##sm##_t)(total_elems - 1) * m);  \
      return (int##sm##_t)(uint##sm##_t)(last_elem - (uint##sm##_t)neg_idx * m);         \
    }                                                                                    \
  }

CONGR_GEN(8)
CONGR_GEN(16)
CONGR_GEN(32)
CONGR_GEN(64)

/* uintptr_t delegates to uint64_t */
uintptr_t bennet_arbitrary_congr_uintptr_t(bennet_domain_congr(uintptr_t) * d) {
  assert(sizeof(uintptr_t) == sizeof(uint64_t));
  return (uintptr_t)bennet_arbitrary_congr_uint64_t((bennet_domain_congr(uint64_t)*)d);
}

/* Helper to call congr_xi_norm with type-safe temporaries.
 * Needed because on some platforms ucty (e.g. uintptr_t = unsigned long)
 * differs from uint##sm##_t (e.g. uint64_t = unsigned long long). */
#define CONGR_XI_NORM_CALL(sm, ucty, m_var, r_var)                                       \
  do {                                                                                   \
    uint##sm##_t _xi_m = (uint##sm##_t)(m_var);                                          \
    uint##sm##_t _xi_r = (uint##sm##_t)(r_var);                                          \
    congr_xi_norm_##sm(&_xi_m, &_xi_r);                                                  \
    (m_var) = (ucty)_xi_m;                                                               \
    (r_var) = (ucty)_xi_r;                                                               \
  } while (0)

/* Complete domain implementation for each type */
#define CONGR_DOMAIN_IMPL(cty, ucty, FULL_MASK, sm)                                      \
  bennet_domain_congr(cty) * bennet_domain_congr_top_##cty(void) {                       \
    bennet_domain_congr(cty)* r = std_malloc(sizeof(bennet_domain_congr(cty)));          \
    assert(r);                                                                           \
    r->top = true;                                                                       \
    r->bottom = false;                                                                   \
    r->modulus = 1;                                                                      \
    r->residue = 0;                                                                      \
    return r;                                                                            \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) * bennet_domain_congr_bottom_##cty(void) {                    \
    bennet_domain_congr(cty)* r = std_malloc(sizeof(bennet_domain_congr(cty)));          \
    assert(r);                                                                           \
    r->top = false;                                                                      \
    r->bottom = true;                                                                    \
    r->modulus = 0;                                                                      \
    r->residue = 0;                                                                      \
    return r;                                                                            \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) * bennet_domain_congr_of_##cty(cty modulus, cty residue) {    \
    bennet_domain_congr(cty)* r = std_malloc(sizeof(bennet_domain_congr(cty)));          \
    assert(r);                                                                           \
    ucty m = (ucty)modulus;                                                              \
    ucty res = (ucty)residue;                                                            \
    CONGR_XI_NORM_CALL(sm, ucty, m, res);                                                \
    r->modulus = (cty)m;                                                                 \
    r->residue = (cty)res;                                                               \
    r->bottom = false;                                                                   \
    r->top = ((ucty)r->modulus == 1);                                                    \
    return r;                                                                            \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_congr_is_top_##cty(bennet_domain_congr(cty) * d) {                  \
    return d->top;                                                                       \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_congr_is_bottom_##cty(bennet_domain_congr(cty) * d) {               \
    return d->bottom;                                                                    \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_congr_equal_##cty(                                                  \
      bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                    \
    if (d1->top && d2->top)                                                              \
      return true;                                                                       \
    if (d1->bottom && d2->bottom)                                                        \
      return true;                                                                       \
    if (d1->top || d1->bottom || d2->top || d2->bottom)                                  \
      return false;                                                                      \
    return (ucty)d1->modulus == (ucty)d2->modulus &&                                     \
           (ucty)d1->residue == (ucty)d2->residue;                                       \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_congr_leq_##cty(                                                    \
      bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                    \
    if (d1->bottom)                                                                      \
      return true;                                                                       \
    if (d2->top)                                                                         \
      return true;                                                                       \
    if (d1->top && !d2->top)                                                             \
      return false;                                                                      \
    if (d2->bottom)                                                                      \
      return false;                                                                      \
    /* aZ+b <= cZ+d iff c|a and b == d (mod c) */                                        \
    ucty a = (ucty)d1->modulus;                                                          \
    ucty b = (ucty)d1->residue;                                                          \
    ucty c = (ucty)d2->modulus;                                                          \
    ucty d = (ucty)d2->residue;                                                          \
    if (c == 0) {                                                                        \
      /* d2 is singleton: d1 must also be same singleton */                              \
      return a == 0 && b == d;                                                           \
    }                                                                                    \
    /* c | a: since both are power-of-2 after xi-norm, c|a iff a % c == 0 */             \
    if (a % c != 0)                                                                      \
      return false;                                                                      \
    /* b == d (mod c): since c is power-of-2, use bitwise */                             \
    return (b & (c - 1)) == (d & (c - 1));                                               \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_join_##cty(                                                    \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    bennet_domain_congr(cty)* r = std_malloc(sizeof(bennet_domain_congr(cty)));          \
    assert(r);                                                                           \
                                                                                         \
    if (d1->top || d2->top) {                                                            \
      r->top = true;                                                                     \
      r->bottom = false;                                                                 \
      r->modulus = 1;                                                                    \
      r->residue = 0;                                                                    \
      return r;                                                                          \
    }                                                                                    \
    if (d1->bottom) {                                                                    \
      *r = *d2;                                                                          \
      return r;                                                                          \
    }                                                                                    \
    if (d2->bottom) {                                                                    \
      *r = *d1;                                                                          \
      return r;                                                                          \
    }                                                                                    \
                                                                                         \
    /* gcd(a, c, |b - d|) */                                                             \
    ucty a = (ucty)d1->modulus;                                                          \
    ucty b = (ucty)d1->residue;                                                          \
    ucty c = (ucty)d2->modulus;                                                          \
    ucty dd = (ucty)d2->residue;                                                         \
    ucty diff = congr_abs_diff_##sm(b, dd);                                              \
    ucty g = congr_gcd_##sm(congr_gcd_##sm(a, c), diff);                                 \
    /* xi-normalize */                                                                   \
    ucty res = (g == 0) ? b : (b & (g - 1));                                             \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    r->modulus = (cty)g;                                                                 \
    r->residue = (cty)res;                                                               \
    r->bottom = false;                                                                   \
    r->top = ((ucty)r->modulus == 1);                                                    \
    return r;                                                                            \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_meet_##cty(                                                    \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    bennet_domain_congr(cty)* r = std_malloc(sizeof(bennet_domain_congr(cty)));          \
    assert(r);                                                                           \
                                                                                         \
    if (d1->bottom || d2->bottom) {                                                      \
      r->top = false;                                                                    \
      r->bottom = true;                                                                  \
      r->modulus = 0;                                                                    \
      r->residue = 0;                                                                    \
      return r;                                                                          \
    }                                                                                    \
    if (d1->top) {                                                                       \
      *r = *d2;                                                                          \
      return r;                                                                          \
    }                                                                                    \
    if (d2->top) {                                                                       \
      *r = *d1;                                                                          \
      return r;                                                                          \
    }                                                                                    \
                                                                                         \
    /* Meet via CRT: lcm(a, c) if compatible residues */                                 \
    ucty a = (ucty)d1->modulus;                                                          \
    ucty b = (ucty)d1->residue;                                                          \
    ucty c = (ucty)d2->modulus;                                                          \
    ucty dd = (ucty)d2->residue;                                                         \
                                                                                         \
    /* Both singletons */                                                                \
    if (a == 0 && c == 0) {                                                              \
      if (b == dd) {                                                                     \
        *r = *d1;                                                                        \
      } else {                                                                           \
        r->top = false;                                                                  \
        r->bottom = true;                                                                \
        r->modulus = 0;                                                                  \
        r->residue = 0;                                                                  \
      }                                                                                  \
      return r;                                                                          \
    }                                                                                    \
                                                                                         \
    /* One singleton */                                                                  \
    if (a == 0) {                                                                        \
      /* Check if b is in cZ+dd */                                                       \
      if ((b & (c - 1)) == dd) {                                                         \
        *r = *d1; /* singleton b */                                                      \
      } else {                                                                           \
        r->top = false;                                                                  \
        r->bottom = true;                                                                \
        r->modulus = 0;                                                                  \
        r->residue = 0;                                                                  \
      }                                                                                  \
      return r;                                                                          \
    }                                                                                    \
    if (c == 0) {                                                                        \
      /* Check if dd is in aZ+b */                                                       \
      if ((dd & (a - 1)) == b) {                                                         \
        *r = *d2; /* singleton dd */                                                     \
      } else {                                                                           \
        r->top = false;                                                                  \
        r->bottom = true;                                                                \
        r->modulus = 0;                                                                  \
        r->residue = 0;                                                                  \
      }                                                                                  \
      return r;                                                                          \
    }                                                                                    \
                                                                                         \
    /* General case: both power-of-2 moduli */                                           \
    /* For power-of-2 moduli: gcd = min(a,c), lcm = max(a,c) */                          \
    ucty g = (a < c) ? a : c;                                                            \
    ucty l = (a > c) ? a : c;                                                            \
    /* Check residue compatibility: b == dd (mod gcd(a,c)) */                            \
    if ((b & (g - 1)) != (dd & (g - 1))) {                                               \
      r->top = false;                                                                    \
      r->bottom = true;                                                                  \
      r->modulus = 0;                                                                    \
      r->residue = 0;                                                                    \
      return r;                                                                          \
    }                                                                                    \
    /* Result: lcm(a,c)Z + (b with bits from dd for the finer modulus) */                \
    /* Since both are pow2, the finer residue is just the one with larger modulus */     \
    ucty res = (a >= c) ? b : dd;                                                        \
    CONGR_XI_NORM_CALL(sm, ucty, l, res);                                                \
    r->modulus = (cty)l;                                                                 \
    r->residue = (cty)res;                                                               \
    r->bottom = false;                                                                   \
    r->top = ((ucty)r->modulus == 1);                                                    \
    return r;                                                                            \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_copy_##cty(bennet_domain_congr(cty) * d) {                     \
    bennet_domain_congr(cty)* r = std_malloc(sizeof(bennet_domain_congr(cty)));          \
    assert(r);                                                                           \
    *r = *d;                                                                             \
    return r;                                                                            \
  }                                                                                      \
                                                                                         \
  cty bennet_domain_congr_arbitrary_##cty(bennet_domain_congr(cty) * d) {                \
    return bennet_arbitrary_congr_##cty(d);                                              \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_congr_check_##cty(cty v, bennet_domain_congr(cty) * d) {            \
    if (d->bottom)                                                                       \
      return false;                                                                      \
    if (d->top)                                                                          \
      return true;                                                                       \
    ucty uv = (ucty)v;                                                                   \
    ucty m = (ucty)d->modulus;                                                           \
    ucty res = (ucty)d->residue;                                                         \
    if (m == 0)                                                                          \
      return uv == res;                                                                  \
    /* modulus is power-of-2: (v - residue) & (modulus - 1) == 0 */                      \
    return ((uv - res) & (m - 1)) == 0;                                                  \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) * bennet_domain_congr_from_assignment_##cty(                  \
                                 void* base_ptr, void* addr, size_t bytes) {             \
    if (sizeof(cty) == sizeof(uintptr_t) && bytes > 0) {                                 \
      uintptr_t min_ptr = (uintptr_t)bennet_rand_alloc_min_ptr();                        \
      uintptr_t max_ptr = (uintptr_t)bennet_rand_alloc_max_ptr();                        \
      uintptr_t offset = (uintptr_t)addr - (uintptr_t)base_ptr;                          \
      if (offset > min_ptr) {                                                            \
        return bennet_domain_congr_top_##cty();                                          \
      }                                                                                  \
      uintptr_t lo = min_ptr - offset;                                                   \
      if (offset + bytes - 1 > max_ptr) {                                                \
        return bennet_domain_congr_top_##cty();                                          \
      }                                                                                  \
      uintptr_t hi = max_ptr - offset - bytes + 1;                                       \
      if (hi < lo) {                                                                     \
        return bennet_domain_congr_top_##cty();                                          \
      }                                                                                  \
      return bennet_domain_congr_of_interval_##cty((cty)lo, (cty)hi);                    \
    }                                                                                    \
    return bennet_domain_congr_top_##cty();                                              \
  }                                                                                      \
                                                                                         \
  /* Addition: gcd(a, c, 2^w)Z + ((b+d) mod 2^w) */                                      \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_add_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty a = (ucty)d1->modulus, b = (ucty)d1->residue;                                   \
    ucty c = (ucty)d2->modulus, dd = (ucty)d2->residue;                                  \
    ucty g = congr_gcd_##sm(a, c);                                                       \
    /* gcd with 2^w is handled by xi_norm */                                             \
    ucty res = (ucty)((b + dd) & (ucty)(FULL_MASK));                                     \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Subtraction: gcd(a, c, 2^w)Z + ((b-d) mod 2^w) */                                   \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_sub_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty a = (ucty)d1->modulus, b = (ucty)d1->residue;                                   \
    ucty c = (ucty)d2->modulus, dd = (ucty)d2->residue;                                  \
    ucty g = congr_gcd_##sm(a, c);                                                       \
    ucty res = (ucty)((b - dd) & (ucty)(FULL_MASK));                                     \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Multiplication: gcd(ac, ad, bc, 2^w)Z + ((bd) mod 2^w) */                           \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_mul_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty fm = (ucty)(FULL_MASK);                                                         \
    ucty a = (ucty)d1->modulus, b = (ucty)d1->residue;                                   \
    ucty c = (ucty)d2->modulus, dd = (ucty)d2->residue;                                  \
    ucty ac = (ucty)(a * c) & fm;                                                        \
    ucty ad = (ucty)(a * dd) & fm;                                                       \
    ucty bc = (ucty)(b * c) & fm;                                                        \
    ucty g = congr_gcd_##sm(congr_gcd_##sm(congr_gcd_##sm(ac, ad), bc), 0);              \
    /* gcd with 0 gives the other arg; gcd with 2^w handled by xi_norm */                \
    ucty res = (ucty)(b * dd) & fm;                                                      \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Division: singleton divisor n!=0 where n|a and n|b: (a/|n|)Z+(b/n). Else top. */    \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_div_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty c = (ucty)d2->modulus;                                                          \
    ucty dd = (ucty)d2->residue;                                                         \
    /* Division by zero */                                                               \
    if (c == 0 && dd == 0)                                                               \
      return bennet_domain_congr_bottom_##cty();                                         \
    /* Only handle singleton divisor */                                                  \
    if (c != 0)                                                                          \
      return bennet_domain_congr_top_##cty();                                            \
    ucty a = (ucty)d1->modulus;                                                          \
    ucty b = (ucty)d1->residue;                                                          \
    /* Check divisibility */                                                             \
    if (a % dd != 0 || b % dd != 0)                                                      \
      return bennet_domain_congr_top_##cty();                                            \
    ucty g = a / dd;                                                                     \
    ucty res = b / dd;                                                                   \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Modulo: singleton divisor n!=0: gcd(a, |n|)Z + (b mod gcd(a, |n|)). Else top. */    \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_mod_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty c = (ucty)d2->modulus;                                                          \
    ucty dd = (ucty)d2->residue;                                                         \
    if (c == 0 && dd == 0)                                                               \
      return bennet_domain_congr_bottom_##cty();                                         \
    if (c != 0)                                                                          \
      return bennet_domain_congr_top_##cty();                                            \
    ucty a = (ucty)d1->modulus;                                                          \
    ucty b = (ucty)d1->residue;                                                          \
    ucty g = congr_gcd_##sm(a, dd);                                                      \
    ucty res = (g == 0) ? (b % dd) : (b % g);                                            \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Bitwise AND: extract trailing zeros k = ctz(modulus). */                            \
  /* Let k = min(k_x, k_y). Result: 2^k Z + ((b & d) mod 2^k) */                         \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_and_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    /* Singletons: exact */                                                              \
    ucty a = (ucty)d1->modulus, b = (ucty)d1->residue;                                   \
    ucty c = (ucty)d2->modulus, dd = (ucty)d2->residue;                                  \
    if (a == 0 && c == 0) {                                                              \
      return bennet_domain_congr_of_##cty((cty)0, (cty)(b & dd));                        \
    }                                                                                    \
    /* AND with 0 */                                                                     \
    if (a == 0 && b == 0)                                                                \
      return bennet_domain_congr_of_##cty(0, 0);                                         \
    if (c == 0 && dd == 0)                                                               \
      return bennet_domain_congr_of_##cty(0, 0);                                         \
    /* General: use trailing zeros */                                                    \
    int k1 = (a == 0) ? (int)(sizeof(cty) * 8) : __builtin_ctz##sm(a);                   \
    int k2 = (c == 0) ? (int)(sizeof(cty) * 8) : __builtin_ctz##sm(c);                   \
    int k = (k1 < k2) ? k1 : k2;                                                         \
    if (k == 0)                                                                          \
      return bennet_domain_congr_top_##cty();                                            \
    ucty g = (ucty)1 << k;                                                               \
    ucty res = (b & dd) & (g - 1);                                                       \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Bitwise OR: same trailing zero approach */                                          \
  bennet_domain_congr(cty) * bennet_domain_congr_or_##cty(bennet_domain_congr(cty) * d1, \
                                 bennet_domain_congr(cty) * d2) {                        \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty a = (ucty)d1->modulus, b = (ucty)d1->residue;                                   \
    ucty c = (ucty)d2->modulus, dd = (ucty)d2->residue;                                  \
    if (a == 0 && c == 0) {                                                              \
      return bennet_domain_congr_of_##cty((cty)0, (cty)(b | dd));                        \
    }                                                                                    \
    int k1 = (a == 0) ? (int)(sizeof(cty) * 8) : __builtin_ctz##sm(a);                   \
    int k2 = (c == 0) ? (int)(sizeof(cty) * 8) : __builtin_ctz##sm(c);                   \
    int k = (k1 < k2) ? k1 : k2;                                                         \
    if (k == 0)                                                                          \
      return bennet_domain_congr_top_##cty();                                            \
    ucty g = (ucty)1 << k;                                                               \
    ucty res = (b | dd) & (g - 1);                                                       \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Bitwise XOR: same trailing zero approach */                                         \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_xor_##cty(                                                     \
          bennet_domain_congr(cty) * d1, bennet_domain_congr(cty) * d2) {                \
    if (d1->bottom || d2->bottom)                                                        \
      return bennet_domain_congr_bottom_##cty();                                         \
    ucty a = (ucty)d1->modulus, b = (ucty)d1->residue;                                   \
    ucty c = (ucty)d2->modulus, dd = (ucty)d2->residue;                                  \
    if (a == 0 && c == 0) {                                                              \
      return bennet_domain_congr_of_##cty((cty)0, (cty)(b ^ dd));                        \
    }                                                                                    \
    int k1 = (a == 0) ? (int)(sizeof(cty) * 8) : __builtin_ctz##sm(a);                   \
    int k2 = (c == 0) ? (int)(sizeof(cty) * 8) : __builtin_ctz##sm(c);                   \
    int k = (k1 < k2) ? k1 : k2;                                                         \
    if (k == 0)                                                                          \
      return bennet_domain_congr_top_##cty();                                            \
    ucty g = (ucty)1 << k;                                                               \
    ucty res = (b ^ dd) & (g - 1);                                                       \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Shift left by constant k: gcd(a*2^k, 2^w)Z + ((b*2^k) mod 2^w) */                   \
  bennet_domain_congr(cty) * bennet_domain_congr_shl_##cty(bennet_domain_congr(cty) * d, \
                                 bennet_domain_congr(cty) * shift_amt) {                 \
    if (d->bottom || shift_amt->bottom)                                                  \
      return bennet_domain_congr_bottom_##cty();                                         \
    /* Non-constant shift: top */                                                        \
    if ((ucty)shift_amt->modulus != 0)                                                   \
      return bennet_domain_congr_top_##cty();                                            \
    int width = (int)(sizeof(cty) * 8);                                                  \
    int k = (int)(ucty)shift_amt->residue;                                               \
    if (k < 0 || k >= width)                                                             \
      return bennet_domain_congr_top_##cty();                                            \
    ucty fm = (ucty)(FULL_MASK);                                                         \
    ucty a = (ucty)d->modulus;                                                           \
    ucty b = (ucty)d->residue;                                                           \
    ucty g = (a == 0) ? 0 : (ucty)(a << k) & fm;                                         \
    ucty res = (ucty)(b << k) & fm;                                                      \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  /* Logical right shift by constant k */                                                \
  bennet_domain_congr(cty) *                                                             \
      bennet_domain_congr_lshr_##cty(                                                    \
          bennet_domain_congr(cty) * d, bennet_domain_congr(cty) * shift_amt) {          \
    if (d->bottom || shift_amt->bottom)                                                  \
      return bennet_domain_congr_bottom_##cty();                                         \
    if ((ucty)shift_amt->modulus != 0)                                                   \
      return bennet_domain_congr_top_##cty();                                            \
    int width = (int)(sizeof(cty) * 8);                                                  \
    int k = (int)(ucty)shift_amt->residue;                                               \
    if (k < 0 || k >= width)                                                             \
      return bennet_domain_congr_top_##cty();                                            \
    ucty a = (ucty)d->modulus;                                                           \
    ucty b = (ucty)d->residue;                                                           \
    /* Check if shift is cleanly divisible */                                            \
    ucty shift_mask = ((ucty)1 << k) - 1;                                                \
    if (a != 0 && (a & shift_mask) != 0)                                                 \
      return bennet_domain_congr_top_##cty();                                            \
    ucty g = (a == 0) ? 0 : a >> k;                                                      \
    ucty res = b >> k;                                                                   \
    CONGR_XI_NORM_CALL(sm, ucty, g, res);                                                \
    return bennet_domain_congr_of_##cty((cty)g, (cty)res);                               \
  }                                                                                      \
                                                                                         \
  bool bennet_domain_congr_to_interval_##cty(                                            \
      bennet_domain_congr(cty) * d, cty * lo_out, cty * hi_out) {                        \
    if (d->top || d->bottom)                                                             \
      return false;                                                                      \
    ucty m = (ucty)d->modulus;                                                           \
    ucty res = (ucty)d->residue;                                                         \
    if (m == 0) {                                                                        \
      *lo_out = (cty)res;                                                                \
      *hi_out = (cty)res;                                                                \
      return true;                                                                       \
    }                                                                                    \
    /* Compute max element: residue + modulus * floor((max - residue) / modulus) */      \
    ucty fm = (ucty)(FULL_MASK);                                                         \
    ucty max_elem = res + m * ((fm - res) / m);                                          \
    *lo_out = (cty)res;                                                                  \
    *hi_out = (cty)max_elem;                                                             \
    return true;                                                                         \
  }                                                                                      \
                                                                                         \
  bennet_domain_congr(cty) * bennet_domain_congr_of_interval_##cty(cty lo, cty hi) {     \
    if ((ucty)lo > (ucty)hi)                                                             \
      return bennet_domain_congr_bottom_##cty();                                         \
    if (lo == hi)                                                                        \
      return bennet_domain_congr_of_##cty(0, lo);                                        \
    /* General interval: top (congruence can't represent arbitrary intervals) */         \
    return bennet_domain_congr_top_##cty();                                              \
  }

/* Use __builtin_ctz for 32-bit, __builtin_ctzll for 64-bit.
 * Each macro casts to the appropriate unsigned type to avoid truncation. */
#define __builtin_ctz8(x)  __builtin_ctz((uint8_t)(x))
#define __builtin_ctz16(x) __builtin_ctz((uint16_t)(x))
#define __builtin_ctz32(x) __builtin_ctz((uint32_t)(x))
#define __builtin_ctz64(x) __builtin_ctzll((uint64_t)(x))

CONGR_DOMAIN_IMPL(uint8_t, uint8_t, UINT8_MAX, 8)
CONGR_DOMAIN_IMPL(uint16_t, uint16_t, UINT16_MAX, 16)
CONGR_DOMAIN_IMPL(uint32_t, uint32_t, UINT32_MAX, 32)
CONGR_DOMAIN_IMPL(uint64_t, uint64_t, UINT64_MAX, 64)
CONGR_DOMAIN_IMPL(uintptr_t, uintptr_t, UINTPTR_MAX, 64)
CONGR_DOMAIN_IMPL(int8_t, uint8_t, UINT8_MAX, 8)
CONGR_DOMAIN_IMPL(int16_t, uint16_t, UINT16_MAX, 16)
CONGR_DOMAIN_IMPL(int32_t, uint32_t, UINT32_MAX, 32)
CONGR_DOMAIN_IMPL(int64_t, uint64_t, UINT64_MAX, 64)

/*-----------------------------------------------------------------------------
 * Generic congruence representation (type-erased for tagged domain ops)
 *---------------------------------------------------------------------------*/

typedef struct {
  bool is_top;
  bool is_bottom;
  bool is_signed;
  int width;
  uint64_t modulus;
  uint64_t residue;
} congr_generic;

static void congr_get_type_info(cn_base_type* type, int* width, bool* is_signed) {
  *width = 64;
  *is_signed = false;
  if (!type)
    return;
  if (type->tag == CN_BASE_LOC) {
    *width = 64;
    *is_signed = false;
  } else if (type->tag == CN_BASE_BITS) {
    *width = type->data.bits.size_bits;
    *is_signed = type->data.bits.is_signed;
  }
}

static congr_generic congr_generic_top(int width, bool is_signed) {
  return (congr_generic){
      .is_top = true,
      .is_bottom = false,
      .is_signed = is_signed,
      .width = width,
      .modulus = 1,
      .residue = 0,
  };
}

static congr_generic congr_generic_bottom(int width, bool is_signed) {
  return (congr_generic){
      .is_top = false,
      .is_bottom = true,
      .is_signed = is_signed,
      .width = width,
      .modulus = 0,
      .residue = 0,
  };
}

static congr_generic congr_from_tagged(bennet_tagged_domain* d) {
  congr_generic result = {0};
  if (!d || !d->type || !d->domain) {
    result.is_top = true;
    result.width = 64;
    return result;
  }

  congr_get_type_info(d->type, &result.width, &result.is_signed);

  if (result.is_signed) {
    switch (result.width) {
      case 8: {
        bennet_domain_congr(int8_t)* dom = (bennet_domain_congr(int8_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)(uint8_t)dom->modulus;
        result.residue = (uint64_t)(uint8_t)dom->residue;
        break;
      }
      case 16: {
        bennet_domain_congr(int16_t)* dom = (bennet_domain_congr(int16_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)(uint16_t)dom->modulus;
        result.residue = (uint64_t)(uint16_t)dom->residue;
        break;
      }
      case 32: {
        bennet_domain_congr(int32_t)* dom = (bennet_domain_congr(int32_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)(uint32_t)dom->modulus;
        result.residue = (uint64_t)(uint32_t)dom->residue;
        break;
      }
      case 64:
      default: {
        bennet_domain_congr(int64_t)* dom = (bennet_domain_congr(int64_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)dom->modulus;
        result.residue = (uint64_t)dom->residue;
        break;
      }
    }
  } else {
    switch (result.width) {
      case 8: {
        bennet_domain_congr(uint8_t)* dom = (bennet_domain_congr(uint8_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)dom->modulus;
        result.residue = (uint64_t)dom->residue;
        break;
      }
      case 16: {
        bennet_domain_congr(uint16_t)* dom = (bennet_domain_congr(uint16_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)dom->modulus;
        result.residue = (uint64_t)dom->residue;
        break;
      }
      case 32: {
        bennet_domain_congr(uint32_t)* dom = (bennet_domain_congr(uint32_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)dom->modulus;
        result.residue = (uint64_t)dom->residue;
        break;
      }
      case 64:
      default: {
        bennet_domain_congr(uint64_t)* dom = (bennet_domain_congr(uint64_t)*)d->domain;
        result.is_top = dom->top;
        result.is_bottom = dom->bottom;
        result.modulus = (uint64_t)dom->modulus;
        result.residue = (uint64_t)dom->residue;
        break;
      }
    }
  }

  return result;
}

static bennet_tagged_domain congr_to_tagged(congr_generic* g, cn_base_type* type) {
  bennet_tagged_domain result;
  result.type = type;

  int width;
  bool is_signed;
  congr_get_type_info(type, &width, &is_signed);

  if (is_signed) {
    switch (width) {
      case 8: {
        bennet_domain_congr(int8_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (int8_t)(uint8_t)g->modulus;
        dom->residue = (int8_t)(uint8_t)g->residue;
        result.domain = dom;
        break;
      }
      case 16: {
        bennet_domain_congr(int16_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (int16_t)(uint16_t)g->modulus;
        dom->residue = (int16_t)(uint16_t)g->residue;
        result.domain = dom;
        break;
      }
      case 32: {
        bennet_domain_congr(int32_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (int32_t)(uint32_t)g->modulus;
        dom->residue = (int32_t)(uint32_t)g->residue;
        result.domain = dom;
        break;
      }
      case 64:
      default: {
        bennet_domain_congr(int64_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (int64_t)g->modulus;
        dom->residue = (int64_t)g->residue;
        result.domain = dom;
        break;
      }
    }
  } else {
    switch (width) {
      case 8: {
        bennet_domain_congr(uint8_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (uint8_t)g->modulus;
        dom->residue = (uint8_t)g->residue;
        result.domain = dom;
        break;
      }
      case 16: {
        bennet_domain_congr(uint16_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (uint16_t)g->modulus;
        dom->residue = (uint16_t)g->residue;
        result.domain = dom;
        break;
      }
      case 32: {
        bennet_domain_congr(uint32_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (uint32_t)g->modulus;
        dom->residue = (uint32_t)g->residue;
        result.domain = dom;
        break;
      }
      case 64:
      default: {
        bennet_domain_congr(uint64_t)* dom = std_malloc(sizeof(*dom));
        dom->top = g->is_top;
        dom->bottom = g->is_bottom;
        dom->modulus = (uint64_t)g->modulus;
        dom->residue = (uint64_t)g->residue;
        result.domain = dom;
        break;
      }
    }
  }

  return result;
}

static congr_generic congr_generic_meet(congr_generic* a, congr_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top)
    return *b;
  if (b->is_top)
    return *a;

  uint64_t am = a->modulus, ar = a->residue;
  uint64_t bm = b->modulus, br = b->residue;

  /* Both singletons */
  if (am == 0 && bm == 0) {
    if (ar == br)
      return *a;
    return congr_generic_bottom(a->width, a->is_signed);
  }

  /* One singleton */
  if (am == 0) {
    if ((ar & (bm - 1)) == br)
      return *a;
    return congr_generic_bottom(a->width, a->is_signed);
  }
  if (bm == 0) {
    if ((br & (am - 1)) == ar)
      return *b;
    return congr_generic_bottom(a->width, a->is_signed);
  }

  /* General: both power-of-2 moduli */
  uint64_t g = (am < bm) ? am : bm;
  uint64_t l = (am > bm) ? am : bm;
  if ((ar & (g - 1)) != (br & (g - 1)))
    return congr_generic_bottom(a->width, a->is_signed);

  uint64_t res = (am >= bm) ? ar : br;
  congr_xi_norm_64(&l, &res);

  return (congr_generic){
      .is_top = (l == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = l,
      .residue = res,
  };
}

/*-----------------------------------------------------------------------------
 * Generic Arithmetic Helpers (64-bit, type-erased)
 *---------------------------------------------------------------------------*/

static congr_generic congr_generic_const(int width, bool is_signed, uint64_t value) {
  uint64_t mod = 0;
  uint64_t res = value;
  congr_xi_norm_64(&mod, &res);
  return (congr_generic){
      .is_top = false,
      .is_bottom = false,
      .is_signed = is_signed,
      .width = width,
      .modulus = mod,
      .residue = res,
  };
}

/* Addition: gcd(a, c)Z + (b + d) */
static congr_generic congr_generic_add(congr_generic* a, congr_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return congr_generic_top(a->width, a->is_signed);

  uint64_t g = congr_gcd_64(a->modulus, b->modulus);
  uint64_t res = a->residue + b->residue;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Subtraction: gcd(a, c)Z + (b - d) */
static congr_generic congr_generic_sub(congr_generic* a, congr_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return congr_generic_top(a->width, a->is_signed);

  uint64_t g = congr_gcd_64(a->modulus, b->modulus);
  uint64_t res = a->residue - b->residue;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Multiplication: gcd(ac, ad, bc)Z + (bd) */
static congr_generic congr_generic_mul(congr_generic* a, congr_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return congr_generic_top(a->width, a->is_signed);

  uint64_t am = a->modulus, ar = a->residue;
  uint64_t bm = b->modulus, br = b->residue;
  uint64_t ac = am * bm;
  uint64_t ad = am * br;
  uint64_t bc = ar * bm;
  uint64_t g = congr_gcd_64(congr_gcd_64(congr_gcd_64(ac, ad), bc), 0);
  uint64_t res = ar * br;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Shift left by singleton shift: modulus << k, residue << k */
static congr_generic congr_generic_shl(congr_generic* a, congr_generic* shift) {
  if (a->is_bottom || shift->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || shift->is_top)
    return congr_generic_top(a->width, a->is_signed);
  /* Shift must be singleton */
  if (shift->modulus != 0)
    return congr_generic_top(a->width, a->is_signed);

  int k = (int)shift->residue;
  if (k < 0 || k >= 64)
    return congr_generic_top(a->width, a->is_signed);

  uint64_t g = (a->modulus == 0) ? 0 : (a->modulus << k);
  uint64_t res = a->residue << k;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Logical right shift by singleton shift */
static congr_generic congr_generic_lshr(congr_generic* a, congr_generic* shift) {
  if (a->is_bottom || shift->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || shift->is_top)
    return congr_generic_top(a->width, a->is_signed);
  if (shift->modulus != 0)
    return congr_generic_top(a->width, a->is_signed);

  int k = (int)shift->residue;
  if (k < 0 || k >= 64)
    return congr_generic_top(a->width, a->is_signed);

  /* Check if shift is cleanly divisible */
  uint64_t shift_mask = ((uint64_t)1 << k) - 1;
  if (a->modulus != 0 && (a->modulus & shift_mask) != 0)
    return congr_generic_top(a->width, a->is_signed);

  uint64_t g = (a->modulus == 0) ? 0 : (a->modulus >> k);
  uint64_t res = a->residue >> k;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Division: singleton divisor n!=0 where n|a and n|b: (a/n)Z+(b/n). Else top. */
static congr_generic congr_generic_div(congr_generic* a, congr_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return congr_generic_top(a->width, a->is_signed);
  /* Only handle singleton divisor */
  if (b->modulus != 0)
    return congr_generic_top(a->width, a->is_signed);
  /* Division by zero */
  if (b->residue == 0)
    return congr_generic_bottom(a->width, a->is_signed);
  /* Check divisibility */
  if (a->modulus % b->residue != 0 || a->residue % b->residue != 0)
    return congr_generic_top(a->width, a->is_signed);
  uint64_t g = a->modulus / b->residue;
  uint64_t res = a->residue / b->residue;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Modulo: singleton divisor n!=0: gcd(a, n)Z + (b mod gcd(a, n)). Else top. */
static congr_generic congr_generic_mod(congr_generic* a, congr_generic* b) {
  if (a->is_bottom || b->is_bottom)
    return congr_generic_bottom(a->width, a->is_signed);
  if (a->is_top || b->is_top)
    return congr_generic_top(a->width, a->is_signed);
  /* Only handle singleton divisor */
  if (b->modulus != 0)
    return congr_generic_top(a->width, a->is_signed);
  /* Division by zero */
  if (b->residue == 0)
    return congr_generic_bottom(a->width, a->is_signed);
  uint64_t am = a->modulus;
  uint64_t ar = a->residue;
  uint64_t n = b->residue;
  uint64_t g = congr_gcd_64(am, n);
  uint64_t res = (g == 0) ? (ar % n) : (ar % g);
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/* Negate: (-a)Z + (-b) = aZ + (-b) */
static congr_generic congr_generic_negate(congr_generic* a) {
  if (a->is_bottom)
    return *a;
  if (a->is_top)
    return *a;

  uint64_t g = a->modulus;
  uint64_t res = -a->residue;
  congr_xi_norm_64(&g, &res);
  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

static congr_generic congr_generic_join(congr_generic* a, congr_generic* b) {
  if (a->is_bottom)
    return *b;
  if (b->is_bottom)
    return *a;
  if (a->is_top || b->is_top)
    return congr_generic_top(a->width, a->is_signed);

  uint64_t am = a->modulus, ar = a->residue;
  uint64_t bm = b->modulus, br = b->residue;
  uint64_t diff = (ar >= br) ? (ar - br) : (br - ar);
  uint64_t g = congr_gcd_64(congr_gcd_64(am, bm), diff);
  uint64_t res = (g == 0) ? ar : (ar & (g - 1));
  congr_xi_norm_64(&g, &res);

  return (congr_generic){
      .is_top = (g == 1),
      .is_bottom = false,
      .is_signed = a->is_signed,
      .width = a->width,
      .modulus = g,
      .residue = res,
  };
}

/*-----------------------------------------------------------------------------
 * Tagged Domain Functions (congr-specific)
 *---------------------------------------------------------------------------*/

bool bennet_tagged_domain_is_bottom_congr(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return false;
  congr_generic g = congr_from_tagged(d);
  return g.is_bottom;
}

bool bennet_tagged_domain_is_top_congr(bennet_tagged_domain* d) {
  if (!d || !d->domain)
    return true;
  congr_generic g = congr_from_tagged(d);
  return g.is_top;
}

bennet_tagged_domain bennet_tagged_domain_copy_congr(bennet_tagged_domain* d) {
  if (!d || !d->domain) {
    return bennet_tagged_domain_top_congr(d ? d->type : NULL);
  }
  congr_generic g = congr_from_tagged(d);
  return congr_to_tagged(&g, d->type);
}

bennet_tagged_domain bennet_tagged_domain_top_congr(cn_base_type* type) {
  int width = 64;
  bool is_signed = false;
  if (type)
    congr_get_type_info(type, &width, &is_signed);
  congr_generic g = congr_generic_top(width, is_signed);
  return congr_to_tagged(&g, type);
}

bennet_tagged_domain bennet_tagged_domain_bottom_congr(cn_base_type* type) {
  int width = 64;
  bool is_signed = false;
  if (type)
    congr_get_type_info(type, &width, &is_signed);
  congr_generic g = congr_generic_bottom(width, is_signed);
  return congr_to_tagged(&g, type);
}

bennet_tagged_domain bennet_tagged_domain_meet_congr(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2 && d1->type && d2->type);
  congr_generic g1 = congr_from_tagged(d1);
  congr_generic g2 = congr_from_tagged(d2);
  congr_generic result = congr_generic_meet(&g1, &g2);
  return congr_to_tagged(&result, d1->type);
}

bennet_tagged_domain bennet_tagged_domain_join_congr(
    bennet_tagged_domain* d1, bennet_tagged_domain* d2) {
  assert(d1 && d2 && d1->type && d2->type);
  congr_generic g1 = congr_from_tagged(d1);
  congr_generic g2 = congr_from_tagged(d2);
  congr_generic result = congr_generic_join(&g1, &g2);
  return congr_to_tagged(&result, d1->type);
}

/*-----------------------------------------------------------------------------
 * Abstract State Implementation (congr)
 *---------------------------------------------------------------------------*/

BENNET_ABSINT_STATE_IMPL(congr)

/*-----------------------------------------------------------------------------
 * Forward Transform (congr)
 *---------------------------------------------------------------------------*/

static congr_generic congr_forward_binop(
    cn_binop op, congr_generic* left, congr_generic* right) {
  switch (op) {
    case CN_BINOP_ADD:
      return congr_generic_add(left, right);
    case CN_BINOP_SUB:
      return congr_generic_sub(left, right);
    case CN_BINOP_MUL:
    case CN_BINOP_MULNOSMT:
      return congr_generic_mul(left, right);
    case CN_BINOP_SHIFT_LEFT: {
      return congr_generic_shl(left, right);
    }
    case CN_BINOP_SHIFT_RIGHT: {
      return congr_generic_lshr(left, right);
    }
    case CN_BINOP_DIV:
    case CN_BINOP_DIVNOSMT:
      return congr_generic_div(left, right);
    case CN_BINOP_MOD:
    case CN_BINOP_MODNOSMT:
    case CN_BINOP_REM:
    case CN_BINOP_REMNOSMT:
      return congr_generic_mod(left, right);
    default:
      return congr_generic_top(left->width, left->is_signed);
  }
}

bennet_tagged_domain bennet_congr_transform_forward(
    cn_term* term, bennet_absint_state* state) {
  if (!term) {
    cn_base_type bt = cn_base_type_bits(false, 64);
    return bennet_tagged_domain_top_congr(&bt);
  }

  switch (term->type) {
    case CN_TERM_CONST: {
      int width = 64;
      bool is_signed = false;
      congr_get_type_info(&term->base_type, &width, &is_signed);

      uint64_t val = 0;
      switch (term->data.const_val.type) {
        case CN_CONST_BITS:
          val = (uint64_t)term->data.const_val.data.bits.value;
          break;
        case CN_CONST_Z:
          val = (uint64_t)term->data.const_val.data.z;
          break;
        case CN_CONST_POINTER:
          val = (uint64_t)term->data.const_val.data.pointer;
          break;
        case CN_CONST_BOOL:
          val = term->data.const_val.data.boolean ? 1 : 0;
          break;
        case CN_CONST_NULL:
          val = 0;
          break;
        default:
          return bennet_tagged_domain_top_congr(&term->base_type);
      }
      congr_generic g = congr_generic_const(width, is_signed, val);
      return congr_to_tagged(&g, &term->base_type);
    }

    case CN_TERM_SYM: {
      bennet_absint_sym sym = {.name = term->data.sym.name, .id = term->data.sym.id};
      return bennet_absint_state_get_congr(state, sym, &term->base_type);
    }

    case CN_TERM_UNOP: {
      bennet_tagged_domain operand_td =
          bennet_congr_transform_forward(term->data.unop.operand, state);
      congr_generic operand = congr_from_tagged(&operand_td);

      switch (term->data.unop.op) {
        case CN_UNOP_NEGATE: {
          congr_generic result = congr_generic_negate(&operand);
          return congr_to_tagged(&result, &term->base_type);
        }
        default:
          return bennet_tagged_domain_top_congr(&term->base_type);
      }
    }

    case CN_TERM_BINOP: {
      bennet_tagged_domain left_td =
          bennet_congr_transform_forward(term->data.binop.left, state);
      bennet_tagged_domain right_td =
          bennet_congr_transform_forward(term->data.binop.right, state);
      congr_generic lg = congr_from_tagged(&left_td);
      congr_generic rg = congr_from_tagged(&right_td);
      congr_generic result = congr_forward_binop(term->data.binop.op, &lg, &rg);
      return congr_to_tagged(&result, &term->base_type);
    }

    case CN_TERM_ITE: {
      bennet_tagged_domain then_td =
          bennet_congr_transform_forward(term->data.ite.then_term, state);
      bennet_tagged_domain else_td =
          bennet_congr_transform_forward(term->data.ite.else_term, state);
      return bennet_tagged_domain_join_congr(&then_td, &else_td);
    }

    case CN_TERM_CAST: {
      bennet_tagged_domain src_td =
          bennet_congr_transform_forward(term->data.cast.value, state);
      congr_generic src = congr_from_tagged(&src_td);
      if (src.is_bottom) {
        return bennet_tagged_domain_bottom_congr(&term->base_type);
      }
      /* Congruence info propagates through casts (modulus/residue still valid) */
      int dst_width = 64;
      bool dst_signed = false;
      congr_get_type_info(&term->base_type, &dst_width, &dst_signed);
      src.width = dst_width;
      src.is_signed = dst_signed;
      /* Re-normalize for new width */
      congr_xi_norm_64(&src.modulus, &src.residue);
      src.is_top = (src.modulus == 1);
      return congr_to_tagged(&src, &term->base_type);
    }

    case CN_TERM_ARRAY_SHIFT: {
      bennet_tagged_domain base_td =
          bennet_congr_transform_forward(term->data.array_shift.base, state);
      bennet_tagged_domain index_td =
          bennet_congr_transform_forward(term->data.array_shift.index, state);
      congr_generic base_g = congr_from_tagged(&base_td);
      congr_generic index_g = congr_from_tagged(&index_td);

      /* elem_size as singleton constant */
      congr_generic elem_g = congr_generic_const(index_g.width,
          index_g.is_signed,
          (uint64_t)term->data.array_shift.element_size);

      /* result = base + index * elem_size */
      congr_generic offset = congr_generic_mul(&index_g, &elem_g);
      congr_generic result = congr_generic_add(&base_g, &offset);
      return congr_to_tagged(&result, &term->base_type);
    }

    case CN_TERM_MEMBER_SHIFT: {
      bennet_tagged_domain base_td =
          bennet_congr_transform_forward(term->data.member_shift.base, state);
      congr_generic base_g = congr_from_tagged(&base_td);

      congr_generic offset_g = congr_generic_const(
          base_g.width, base_g.is_signed, (uint64_t)term->data.member_shift.offset);

      congr_generic result = congr_generic_add(&base_g, &offset_g);
      return congr_to_tagged(&result, &term->base_type);
    }

    default:
      return bennet_tagged_domain_top_congr(&term->base_type);
  }
}

/*-----------------------------------------------------------------------------
 * Backward Transform Helpers (congr)
 *---------------------------------------------------------------------------*/

/**
 * Collect all sym IDs from a term into a fixed-size buffer.
 * Returns the number of syms found (up to max_syms).
 */
static int congr_term_collect_syms(cn_term* term, bennet_absint_sym* syms, int max_syms) {
  if (!term || max_syms <= 0)
    return 0;

  switch (term->type) {
    case CN_TERM_SYM: {
      syms[0] = (bennet_absint_sym){.name = term->data.sym.name, .id = term->data.sym.id};
      return 1;
    }
    case CN_TERM_UNOP:
      return congr_term_collect_syms(term->data.unop.operand, syms, max_syms);
    case CN_TERM_BINOP: {
      int n = congr_term_collect_syms(term->data.binop.left, syms, max_syms);
      n += congr_term_collect_syms(term->data.binop.right, syms + n, max_syms - n);
      return n;
    }
    case CN_TERM_CAST:
      return congr_term_collect_syms(term->data.cast.value, syms, max_syms);
    case CN_TERM_ITE: {
      int n = congr_term_collect_syms(term->data.ite.then_term, syms, max_syms);
      n += congr_term_collect_syms(term->data.ite.else_term, syms + n, max_syms - n);
      return n;
    }
    case CN_TERM_ARRAY_SHIFT: {
      int n = congr_term_collect_syms(term->data.array_shift.base, syms, max_syms);
      n += congr_term_collect_syms(term->data.array_shift.index, syms + n, max_syms - n);
      return n;
    }
    case CN_TERM_MEMBER_SHIFT:
      return congr_term_collect_syms(term->data.member_shift.base, syms, max_syms);
    default:
      return 0;
  }
}

/**
 * Apply a refined domain to all SYMs in a term by calling transform_backward
 * for each SYM found. Accumulates refinements into the state.
 */
static bennet_absint_state* congr_backward_apply_to_all_syms(
    cn_term* term, bennet_tagged_domain* refined_dom, bennet_absint_state* state) {
  bennet_absint_sym syms[16];
  int n = congr_term_collect_syms(term, syms, 16);

  bennet_absint_state* result = state;
  for (int i = 0; i < n; i++) {
    result = bennet_congr_transform_backward(term, syms[i], *refined_dom, result);
  }
  return result;
}

/*-----------------------------------------------------------------------------
 * Backward Transform (congr)
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_congr_transform_backward(cn_term* term,
    bennet_absint_sym target_sym,
    bennet_tagged_domain output_domain,
    bennet_absint_state* state) {
  if (!term || !state)
    return state;

  /* Bottom output -> propagate bottom to target */
  if (bennet_tagged_domain_is_bottom_congr(&output_domain)) {
    return bennet_absint_state_set_congr(bennet_absint_state_copy_congr(state),
        target_sym,
        bennet_tagged_domain_bottom_congr(&term->base_type));
  }

  switch (term->type) {
    case CN_TERM_SYM: {
      if (term->data.sym.id == target_sym.id) {
        return bennet_absint_state_meet_congr(state, target_sym, output_domain);
      }
      return bennet_absint_state_copy_congr(state);
    }

    case CN_TERM_BINOP: {
      cn_term* left = term->data.binop.left;
      cn_term* right = term->data.binop.right;
      bool left_has_target = term_contains_sym(left, target_sym.id);
      bool right_has_target = term_contains_sym(right, target_sym.id);

      if (!left_has_target && !right_has_target)
        return bennet_absint_state_copy_congr(state);

      /* Comparison ops are handled by backward_assume */
      switch (term->data.binop.op) {
        case CN_BINOP_EQ:
        case CN_BINOP_LT:
        case CN_BINOP_LE:
        case CN_BINOP_LT_POINTER:
        case CN_BINOP_LE_POINTER:
          return bennet_absint_state_copy_congr(state);
        default:
          break;
      }

      congr_generic out = congr_from_tagged(&output_domain);
      cn_term* other = left_has_target ? right : left;
      cn_term* target_side = left_has_target ? left : right;
      bennet_tagged_domain other_td = bennet_congr_transform_forward(other, state);
      congr_generic og = congr_from_tagged(&other_td);

      if (!out.is_top && !og.is_top) {
        congr_generic inverted;
        bool did_invert = true;

        switch (term->data.binop.op) {
          case CN_BINOP_ADD:
            /* out = target + other => target = out - other */
            inverted = congr_generic_sub(&out, &og);
            break;
          case CN_BINOP_SUB:
            if (left_has_target) {
              /* out = target - other => target = out + other */
              inverted = congr_generic_add(&out, &og);
            } else {
              /* out = other - target => target = other - out */
              inverted = congr_generic_sub(&og, &out);
            }
            break;
          case CN_BINOP_MUL:
          case CN_BINOP_MULNOSMT:
            /* Only invertible if other is singleton */
            if (og.modulus == 0 && og.residue != 0) {
              inverted = congr_generic_div(&out, &og);
            } else {
              did_invert = false;
            }
            break;
          case CN_BINOP_SHIFT_LEFT:
            if (left_has_target && og.modulus == 0) {
              /* out = target << k => target = out >> k */
              inverted = congr_generic_lshr(&out, &og);
            } else {
              did_invert = false;
            }
            break;
          case CN_BINOP_SHIFT_RIGHT:
            did_invert = false;
            break;
          case CN_BINOP_DIV:
          case CN_BINOP_DIVNOSMT:
            /* out = target / n => target in (out.modulus * n)Z + (out.residue * n) */
            if (left_has_target && og.modulus == 0 && og.residue == 1) {
              inverted = out; /* target / 1 = target, trivially exact */
            } else {
              did_invert = false;
            }
            break;
          case CN_BINOP_MOD:
          case CN_BINOP_MODNOSMT:
          case CN_BINOP_REM:
          case CN_BINOP_REMNOSMT:
            /* out = target mod n => target in gcd(out.modulus, n)Z + out.residue */
            if (left_has_target && og.modulus == 0 && og.residue != 0) {
              uint64_t n = og.residue;
              uint64_t g = congr_gcd_64(out.modulus, n);
              uint64_t res = (g == 0) ? out.residue : (out.residue & (g - 1));
              congr_xi_norm_64(&g, &res);
              inverted = (congr_generic){
                  .is_top = (g == 1),
                  .is_bottom = false,
                  .is_signed = out.is_signed,
                  .width = out.width,
                  .modulus = g,
                  .residue = res,
              };
            } else {
              did_invert = false;
            }
            break;
          default:
            did_invert = false;
            break;
        }

        if (did_invert) {
          bennet_tagged_domain inv_dom =
              congr_to_tagged(&inverted, &target_side->base_type);
          return bennet_congr_transform_backward(target_side, target_sym, inv_dom, state);
        }
      }

      /* No valid inversion — return unchanged state (sound over-approximation) */
      return bennet_absint_state_copy_congr(state);
    }

    case CN_TERM_UNOP: {
      /* Propagate to operand */
      if (term->data.unop.op == CN_UNOP_NEGATE) {
        /* out = -operand => operand = -out */
        congr_generic out = congr_from_tagged(&output_domain);
        if (!out.is_top) {
          congr_generic inv = congr_generic_negate(&out);
          bennet_tagged_domain inv_dom =
              congr_to_tagged(&inv, &term->data.unop.operand->base_type);
          return bennet_congr_transform_backward(
              term->data.unop.operand, target_sym, inv_dom, state);
        }
      }
      /* Unknown unop: no safe refinement possible */
      return bennet_absint_state_copy_congr(state);
    }

    case CN_TERM_ITE: {
      /* Propagate to both branches, join results */
      bennet_absint_state* then_state = bennet_congr_transform_backward(
          term->data.ite.then_term, target_sym, output_domain, state);
      bennet_absint_state* else_state = bennet_congr_transform_backward(
          term->data.ite.else_term, target_sym, output_domain, state);

      if (bennet_absint_state_is_bottom_congr(then_state))
        return else_state;
      if (bennet_absint_state_is_bottom_congr(else_state))
        return then_state;

      bennet_tagged_domain then_dom =
          bennet_absint_state_get_congr(then_state, target_sym, &term->base_type);
      bennet_tagged_domain else_dom =
          bennet_absint_state_get_congr(else_state, target_sym, &term->base_type);
      bennet_tagged_domain joined = bennet_tagged_domain_join_congr(&then_dom, &else_dom);
      return bennet_absint_state_set_congr(state, target_sym, joined);
    }

    case CN_TERM_ARRAY_SHIFT: {
      cn_term* base = term->data.array_shift.base;
      cn_term* index = term->data.array_shift.index;
      bool base_has_target = term_contains_sym(base, target_sym.id);
      bool index_has_target = term_contains_sym(index, target_sym.id);

      if (!base_has_target && !index_has_target)
        return bennet_absint_state_copy_congr(state);

      congr_generic out = congr_from_tagged(&output_domain);
      uint64_t elem_size = (uint64_t)term->data.array_shift.element_size;

      if (base_has_target && !out.is_top && !out.is_bottom) {
        /* result = base + index * elem_size => base = result - index * elem_size */
        bennet_tagged_domain index_td = bennet_congr_transform_forward(index, state);
        congr_generic index_g = congr_from_tagged(&index_td);

        if (!index_g.is_top) {
          congr_generic elem_g =
              congr_generic_const(index_g.width, index_g.is_signed, elem_size);
          congr_generic offset = congr_generic_mul(&index_g, &elem_g);
          if (!offset.is_top) {
            congr_generic inv = congr_generic_sub(&out, &offset);
            bennet_tagged_domain inv_dom = congr_to_tagged(&inv, &base->base_type);
            return bennet_congr_transform_backward(base, target_sym, inv_dom, state);
          }
        }
        return bennet_congr_transform_backward(base, target_sym, output_domain, state);
      }

      if (index_has_target && !out.is_top && !out.is_bottom) {
        /* result = base + index * elem_size */
        bennet_tagged_domain base_td = bennet_congr_transform_forward(base, state);
        congr_generic base_g = congr_from_tagged(&base_td);

        if (!base_g.is_top && elem_size != 0) {
          /* diff = result - base */
          congr_generic diff = congr_generic_sub(&out, &base_g);
          if (!diff.is_top) {
            /* For congruence: if elem_size divides diff's modulus and residue,
             * we can refine. Otherwise propagate output. */
            if (diff.modulus == 0) {
              /* Singleton diff: index = diff / elem_size (exact if divisible) */
              if (diff.residue % elem_size == 0) {
                congr_generic idx_refined =
                    congr_generic_const(index->base_type.data.bits.size_bits,
                        index->base_type.data.bits.is_signed,
                        diff.residue / elem_size);
                bennet_tagged_domain idx_dom =
                    congr_to_tagged(&idx_refined, &index->base_type);
                return bennet_congr_transform_backward(index, target_sym, idx_dom, state);
              }
            }
          }
        }
        return bennet_absint_state_copy_congr(state);
      }

      /* Fallback */
      cn_term* target_side = base_has_target ? base : index;
      return bennet_congr_transform_backward(
          target_side, target_sym, output_domain, state);
    }

    case CN_TERM_MEMBER_SHIFT: {
      cn_term* base = term->data.member_shift.base;
      if (!term_contains_sym(base, target_sym.id))
        return bennet_absint_state_copy_congr(state);

      congr_generic out = congr_from_tagged(&output_domain);
      if (!out.is_top && !out.is_bottom) {
        /* result = base + offset => base = result - offset */
        congr_generic offset_g = congr_generic_const(
            out.width, out.is_signed, (uint64_t)term->data.member_shift.offset);
        congr_generic inv = congr_generic_sub(&out, &offset_g);
        bennet_tagged_domain inv_dom = congr_to_tagged(&inv, &base->base_type);
        return bennet_congr_transform_backward(base, target_sym, inv_dom, state);
      }

      return bennet_congr_transform_backward(base, target_sym, output_domain, state);
    }

    case CN_TERM_CAST: {
      cn_term* inner = term->data.cast.value;
      if (!term_contains_sym(inner, target_sym.id))
        return bennet_absint_state_copy_congr(state);

      /* Propagate through cast: congruence info transfers across casts */
      congr_generic out = congr_from_tagged(&output_domain);
      int src_width = 64;
      bool src_signed = false;
      congr_get_type_info(&inner->base_type, &src_width, &src_signed);
      out.width = src_width;
      out.is_signed = src_signed;
      congr_xi_norm_64(&out.modulus, &out.residue);
      out.is_top = (out.modulus == 1);
      bennet_tagged_domain inner_dom = congr_to_tagged(&out, &inner->base_type);
      return bennet_congr_transform_backward(inner, target_sym, inner_dom, state);
    }

    default:
      /* Unknown term type: no safe refinement possible */
      return bennet_absint_state_copy_congr(state);
  }
}

/*-----------------------------------------------------------------------------
 * Backward Assume (congr)
 *---------------------------------------------------------------------------*/

bennet_absint_state* bennet_congr_transform_backward_assume(
    cn_term* term, bool value, bennet_absint_state* state) {
  if (!term || !state)
    return state;

  /* Handle NOT(expr) by recursing with flipped value */
  if (term->type == CN_TERM_UNOP && term->data.unop.op == CN_UNOP_NOT) {
    return bennet_congr_transform_backward_assume(term->data.unop.operand, !value, state);
  }

  /* Handle comparison operators */
  if (term->type == CN_TERM_BINOP) {
    cn_term* left = term->data.binop.left;
    cn_term* right = term->data.binop.right;
    cn_binop op = term->data.binop.op;

    switch (op) {
      case CN_BINOP_EQ: {
        if (value) {
          /* a == b must be true: meet both domains */
          bennet_tagged_domain left_td = bennet_congr_transform_forward(left, state);
          bennet_tagged_domain right_td = bennet_congr_transform_forward(right, state);
          congr_generic lg = congr_from_tagged(&left_td);
          congr_generic rg = congr_from_tagged(&right_td);
          congr_generic met = congr_generic_meet(&lg, &rg);

          if (met.is_bottom) {
            /* Unsatisfiable: propagate bottom to all syms */
            bennet_absint_state* bot_state = bennet_absint_state_copy_congr(state);
            bennet_absint_sym all_syms[16];
            int nl = congr_term_collect_syms(left, all_syms, 16);
            int nr = congr_term_collect_syms(right, all_syms + nl, 16 - nl);
            for (int i = 0; i < nl + nr; i++) {
              cn_base_type loc_bt = {.tag = CN_BASE_LOC};
              bot_state = bennet_absint_state_set_congr(
                  bot_state, all_syms[i], bennet_tagged_domain_bottom_congr(&loc_bt));
            }
            return bot_state;
          }

          /* Apply met to all syms in both sides */
          bennet_tagged_domain met_td;
          bennet_absint_state* result = bennet_absint_state_copy_congr(state);

          met_td = congr_to_tagged(
              &met, left->base_type.tag == CN_BASE_LOC ? &left->base_type : left_td.type);
          result = congr_backward_apply_to_all_syms(left, &met_td, result);

          met_td = congr_to_tagged(&met,
              right->base_type.tag == CN_BASE_LOC ? &right->base_type : right_td.type);
          result = congr_backward_apply_to_all_syms(right, &met_td, result);

          return result;
        }
        /* a != b: limited refinement in congruence domain, skip */
        return bennet_absint_state_copy_congr(state);
      }

      case CN_BINOP_AND: {
        if (value) {
          /* Both sides must be true */
          bennet_absint_state* result =
              bennet_congr_transform_backward_assume(left, true, state);
          return bennet_congr_transform_backward_assume(right, true, result);
        }
        return bennet_absint_state_copy_congr(state);
      }

      case CN_BINOP_OR: {
        if (!value) {
          /* Both sides must be false */
          bennet_absint_state* result =
              bennet_congr_transform_backward_assume(left, false, state);
          return bennet_congr_transform_backward_assume(right, false, result);
        }
        return bennet_absint_state_copy_congr(state);
      }

      default:
        /* LT, LE, etc.: congruence can't represent intervals, no refinement */
        return bennet_absint_state_copy_congr(state);
    }
  }

  return bennet_absint_state_copy_congr(state);
}
