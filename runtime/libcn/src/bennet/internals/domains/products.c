#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domains/products.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils.h>
#include <cn-executable/utils.h>

/* NULL bias for pointer-typed samplers: with probability
 * 1/get_null_in_every(), return NULL when `check_zero_expr` says the domains
 * admit 0. Passed as the hook argument of the sampler IMPL macros for their
 * uintptr_t instantiations (narrow types pass no hook).
 */
#define BENNET_PRODUCT_NULL_BIAS(check_zero_expr)                                        \
  do {                                                                                   \
    if (check_zero_expr) {                                                               \
      uint8_t bennet_product_null_rnd = bennet_uniform_uint8_t(get_null_in_every());     \
      if (bennet_product_null_rnd == 0) {                                                \
        return (uintptr_t)NULL;                                                          \
      }                                                                                  \
    }                                                                                    \
  } while (0)

#define BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(cty, ...)                                     \
  cty bennet_domain_ownership_wint_arbitrary_##cty(                                      \
      bennet_domain_ownership(cty) * d1, bennet_domain_wint(cty) * d2) {                 \
    assert(!d1->bottom && !d2->bottom);                                                  \
                                                                                         \
    /* Only allocate */                                                                  \
    if (d1->before != 0 || d1->after != 0) {                                             \
      size_t bytes = d1->before + d1->after;                                             \
      if (bytes < d1->before || bytes < d1->after) {                                     \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      void* p = (d2->top) ? bennet_alloc(bytes)                                          \
                          : bennet_alloc_bounded(                                        \
                                bytes, d2->start - d1->before, d2->end - d1->before);    \
                                                                                         \
      return (cty)((uintptr_t)p + d1->before);                                           \
    }                                                                                    \
                                                                                         \
    __VA_ARGS__                                                                          \
                                                                                         \
    return bennet_arbitrary_wint(cty, d2);                                               \
  }

BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(int64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint8_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint16_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint32_t)
BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(uint64_t)

BENNET_DOMAIN_PRODUCT_BUILTIN_IMPL(
    uintptr_t, BENNET_PRODUCT_NULL_BIAS(bennet_domain_wint_check_uintptr_t(0, d2));)

#define BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(cty)                                    \
  void bennet_domain_ownership_wint_reduce_##cty(                                        \
      bennet_domain_ownership(cty) * ownership, bennet_domain_wint(cty) * wint) {        \
    /* Top ownership = no allocation needed, nothing to constrain */                     \
    if (ownership->bottom || (ownership->before == 0 && ownership->after == 0))          \
      return;                                                                            \
    if (wint->bottom)                                                                    \
      return;                                                                            \
                                                                                         \
    size_t bytes = ownership->before + ownership->after;                                 \
    if (bytes < ownership->before || bytes < ownership->after) {                         \
      wint->bottom = true;                                                               \
      return;                                                                            \
    }                                                                                    \
                                                                                         \
    uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                        \
    uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                        \
    size_t buffer_size = alloc_max - alloc_min + 1;                                      \
                                                                                         \
    /* Allocation must fit in buffer */                                                  \
    if (bytes > buffer_size) {                                                           \
      wint->bottom = true;                                                               \
      return;                                                                            \
    }                                                                                    \
                                                                                         \
    /* Effective pointer range: [alloc_min + before, alloc_max - after + 1] */           \
    uintptr_t effective_min = alloc_min + ownership->before;                             \
    uintptr_t effective_max = alloc_max - ownership->after + 1;                          \
                                                                                         \
    /* For pointer-width types, intersect wint with effective range */                   \
    if (sizeof(cty) >= sizeof(uintptr_t)) {                                              \
      if (wint->top) {                                                                   \
        wint->start = (cty)effective_min;                                                \
        wint->end = (cty)effective_max;                                                  \
        wint->top = false;                                                               \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      uintptr_t ws = (uintptr_t)wint->start;                                             \
      uintptr_t we = (uintptr_t)wint->end;                                               \
                                                                                         \
      /* Skip if wrapping interval (signed negative to positive) */                      \
      if (ws <= we) {                                                                    \
        if (we < effective_min || ws > effective_max) {                                  \
          wint->bottom = true;                                                           \
          return;                                                                        \
        }                                                                                \
        if (ws < effective_min)                                                          \
          wint->start = (cty)effective_min;                                              \
        if (we > effective_max)                                                          \
          wint->end = (cty)effective_max;                                                \
      }                                                                                  \
    }                                                                                    \
    /* For narrow types, bytes-in-buffer check above suffices since many */              \
    /* buffer positions map to the same narrow value via truncation     */               \
  }

BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(int8_t)
BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(int16_t)
BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(int32_t)
BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(int64_t)

BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(uint8_t)
BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(uint16_t)
BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(uint32_t)
BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(uint64_t)

BENNET_DOMAIN_OWNERSHIP_WINT_REDUCE_IMPL(uintptr_t)

/*---------------------------------------------------------------------------
 * congr_ownership combined arbitrary
 *
 * Alphabetical order: congr before ownership.
 * When ownership needs allocation, pick an allocation base such that
 * (base + before) satisfies the congruence constraint.
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(cty, ...)                           \
  cty bennet_domain_congr_ownership_arbitrary_##cty(                                     \
      bennet_domain_congr(cty) * congr, bennet_domain_ownership(cty) * own) {            \
    assert(!congr->bottom && !own->bottom);                                              \
                                                                                         \
    /* Ownership top = no allocation needed, delegate to congr */                        \
    if (own->before == 0 && own->after == 0) {                                           \
      __VA_ARGS__                                                                        \
      return bennet_domain_congr_arbitrary_##cty(congr);                                 \
    }                                                                                    \
                                                                                         \
    size_t bytes = own->before + own->after;                                             \
    if (bytes < own->before || bytes < own->after) {                                     \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    /* Congr top = any value works, just allocate */                                     \
    if (congr->top) {                                                                    \
      void* p = bennet_rand_alloc(bytes);                                                \
      if (!p) {                                                                          \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
      bennet_alloc_record(p, bytes);                                                     \
      return (cty)((uintptr_t)p + own->before);                                          \
    }                                                                                    \
                                                                                         \
    /* Both constrained: for pointer-width types, find valid positions */                \
    if (sizeof(cty) >= sizeof(uintptr_t)) {                                              \
      uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                      \
      uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                      \
      size_t buffer_size = alloc_max - alloc_min + 1;                                    \
                                                                                         \
      if (bytes > buffer_size) {                                                         \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      /* Effective pointer range */                                                      \
      uintptr_t eff_min = alloc_min + own->before;                                       \
      uintptr_t eff_max = alloc_max - own->after + 1;                                    \
                                                                                         \
      uintptr_t m = (uintptr_t)(cty)congr->modulus;                                      \
      uintptr_t r = (uintptr_t)(cty)congr->residue;                                      \
                                                                                         \
      /* Singleton: congr modulus == 0 means exactly {residue} */                        \
      if (m == 0) {                                                                      \
        if (r < eff_min || r > eff_max) {                                                \
          cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                   \
        }                                                                                \
        bennet_alloc_record((void*)(r - own->before), bytes);                            \
        return (cty)r;                                                                   \
      }                                                                                  \
                                                                                         \
      /* modulus is power-of-2: find first valid pointer >= eff_min */                   \
      uintptr_t mask = m - 1;                                                            \
      uintptr_t offset = (r - eff_min) & mask;                                           \
      uintptr_t first_valid = eff_min + offset;                                          \
                                                                                         \
      if (first_valid > eff_max) {                                                       \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      uintptr_t count = (eff_max - first_valid) / m + 1;                                 \
      size_t sz = bennet_get_size();                                                     \
      uintptr_t max_idx = count - 1;                                                     \
      if (max_idx > sz) {                                                                \
        max_idx = (uintptr_t)sz;                                                         \
      }                                                                                  \
                                                                                         \
      uintptr_t idx = bennet_range_uint64_t(0, max_idx);                                 \
      uintptr_t chosen = first_valid + idx * m;                                          \
                                                                                         \
      bennet_alloc_record((void*)(chosen - own->before), bytes);                         \
      return (cty)chosen;                                                                \
    }                                                                                    \
                                                                                         \
    /* Narrow types: allocate then generate congr value */                               \
    void* p = bennet_rand_alloc(bytes);                                                  \
    if (!p) {                                                                            \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
    bennet_alloc_record(p, bytes);                                                       \
    return bennet_domain_congr_arbitrary_##cty(congr);                                   \
  }

BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_ARBITRARY_IMPL(
    uintptr_t, BENNET_PRODUCT_NULL_BIAS(bennet_domain_congr_check_uintptr_t(0, congr));)

/*---------------------------------------------------------------------------
 * congr_ownership reduce
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(cty)                                   \
  void bennet_domain_congr_ownership_reduce_##cty(                                       \
      bennet_domain_congr(cty) * congr, bennet_domain_ownership(cty) * own) {            \
    if (own->bottom || (own->before == 0 && own->after == 0))                            \
      return;                                                                            \
    if (congr->bottom)                                                                   \
      return;                                                                            \
                                                                                         \
    size_t bytes = own->before + own->after;                                             \
    if (bytes < own->before || bytes < own->after) {                                     \
      congr->bottom = true;                                                              \
      return;                                                                            \
    }                                                                                    \
                                                                                         \
    if (sizeof(cty) >= sizeof(uintptr_t)) {                                              \
      uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                      \
      uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                      \
      size_t buffer_size = alloc_max - alloc_min + 1;                                    \
                                                                                         \
      if (bytes > buffer_size) {                                                         \
        congr->bottom = true;                                                            \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      uintptr_t eff_min = alloc_min + own->before;                                       \
      uintptr_t eff_max = alloc_max - own->after + 1;                                    \
                                                                                         \
      /* Meet congr with interval-derived congr */                                       \
      bennet_domain_congr(cty)* interval_congr =                                         \
          bennet_domain_congr_of_interval_##cty((cty)eff_min, (cty)eff_max);             \
      bennet_domain_congr(cty)* met =                                                    \
          bennet_domain_congr_meet_##cty(congr, interval_congr);                         \
      *congr = *met;                                                                     \
    }                                                                                    \
  }

BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_REDUCE_IMPL(uintptr_t)

/*---------------------------------------------------------------------------
 * ownership_tnum combined arbitrary
 *
 * Alphabetical order: ownership before tnum.
 * When ownership needs allocation, convert effective ptr range to tnum,
 * meet with given tnum, then generate from the result.
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(cty, ...)                            \
  cty bennet_domain_ownership_tnum_arbitrary_##cty(                                      \
      bennet_domain_ownership(cty) * own, bennet_domain_tnum(cty) * tnum) {              \
    assert(!own->bottom && !tnum->bottom);                                               \
                                                                                         \
    /* Ownership top = no allocation needed, delegate to tnum */                         \
    if (own->before == 0 && own->after == 0) {                                           \
      __VA_ARGS__                                                                        \
      return bennet_domain_tnum_arbitrary_##cty(tnum);                                   \
    }                                                                                    \
                                                                                         \
    size_t bytes = own->before + own->after;                                             \
    if (bytes < own->before || bytes < own->after) {                                     \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    /* Tnum top = any value works, just allocate */                                      \
    if (tnum->top) {                                                                     \
      void* p = bennet_rand_alloc(bytes);                                                \
      if (!p) {                                                                          \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
      bennet_alloc_record(p, bytes);                                                     \
      return (cty)((uintptr_t)p + own->before);                                          \
    }                                                                                    \
                                                                                         \
    /* Both constrained: allocate randomly, check tnum, retry */                         \
    if (sizeof(cty) >= sizeof(uintptr_t)) {                                              \
      const int max_attempts = 100;                                                      \
      for (int attempt = 0; attempt < max_attempts; ++attempt) {                         \
        void* p = bennet_rand_alloc(bytes);                                              \
        if (!p) {                                                                        \
          cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                   \
        }                                                                                \
        cty val = (cty)((uintptr_t)p + own->before);                                     \
        if (bennet_domain_tnum_check_##cty(val, tnum)) {                                 \
          bennet_alloc_record(p, bytes);                                                 \
          return val;                                                                    \
        }                                                                                \
      }                                                                                  \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    /* Narrow types: allocate then generate tnum value */                                \
    void* p = bennet_rand_alloc(bytes);                                                  \
    if (!p) {                                                                            \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
    bennet_alloc_record(p, bytes);                                                       \
    return bennet_domain_tnum_arbitrary_##cty(tnum);                                     \
  }

BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(int8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(int16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(int32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(int64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(uint8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(uint16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(uint32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(uint64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_ARBITRARY_IMPL(
    uintptr_t, BENNET_PRODUCT_NULL_BIAS(bennet_domain_tnum_check_uintptr_t(0, tnum));)

/*---------------------------------------------------------------------------
 * ownership_tnum reduce
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(cty)                                    \
  void bennet_domain_ownership_tnum_reduce_##cty(                                        \
      bennet_domain_ownership(cty) * own, bennet_domain_tnum(cty) * tnum) {              \
    if (own->bottom || (own->before == 0 && own->after == 0))                            \
      return;                                                                            \
    if (tnum->bottom)                                                                    \
      return;                                                                            \
                                                                                         \
    size_t bytes = own->before + own->after;                                             \
    if (bytes < own->before || bytes < own->after) {                                     \
      tnum->bottom = true;                                                               \
      return;                                                                            \
    }                                                                                    \
                                                                                         \
    if (sizeof(cty) >= sizeof(uintptr_t)) {                                              \
      uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                      \
      uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                      \
      size_t buffer_size = alloc_max - alloc_min + 1;                                    \
                                                                                         \
      if (bytes > buffer_size) {                                                         \
        tnum->bottom = true;                                                             \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      uintptr_t eff_min = alloc_min + own->before;                                       \
      uintptr_t eff_max = alloc_max - own->after + 1;                                    \
                                                                                         \
      /* Meet tnum with interval-derived tnum */                                         \
      bennet_domain_tnum(cty)* interval_tnum =                                           \
          bennet_domain_tnum_of_interval_##cty((cty)eff_min, (cty)eff_max);              \
      bennet_domain_tnum(cty)* met = bennet_domain_tnum_meet_##cty(tnum, interval_tnum); \
      *tnum = *met;                                                                      \
    }                                                                                    \
  }

BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(int8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(int16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(int32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(int64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(uint8_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(uint16_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(uint32_t)
BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(uint64_t)

BENNET_DOMAIN_OWNERSHIP_TNUM_REDUCE_IMPL(uintptr_t)

/*---------------------------------------------------------------------------
 * congr_wint combined arbitrary
 *
 * Alphabetical order: congr before wint.
 * Generates a value satisfying both congruence and wrapped-interval constraints.
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_CONGR_WINT_ARBITRARY_UNSIGNED_IMPL(cty, ...)                       \
  cty bennet_domain_congr_wint_arbitrary_##cty(                                          \
      bennet_domain_congr(cty) * congr, bennet_domain_wint(cty) * wint) {                \
    assert(!congr->bottom && !wint->bottom);                                             \
                                                                                         \
    __VA_ARGS__                                                                          \
                                                                                         \
    if (congr->top)                                                                      \
      return bennet_arbitrary_wint(cty, wint);                                           \
    if (wint->top)                                                                       \
      return bennet_arbitrary_congr_##cty(congr);                                        \
                                                                                         \
    cty m = congr->modulus;                                                              \
    cty r = congr->residue;                                                              \
    cty start = wint->start;                                                             \
    cty end = wint->end;                                                                 \
                                                                                         \
    /* Singleton congr */                                                                \
    if (m == 0) {                                                                        \
      if (r >= start && r <= end)                                                        \
        return r;                                                                        \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    cty mask = m - 1;                                                                    \
    cty first_valid = start + ((r - start) & mask);                                      \
    if (first_valid > end) {                                                             \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    cty count = (end - first_valid) / m + 1;                                             \
    cty max_idx = count - 1;                                                             \
    size_t sz = bennet_get_size();                                                       \
    if (max_idx > (cty)sz)                                                               \
      max_idx = (cty)sz;                                                                 \
                                                                                         \
    cty idx = bennet_arbitrary_wint_of(cty, 0, max_idx);                                 \
    return first_valid + idx * m;                                                        \
  }

#define BENNET_DOMAIN_CONGR_WINT_ARBITRARY_SIGNED_IMPL(sm)                               \
  int##sm##_t bennet_domain_congr_wint_arbitrary_int##sm##_t(                            \
      bennet_domain_congr(int##sm##_t) * congr,                                          \
      bennet_domain_wint(int##sm##_t) * wint) {                                          \
    assert(!congr->bottom && !wint->bottom);                                             \
                                                                                         \
    if (congr->top)                                                                      \
      return bennet_arbitrary_wint(int##sm##_t, wint);                                   \
    if (wint->top)                                                                       \
      return bennet_arbitrary_congr_int##sm##_t(congr);                                  \
                                                                                         \
    uint##sm##_t m = (uint##sm##_t)congr->modulus;                                       \
    uint##sm##_t r = (uint##sm##_t)congr->residue;                                       \
    int##sm##_t start = wint->start;                                                     \
    int##sm##_t end = wint->end;                                                         \
                                                                                         \
    /* Singleton congr */                                                                \
    if (m == 0) {                                                                        \
      int##sm##_t val = (int##sm##_t)r;                                                  \
      if (val >= start && val <= end)                                                    \
        return val;                                                                      \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    uint##sm##_t mask = m - 1;                                                           \
                                                                                         \
    /* Same sign: contiguous in unsigned space */                                        \
    if (start >= 0 || end < 0) {                                                         \
      uint##sm##_t s = (uint##sm##_t)start;                                              \
      uint##sm##_t e = (uint##sm##_t)end;                                                \
      uint##sm##_t first_valid = s + ((r - s) & mask);                                   \
      if (first_valid > e) {                                                             \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
      uint##sm##_t count = (e - first_valid) / m + 1;                                    \
      uint##sm##_t max_idx = count - 1;                                                  \
      size_t sz = bennet_get_size();                                                     \
      if (max_idx > (uint##sm##_t)sz)                                                    \
        max_idx = (uint##sm##_t)sz;                                                      \
      uint##sm##_t idx = bennet_arbitrary_wint_of(uint##sm##_t, 0, max_idx);             \
      return (int##sm##_t)(first_valid + idx * m);                                       \
    }                                                                                    \
                                                                                         \
    /* Crosses zero: positive [0, end] and negative [start, -1] */                       \
    uint##sm##_t end_u = (uint##sm##_t)end;                                              \
    uint##sm##_t start_u = (uint##sm##_t)start;                                          \
    uint##sm##_t fm = UINT##sm##_MAX;                                                    \
                                                                                         \
    /* Positive range: congr-valid in [0, end_u] */                                      \
    size_t num_pos = 0;                                                                  \
    uint##sm##_t first_pos = r; /* r < m after xi-norm, so r >= 0 */                     \
    if (first_pos <= end_u) {                                                            \
      num_pos = (size_t)((end_u - first_pos) / m) + 1;                                   \
    }                                                                                    \
                                                                                         \
    /* Negative range: congr-valid in [start_u, UINT_MAX] */                             \
    size_t num_neg = 0;                                                                  \
    uint##sm##_t first_neg = start_u + ((r - start_u) & mask);                           \
    if (first_neg >= start_u) { /* check no overflow */                                  \
      num_neg = (size_t)((fm - first_neg) / m) + 1;                                      \
    }                                                                                    \
                                                                                         \
    if (num_pos == 0 && num_neg == 0) {                                                  \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
    size_t capped_pos = (num_pos > sz + 1) ? sz + 1 : num_pos;                           \
    size_t capped_neg = (num_neg > sz) ? sz : num_neg;                                   \
    size_t total = capped_pos + capped_neg;                                              \
                                                                                         \
    uint64_t choice = bennet_arbitrary_wint_of(uint64_t, 0, (uint64_t)(total - 1));      \
    if (choice < capped_pos) {                                                           \
      return (int##sm##_t)(uint##sm##_t)(first_pos + (uint##sm##_t)choice * m);          \
    }                                                                                    \
    uint64_t neg_idx = choice - capped_pos;                                              \
    uint##sm##_t last_neg = (uint##sm##_t)(first_neg + (uint##sm##_t)(num_neg - 1) * m); \
    return (int##sm##_t)(uint##sm##_t)(last_neg - (uint##sm##_t)neg_idx * m);            \
  }

BENNET_DOMAIN_CONGR_WINT_ARBITRARY_UNSIGNED_IMPL(uint8_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_UNSIGNED_IMPL(uint16_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_UNSIGNED_IMPL(uint32_t)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_UNSIGNED_IMPL(uint64_t)

BENNET_DOMAIN_CONGR_WINT_ARBITRARY_UNSIGNED_IMPL(uintptr_t,
    BENNET_PRODUCT_NULL_BIAS(bennet_domain_congr_check_uintptr_t(0, congr) &&
                             bennet_domain_wint_check_uintptr_t(0, wint));)

BENNET_DOMAIN_CONGR_WINT_ARBITRARY_SIGNED_IMPL(8)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_SIGNED_IMPL(16)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_SIGNED_IMPL(32)
BENNET_DOMAIN_CONGR_WINT_ARBITRARY_SIGNED_IMPL(64)

/*---------------------------------------------------------------------------
 * congr_wint reduce
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(cty)                                        \
  void bennet_domain_congr_wint_reduce_##cty(                                            \
      bennet_domain_congr(cty) * congr, bennet_domain_wint(cty) * wint) {                \
    if (congr->bottom || wint->bottom)                                                   \
      return;                                                                            \
    if (congr->top && wint->top)                                                         \
      return;                                                                            \
                                                                                         \
    for (int iter = 0; iter < 2; iter++) {                                               \
      /* wint -> congr: narrow congr using wint's interval bounds. Only for  */          \
      /* proper (non-wrapping) intervals: to_interval rejects wrapped wints, */          \
      /* whose raw start/end are not ordered bounds.                         */          \
      cty _r_wlo, _r_whi;                                                                \
      if (!congr->bottom &&                                                              \
          bennet_domain_wint_to_interval_##cty(wint, &_r_wlo, &_r_whi)) {                \
        bennet_domain_congr(cty)* ic =                                                   \
            bennet_domain_congr_of_interval_##cty(_r_wlo, _r_whi);                       \
        bennet_domain_congr(cty)* met = bennet_domain_congr_meet_##cty(congr, ic);       \
        *congr = *met;                                                                   \
        if (congr->bottom) {                                                             \
          wint->bottom = true;                                                           \
          return;                                                                        \
        }                                                                                \
      }                                                                                  \
                                                                                         \
      /* congr -> wint: narrow wint using congr's implied interval */                    \
      if (!congr->top && !wint->bottom) {                                                \
        cty lo, hi;                                                                      \
        if (bennet_domain_congr_to_interval_##cty(congr, &lo, &hi)) {                    \
          if (wint->top) {                                                               \
            wint->start = lo;                                                            \
            wint->end = hi;                                                              \
            wint->top = false;                                                           \
          } else {                                                                       \
            bennet_domain_wint(cty)* iw = bennet_domain_wint_of_##cty(lo, hi);           \
            bennet_domain_wint(cty)* met = bennet_domain_wint_meet_##cty(wint, iw);      \
            *wint = *met;                                                                \
            if (wint->bottom) {                                                          \
              congr->bottom = true;                                                      \
              return;                                                                    \
            }                                                                            \
          }                                                                              \
        }                                                                                \
      }                                                                                  \
    }                                                                                    \
  }

BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(int8_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(int16_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(int32_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(int64_t)

BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(uint8_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(uint16_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(uint32_t)
BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(uint64_t)

BENNET_DOMAIN_CONGR_WINT_REDUCE_IMPL(uintptr_t)

/*---------------------------------------------------------------------------
 * congr_ownership_wint combined arbitrary
 *
 * Alphabetical order: congr, ownership, wint.
 * When ownership needs allocation, narrow wint with ownership's effective
 * range, then use congr_wint_arbitrary to pick a value.
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(cty)                           \
  cty bennet_domain_congr_ownership_wint_arbitrary_##cty(                                \
      bennet_domain_congr(cty) * congr,                                                  \
      bennet_domain_ownership(cty) * own,                                                \
      bennet_domain_wint(cty) * wint) {                                                  \
    assert(!congr->bottom && !own->bottom && !wint->bottom);                             \
                                                                                         \
    /* Ownership top = no allocation needed, delegate to congr_wint */                   \
    if (own->before == 0 && own->after == 0) {                                           \
      return bennet_domain_congr_wint_arbitrary_##cty(congr, wint);                      \
    }                                                                                    \
                                                                                         \
    size_t bytes = own->before + own->after;                                             \
    if (bytes < own->before || bytes < own->after) {                                     \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
                                                                                         \
    /* Both congr and wint are top: just allocate */                                     \
    if (congr->top && wint->top) {                                                       \
      void* p = bennet_rand_alloc(bytes);                                                \
      if (!p) {                                                                          \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
      bennet_alloc_record(p, bytes);                                                     \
      return (cty)((uintptr_t)p + own->before);                                          \
    }                                                                                    \
                                                                                         \
    /* For pointer-width types: narrow wint with ownership range */                      \
    if (sizeof(cty) >= sizeof(uintptr_t)) {                                              \
      uintptr_t alloc_min = (uintptr_t)bennet_rand_alloc_min_ptr();                      \
      uintptr_t alloc_max = (uintptr_t)bennet_rand_alloc_max_ptr();                      \
      size_t buffer_size = alloc_max - alloc_min + 1;                                    \
                                                                                         \
      if (bytes > buffer_size) {                                                         \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      uintptr_t eff_min = alloc_min + own->before;                                       \
      uintptr_t eff_max = alloc_max - own->after + 1;                                    \
                                                                                         \
      /* Create narrowed wint from ownership range */                                    \
      bennet_domain_wint(cty) narrowed;                                                  \
      if (wint->top) {                                                                   \
        narrowed = (bennet_domain_wint(cty)){                                            \
            .top = false, .bottom = false, .start = (cty)eff_min, .end = (cty)eff_max};  \
      } else {                                                                           \
        bennet_domain_wint(cty)* own_wint =                                              \
            bennet_domain_wint_of_##cty((cty)eff_min, (cty)eff_max);                     \
        bennet_domain_wint(cty)* met = bennet_domain_wint_meet_##cty(wint, own_wint);    \
        narrowed = *met;                                                                 \
      }                                                                                  \
                                                                                         \
      if (narrowed.bottom) {                                                             \
        cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                     \
      }                                                                                  \
                                                                                         \
      /* Congr top: allocate within the narrowed range so the pointer keeps    */        \
      /* allocator alignment - congr_wint's byte-granular value stepping would */        \
      /* produce misaligned struct pointers.                                   */        \
      if (congr->top) {                                                                  \
        void* p = bennet_alloc_bounded(bytes,                                            \
            (uintptr_t)narrowed.start - own->before,                                     \
            (uintptr_t)narrowed.end - own->before);                                      \
        return (cty)((uintptr_t)p + own->before);                                        \
      }                                                                                  \
                                                                                         \
      cty chosen = bennet_domain_congr_wint_arbitrary_##cty(congr, &narrowed);           \
      bennet_alloc_record((void*)((uintptr_t)chosen - own->before), bytes);              \
      return chosen;                                                                     \
    }                                                                                    \
                                                                                         \
    /* Narrow types: allocate then generate from congr_wint */                           \
    void* p = bennet_rand_alloc(bytes);                                                  \
    if (!p) {                                                                            \
      cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);                                       \
    }                                                                                    \
    bennet_alloc_record(p, bytes);                                                       \
    return bennet_domain_congr_wint_arbitrary_##cty(congr, wint);                        \
  }

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(uint64_t)

/* No NULL-bias hook: the ownership-top branch delegates to the congr_wint
 * sampler, which applies the bias itself at uintptr_t. */
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_ARBITRARY_IMPL(uintptr_t)

/*---------------------------------------------------------------------------
 * congr_ownership_wint reduce
 *
 * Chains existing two-domain reduces for fixpoint convergence.
 *---------------------------------------------------------------------------*/

#define BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(cty)                              \
  void bennet_domain_congr_ownership_wint_reduce_##cty(bennet_domain_congr(cty) * congr, \
      bennet_domain_ownership(cty) * own,                                                \
      bennet_domain_wint(cty) * wint) {                                                  \
    if (congr->bottom || own->bottom || wint->bottom)                                    \
      return;                                                                            \
                                                                                         \
    /* Ownership top: delegate to congr_wint reduce */                                   \
    if (own->before == 0 && own->after == 0) {                                           \
      bennet_domain_congr_wint_reduce_##cty(congr, wint);                                \
      return;                                                                            \
    }                                                                                    \
                                                                                         \
    /* Iterate two-domain reduces for fixpoint */                                        \
    for (int iter = 0; iter < 2; iter++) {                                               \
      bennet_domain_ownership_wint_reduce_##cty(own, wint);                              \
      if (wint->bottom) {                                                                \
        congr->bottom = true;                                                            \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      bennet_domain_congr_wint_reduce_##cty(congr, wint);                                \
      if (congr->bottom || wint->bottom) {                                               \
        congr->bottom = true;                                                            \
        wint->bottom = true;                                                             \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      bennet_domain_congr_ownership_reduce_##cty(congr, own);                            \
      if (congr->bottom) {                                                               \
        wint->bottom = true;                                                             \
        return;                                                                          \
      }                                                                                  \
    }                                                                                    \
  }

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(int8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(int16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(int32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(int64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(uint8_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(uint16_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(uint32_t)
BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(uint64_t)

BENNET_DOMAIN_CONGR_OWNERSHIP_WINT_REDUCE_IMPL(uintptr_t)
