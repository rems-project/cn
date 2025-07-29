#include <assert.h>
#include <stdbool.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domain.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/failure.h>
#include <cn-executable/utils.h>

#define RANGE_GEN(sm)                                                                    \
  uint##sm##_t bennet_range_uint##sm##_t_sized(uint##sm##_t min, uint##sm##_t max) {     \
    if (min == max) {                                                                    \
      return min;                                                                        \
    }                                                                                    \
                                                                                         \
    if (min == 0 && max == UINT##sm##_MAX) {                                             \
      return bennet_arbitrary_sized_top(uint##sm##_t);                                   \
    }                                                                                    \
                                                                                         \
    size_t sz = bennet_get_size();                                                       \
    size_t width = max - min + 1;                                                        \
    if (width > sz) {                                                                    \
      width = sz;                                                                        \
    }                                                                                    \
                                                                                         \
    return bennet_arbitrary_sized(uint##sm##_t, width) + min;                            \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_range_int##sm##_t_sized(int##sm##_t min, int##sm##_t max) {         \
    if (min == max) {                                                                    \
      return min;                                                                        \
    }                                                                                    \
                                                                                         \
    if (min == INT##sm##_MIN && max == INT##sm##_MAX) {                                  \
      return bennet_arbitrary_sized_top(int##sm##_t);                                    \
    }                                                                                    \
                                                                                         \
    min -= (max == INT##sm##_MAX);                                                       \
                                                                                         \
    int32_t sz = (int32_t)bennet_get_size();                                             \
                                                                                         \
    /* Shifts the range bounds to be centered around zero, */                            \
    /* but ensure `max - min` <= `2 * sz` */                                             \
    if (min <= -sz + 1) {                                                                \
      if (max >= sz) {                                                                   \
        min = -sz + 1;                                                                   \
        max = sz;                                                                        \
      } else {                                                                           \
        int32_t excess = (sz - max);                                                     \
        if (min < -sz + 1 - excess) {                                                    \
          min = -sz + 1 - excess;                                                        \
        }                                                                                \
      }                                                                                  \
    } else {                                                                             \
      int32_t excess = min - (-sz + 1);                                                  \
      if (max > sz + excess) {                                                           \
        max = sz + excess;                                                               \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    return bennet_arbitrary_sized(uint##sm##_t, max - min) + min +                       \
           (max == INT##sm##_MAX);                                                       \
  }

RANGE_GEN(8);
RANGE_GEN(16);
RANGE_GEN(32);
RANGE_GEN(64);

#define INEQ_GEN(sm)                                                                     \
  uint##sm##_t bennet_le_uint##sm##_t_sized(uint##sm##_t max) {                          \
    if (max == UINT##sm##_MAX) {                                                         \
      return bennet_arbitrary_sized_top(uint##sm##_t);                                   \
    }                                                                                    \
                                                                                         \
    return bennet_arbitrary_sized(uint##sm##_t, max + 1);                                \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_le_int##sm##_t_sized(int##sm##_t max) {                             \
    if (max == INT##sm##_MAX) {                                                          \
      return bennet_arbitrary_sized_top(int##sm##_t);                                    \
    }                                                                                    \
                                                                                         \
    return bennet_range_int##sm##_t_sized(INT##sm##_MIN, max);                           \
  }                                                                                      \
                                                                                         \
  uint##sm##_t bennet_ge_uint##sm##_t_sized(uint##sm##_t min) {                          \
    if (min == 0) {                                                                      \
      return bennet_arbitrary_sized_top(uint##sm##_t);                                   \
    }                                                                                    \
                                                                                         \
    return bennet_range_uint##sm##_t_sized(min, UINT##sm##_MAX);                         \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_ge_int##sm##_t_sized(int##sm##_t min) {                             \
    if (min == INT##sm##_MIN) {                                                          \
      return bennet_arbitrary_sized_top(int##sm##_t);                                    \
    }                                                                                    \
                                                                                         \
    return bennet_range_int##sm##_t_sized(min, INT##sm##_MAX);                           \
  }

INEQ_GEN(8);
INEQ_GEN(16);
INEQ_GEN(32);
INEQ_GEN(64);

#define MULT_RANGE_GEN(sm)                                                               \
  uint##sm##_t bennet_mult_range_uint##sm##_t_sized(                                     \
      uint##sm##_t mul, uint##sm##_t min, uint##sm##_t max) {                            \
    assert(mul != 0);                                                                    \
                                                                                         \
    if (mul == 1) {                                                                      \
      return bennet_range_uint##sm##_t_sized(min, max);                                  \
    }                                                                                    \
                                                                                         \
    uint##sm##_t x =                                                                     \
        bennet_range_uint##sm##_t_sized(min / mul, max / mul + (max % mul != 0));        \
    return x * mul;                                                                      \
  }                                                                                      \
                                                                                         \
  int##sm##_t bennet_mult_range_int##sm##_t_sized(                                       \
      int##sm##_t mul, int##sm##_t min, int##sm##_t max) {                               \
    assert(mul != 0);                                                                    \
                                                                                         \
    if (mul == 1) {                                                                      \
      return bennet_range_int##sm##_t_sized(min, max);                                   \
    }                                                                                    \
                                                                                         \
    int##sm##_t x =                                                                      \
        bennet_range_int##sm##_t_sized(min / mul, max / mul + (max % mul != 0));         \
                                                                                         \
    return x * mul;                                                                      \
  }

MULT_RANGE_GEN(8);
MULT_RANGE_GEN(16);
MULT_RANGE_GEN(32);
MULT_RANGE_GEN(64);

#define MULT_GEN(sm)                                                                     \
  uint##sm##_t bennet_mult_uint##sm##_t_sized(uint##sm##_t mul) {                        \
    return bennet_mult_range_uint##sm##_t_sized(mul, 0, UINT##sm##_MAX);                 \
  }                                                                                      \
  int##sm##_t bennet_mult_int##sm##_t_sized(int##sm##_t mul) {                           \
    return bennet_mult_range_int##sm##_t_sized(mul, INT##sm##_MIN, INT##sm##_MAX);       \
  }

MULT_GEN(8);
MULT_GEN(16);
MULT_GEN(32);
MULT_GEN(64);

#define BENNET_ARBITRARY_IMPL(cn_ty, c_ty, int_min, int_max)                             \
  cn_ty* bennet_arbitrary_##cn_ty(bennet_domain(c_ty) * cs) {                            \
    if (cs->is_owned) {                                                                  \
      bennet_domain(uintptr_t) pointer_cs = bennet_domain_cast(uintptr_t, cs);           \
      if (bennet_optional_is_some(cs->lower_bound_inc)) {                                \
        c_ty lower_bound = bennet_optional_unwrap(cs->lower_bound_inc);                  \
        pointer_cs.lower_bound_inc =                                                     \
            bennet_optional_some(uintptr_t, (lower_bound < 0) ? 0 : lower_bound);        \
      }                                                                                  \
                                                                                         \
      if (bennet_optional_is_some(cs->upper_bound_inc)) {                                \
        uintptr_t upper_bound = bennet_optional_unwrap(cs->upper_bound_inc);             \
        pointer_cs.upper_bound_inc =                                                     \
            bennet_optional_some(uintptr_t, (upper_bound < 0) ? 0 : upper_bound);        \
      }                                                                                  \
                                                                                         \
      return cast_cn_pointer_to_##cn_ty(bennet_alloc(&pointer_cs));                      \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->multiple)) {                                         \
      c_ty mult = bennet_optional_unwrap(cs->multiple);                                  \
      if (bennet_optional_is_some(cs->lower_bound_inc) ||                                \
          bennet_optional_is_some(cs->upper_bound_inc)) {                                \
        c_ty min = bennet_optional_unwrap_or(c_ty)(&cs->lower_bound_inc, int_min);       \
        c_ty max = bennet_optional_unwrap_or(c_ty)(&cs->upper_bound_inc, int_max);       \
        return convert_to_##cn_ty(bennet_mult_range_##c_ty##_sized(mult, min, max));     \
      }                                                                                  \
                                                                                         \
      return convert_to_##cn_ty(bennet_mult_##c_ty##_sized(mult));                       \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc) &&                                  \
        bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_##cn_ty(                                                         \
          bennet_range_##c_ty##_sized(bennet_optional_unwrap(cs->lower_bound_inc),       \
              bennet_optional_unwrap(cs->upper_bound_inc)));                             \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc)) {                                  \
      return convert_to_##cn_ty(                                                         \
          bennet_ge_##c_ty##_sized(bennet_optional_unwrap(cs->lower_bound_inc)));        \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_##cn_ty(                                                         \
          bennet_le_##c_ty##_sized(bennet_optional_unwrap(cs->upper_bound_inc)));        \
    }                                                                                    \
                                                                                         \
    return convert_to_##cn_ty(bennet_arbitrary_sized_top(c_ty));                         \
  }

#define BENNET_ARBITRARY_IMPL_BV(sz)                                                     \
  BENNET_ARBITRARY_IMPL(cn_bits_u##sz, uint##sz##_t, 0, UINT##sz##_MAX)                  \
  BENNET_ARBITRARY_IMPL(cn_bits_i##sz, int##sz##_t, INT##sz##_MIN, INT##sz##_MAX)

BENNET_ARBITRARY_IMPL_BV(8)
BENNET_ARBITRARY_IMPL_BV(16)
BENNET_ARBITRARY_IMPL_BV(32)
BENNET_ARBITRARY_IMPL_BV(64)

static uint8_t null_in_every = 5;

uint8_t get_null_in_every(void) {
  return null_in_every;
}

void set_null_in_every(uint8_t n) {
  null_in_every = n;
}

cn_pointer* bennet_arbitrary_cn_pointer(bennet_domain(uintptr_t) * cs) {
  // Only allocate
  if (cs->is_owned) {
    return bennet_alloc(cs);
  }

  if (bennet_optional_is_some(cs->multiple)) {
    uintptr_t mult = bennet_optional_unwrap(cs->multiple);
    if (bennet_optional_is_some(cs->lower_bound_inc) ||
        bennet_optional_is_some(cs->upper_bound_inc)) {
      uintptr_t min = bennet_optional_unwrap_or(uintptr_t)(&cs->lower_bound_inc, 0);
      uintptr_t max =
          bennet_optional_unwrap_or(uintptr_t)(&cs->upper_bound_inc, UINTPTR_MAX);
      return convert_to_cn_pointer(
          (void*)bennet_mult_range_uint64_t_sized(mult, min, max));
    }

    return convert_to_cn_pointer((void*)bennet_mult_uint64_t_sized(mult));
  }

  if (bennet_optional_is_some(cs->lower_bound_inc) &&
      bennet_optional_is_some(cs->upper_bound_inc)) {
    return convert_to_cn_pointer(
        (void*)bennet_range_uint64_t_sized(bennet_optional_unwrap(cs->lower_bound_inc),
            bennet_optional_unwrap(cs->upper_bound_inc)));
  }

  if (bennet_optional_is_some(cs->lower_bound_inc)) {
    return convert_to_cn_pointer(
        (void*)bennet_ge_uint64_t_sized(bennet_optional_unwrap(cs->lower_bound_inc)));
  }

  if (bennet_optional_is_some(cs->upper_bound_inc)) {
    return convert_to_cn_pointer(
        (void*)bennet_le_uint64_t_sized(bennet_optional_unwrap(cs->upper_bound_inc)));
  }

  // Weight towards `NULL` for pointers
  // TODO: Figure out general way for generators to learn that this is useful
  // TODO: OR make this unnecessary
  uint8_t rnd = bennet_uniform_uint8_t(null_in_every);
  if (rnd == 0) {
    return convert_to_cn_pointer(NULL);
  }

  return convert_to_cn_pointer((void*)bennet_arbitrary_sized_top(uint64_t));
}
