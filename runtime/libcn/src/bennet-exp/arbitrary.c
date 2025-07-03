#include <assert.h>
#include <stdbool.h>

#include <bennet-exp/alloc.h>
#include <bennet-exp/arbitrary.h>
#include <bennet-exp/domain.h>
#include <bennet-exp/failure.h>
#include <bennet-exp/rand.h>
#include <bennet-exp/size.h>
#include <cn-executable/utils.h>

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
        return convert_to_##cn_ty(bennet_mult_range_##c_ty(mult, min, max));             \
      }                                                                                  \
                                                                                         \
      return convert_to_##cn_ty(bennet_mult_##c_ty(mult));                               \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc) &&                                  \
        bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_##cn_ty(                                                         \
          bennet_range_##c_ty(bennet_optional_unwrap(cs->lower_bound_inc),               \
              bennet_optional_unwrap(cs->upper_bound_inc)));                             \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->lower_bound_inc)) {                                  \
      return convert_to_##cn_ty(                                                         \
          bennet_ge_##c_ty(bennet_optional_unwrap(cs->lower_bound_inc)));                \
    }                                                                                    \
                                                                                         \
    if (bennet_optional_is_some(cs->upper_bound_inc)) {                                  \
      return convert_to_##cn_ty(                                                         \
          bennet_le_##c_ty(bennet_optional_unwrap(cs->upper_bound_inc)));                \
    }                                                                                    \
                                                                                         \
    return convert_to_##cn_ty(bennet_uniform_##c_ty##_sized(0));                         \
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
      return convert_to_cn_pointer((void*)bennet_mult_range_uint64_t(mult, min, max));
    }

    return convert_to_cn_pointer((void*)bennet_mult_uint64_t(mult));
  }

  if (bennet_optional_is_some(cs->lower_bound_inc) &&
      bennet_optional_is_some(cs->upper_bound_inc)) {
    return convert_to_cn_pointer(
        (void*)bennet_range_uint64_t(bennet_optional_unwrap(cs->lower_bound_inc),
            bennet_optional_unwrap(cs->upper_bound_inc)));
  }

  if (bennet_optional_is_some(cs->lower_bound_inc)) {
    return convert_to_cn_pointer(
        (void*)bennet_ge_uint64_t(bennet_optional_unwrap(cs->lower_bound_inc)));
  }

  if (bennet_optional_is_some(cs->upper_bound_inc)) {
    return convert_to_cn_pointer(
        (void*)bennet_le_uint64_t(bennet_optional_unwrap(cs->upper_bound_inc)));
  }

  // Weight towards `NULL` for pointers
  // TODO: Figure out general way for generators to learn that this useful
  // TODO: OR make this unnecessary
  uint8_t rnd = bennet_uniform_uint8_t(null_in_every);
  if (rnd == 0) {
    return convert_to_cn_pointer(NULL);
  }

  return convert_to_cn_pointer((void*)bennet_uniform_uint64_t_sized(0));
}
