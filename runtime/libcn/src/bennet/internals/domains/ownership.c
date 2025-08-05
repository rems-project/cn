#include <assert.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/rand_alloc.h>

#define OWNERSHIP_GEN(cty)                                                               \
  cty bennet_arbitrary_ownership_##cty(bennet_domain_ownership(cty) * d) {               \
    assert(!d->bottom);                                                                  \
                                                                                         \
    if (d->before != 0 && d->after != 0) {                                               \
      void* p = bennet_rand_alloc(d->before + d->after);                                 \
      return (cty)((uintptr_t)p + d->before);                                            \
    }                                                                                    \
                                                                                         \
    return bennet_arbitrary_sized(cty, 0);                                               \
  }

OWNERSHIP_GEN(uint8_t);
OWNERSHIP_GEN(uint16_t);
OWNERSHIP_GEN(uint32_t);
OWNERSHIP_GEN(uint64_t);

OWNERSHIP_GEN(int8_t);
OWNERSHIP_GEN(int16_t);
OWNERSHIP_GEN(int32_t);
OWNERSHIP_GEN(int64_t);

uintptr_t bennet_arbitrary_ownership_uintptr_t(bennet_domain_ownership(uintptr_t) * d) {
  assert(!d->bottom);

  if (d->before != 0 && d->after != 0) {
    size_t bytes = d->before + d->after;
    void* p = bennet_rand_alloc(bytes);
    bennet_alloc_record(p, bytes);

    return (uintptr_t)((uintptr_t)p + d->before);
  }

  // Weight towards `NULL` for pointers
  // TODO: Figure out general way for generators to learn that this is useful
  // TODO: OR make this unnecessary
  uint8_t rnd = bennet_uniform_uint8_t(get_null_in_every());
  if (rnd == 0) {
    return (uintptr_t)NULL;
  }

  return bennet_arbitrary_sized(uintptr_t, 0);
};

#define OWNERSHIP_GEN_CN(cn_ty, c_ty)                                                    \
  cn_ty* bennet_arbitrary_ownership_##cn_ty(bennet_domain_ownership(c_ty) * d) {         \
    return convert_to_##cn_ty(bennet_arbitrary_ownership_##c_ty(d));                     \
  }

OWNERSHIP_GEN_CN(cn_bits_i8, int8_t)
OWNERSHIP_GEN_CN(cn_bits_i16, int16_t)
OWNERSHIP_GEN_CN(cn_bits_i32, int32_t)
OWNERSHIP_GEN_CN(cn_bits_i64, int64_t)

OWNERSHIP_GEN_CN(cn_bits_u8, uint8_t)
OWNERSHIP_GEN_CN(cn_bits_u16, uint16_t)
OWNERSHIP_GEN_CN(cn_bits_u32, uint32_t)
OWNERSHIP_GEN_CN(cn_bits_u64, uint64_t)

cn_pointer* bennet_arbitrary_ownership_cn_pointer(
    bennet_domain_ownership(uintptr_t) * d) {
  return convert_to_cn_pointer((void*)bennet_arbitrary_ownership_uintptr_t(d));
}
