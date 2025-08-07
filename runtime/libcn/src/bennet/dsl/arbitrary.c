#include <assert.h>
#include <stdbool.h>

#include <bennet/dsl/arbitrary.h>
#include <bennet/internals/domain.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/failure.h>
#include <cn-executable/utils.h>

#define BENNET_ARBITRARY_IMPL(cn_ty, c_ty)                                               \
  cn_ty* bennet_arbitrary_##cn_ty(bennet_domain(c_ty) * cs) {                            \
    return convert_to_##cn_ty(bennet_domain_arbitrary(c_ty, cs));                        \
  }

#define BENNET_ARBITRARY_IMPL_BV(sz)                                                     \
  BENNET_ARBITRARY_IMPL(cn_bits_u##sz, uint##sz##_t)                                     \
  BENNET_ARBITRARY_IMPL(cn_bits_i##sz, int##sz##_t)

BENNET_ARBITRARY_IMPL_BV(8)
BENNET_ARBITRARY_IMPL_BV(16)
BENNET_ARBITRARY_IMPL_BV(32)
BENNET_ARBITRARY_IMPL_BV(64)

cn_pointer* bennet_arbitrary_cn_pointer(bennet_domain(uintptr_t) * cs) {
  return convert_to_cn_pointer((void*)bennet_domain_arbitrary(uintptr_t, cs));
}

static uint8_t null_in_every = 5;

uint8_t get_null_in_every(void) {
  return null_in_every;
}

void set_null_in_every(uint8_t n) {
  null_in_every = n;
}
