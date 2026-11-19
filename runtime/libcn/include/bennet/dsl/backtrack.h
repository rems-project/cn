#ifndef BENNET_BACKTRACK_H
#define BENNET_BACKTRACK_H

#include <bennet/internals/domain.h>
#include <bennet/state/checkpoint.h>

#define BENNET_BACKTRACK_ARBITRARY_DECL(bits)                                            \
  bool bennet_backtrack_arbitrary_cn_bits_u##bits(int* backtracks,                       \
      bennet_domain(uint##bits##_t) * *cs,                                               \
      bennet_domain(uint##bits##_t) * *cs_tmp,                                           \
      const bennet_checkpoint* cp,                                                       \
      const void* var);                                                                  \
                                                                                         \
  bool bennet_backtrack_arbitrary_cn_bits_i##bits(int* backtracks,                       \
      bennet_domain(int##bits##_t) * *cs,                                                \
      bennet_domain(int##bits##_t) * *cs_tmp,                                            \
      const bennet_checkpoint* cp,                                                       \
      const void* var);

BENNET_BACKTRACK_ARBITRARY_DECL(8)
BENNET_BACKTRACK_ARBITRARY_DECL(16)
BENNET_BACKTRACK_ARBITRARY_DECL(32)
BENNET_BACKTRACK_ARBITRARY_DECL(64)

bool bennet_backtrack_arbitrary_cn_pointer(int* backtracks,
    bennet_domain(uintptr_t) * *cs,
    bennet_domain(uintptr_t) * *cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);

bool bennet_backtrack(int* backtracks, const bennet_checkpoint* cp, const void* var);

#endif  // BENNET_BACKTRACK_H
