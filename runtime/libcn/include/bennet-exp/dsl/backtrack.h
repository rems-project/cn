#ifndef BENNET_EXP_BACKTRACK_H
#define BENNET_EXP_BACKTRACK_H

#include <bennet-exp/internals/domain.h>
#include <bennet-exp/state/checkpoint.h>

bool bennet_backtrack_arbitrary_cn_pointer(int* backtracks,
    bennet_domain(uintptr_t) * cs,
    bennet_domain(uintptr_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);

bool bennet_backtrack_arbitrary_cn_bits_u8(int* backtracks,
    bennet_domain(uint8_t) * cs,
    bennet_domain(uint8_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);
bool bennet_backtrack_arbitrary_cn_bits_i8(int* backtracks,
    bennet_domain(int8_t) * cs,
    bennet_domain(int8_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);

bool bennet_backtrack_arbitrary_cn_bits_u16(int* backtracks,
    bennet_domain(uint16_t) * cs,
    bennet_domain(uint16_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);
bool bennet_backtrack_arbitrary_cn_bits_i16(int* backtracks,
    bennet_domain(int16_t) * cs,
    bennet_domain(int16_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);

bool bennet_backtrack_arbitrary_cn_bits_u32(int* backtracks,
    bennet_domain(uint32_t) * cs,
    bennet_domain(uint32_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);
bool bennet_backtrack_arbitrary_cn_bits_i32(int* backtracks,
    bennet_domain(int32_t) * cs,
    bennet_domain(int32_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);

bool bennet_backtrack_arbitrary_cn_bits_u64(int* backtracks,
    bennet_domain(uint64_t) * cs,
    bennet_domain(uint64_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);
bool bennet_backtrack_arbitrary_cn_bits_i64(int* backtracks,
    bennet_domain(int64_t) * cs,
    bennet_domain(int64_t) * cs_tmp,
    const bennet_checkpoint* cp,
    const void* var);

bool bennet_backtrack(int* backtracks, const bennet_checkpoint* cp, const void* var);

#endif  // BENNET_EXP_BACKTRACK_H
