#ifndef BENNET_ARBITRARY_H
#define BENNET_ARBITRARY_H

#include <bennet/internals/domain.h>
#include <cn-executable/utils.h>

uint8_t get_null_in_every(void);
void set_null_in_every(uint8_t n);

cn_pointer* bennet_arbitrary_cn_pointer(bennet_domain(uintptr_t) * cs);

cn_bits_u8* bennet_arbitrary_cn_bits_u8(bennet_domain(uint8_t) * cs);
cn_bits_i8* bennet_arbitrary_cn_bits_i8(bennet_domain(int8_t) * cs);

cn_bits_u16* bennet_arbitrary_cn_bits_u16(bennet_domain(uint16_t) * cs);
cn_bits_i16* bennet_arbitrary_cn_bits_i16(bennet_domain(int16_t) * cs);

cn_bits_u32* bennet_arbitrary_cn_bits_u32(bennet_domain(uint32_t) * cs);
cn_bits_i32* bennet_arbitrary_cn_bits_i32(bennet_domain(int32_t) * cs);

cn_bits_u64* bennet_arbitrary_cn_bits_u64(bennet_domain(uint64_t) * cs);
cn_bits_i64* bennet_arbitrary_cn_bits_i64(bennet_domain(int64_t) * cs);

#endif  // BENNET_ARBITRARY_H
