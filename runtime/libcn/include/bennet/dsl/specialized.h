#ifndef BENNET_SPECIALIZED_H
#define BENNET_SPECIALIZED_H

#include <cn-executable/eval.h>

cn_pointer* bennet_specialized_cn_pointer(cn_pointer* lower_bound_ex,
    cn_pointer* lower_bound_inc,
    cn_pointer* upper_bound_inc,
    cn_pointer* upper_bound_ex,
    const void* vars[]);

cn_bits_u8* bennet_specialized_cn_bits_u8(cn_bits_u8* lower_bound_ex,
    cn_bits_u8* lower_bound_inc,
    cn_bits_u8* upper_bound_inc,
    cn_bits_u8* upper_bound_ex,
    const void* vars[]);
cn_bits_i8* bennet_specialized_cn_bits_i8(cn_bits_i8* lower_bound_ex,
    cn_bits_i8* lower_bound_inc,
    cn_bits_i8* upper_bound_inc,
    cn_bits_i8* upper_bound_ex,
    const void* vars[]);

cn_bits_u16* bennet_specialized_cn_bits_u16(cn_bits_u16* lower_bound_ex,
    cn_bits_u16* lower_bound_inc,
    cn_bits_u16* upper_bound_inc,
    cn_bits_u16* upper_bound_ex,
    const void* vars[]);
cn_bits_i16* bennet_specialized_cn_bits_i16(cn_bits_i16* lower_bound_ex,
    cn_bits_i16* lower_bound_inc,
    cn_bits_i16* upper_bound_inc,
    cn_bits_i16* upper_bound_ex,
    const void* vars[]);

cn_bits_u32* bennet_specialized_cn_bits_u32(cn_bits_u32* lower_bound_ex,
    cn_bits_u32* lower_bound_inc,
    cn_bits_u32* upper_bound_inc,
    cn_bits_u32* upper_bound_ex,
    const void* vars[]);
cn_bits_i32* bennet_specialized_cn_bits_i32(cn_bits_i32* lower_bound_ex,
    cn_bits_i32* lower_bound_inc,
    cn_bits_i32* upper_bound_inc,
    cn_bits_i32* upper_bound_ex,
    const void* vars[]);

cn_bits_u64* bennet_specialized_cn_bits_u64(cn_bits_u64* lower_bound_ex,
    cn_bits_u64* lower_bound_inc,
    cn_bits_u64* upper_bound_inc,
    cn_bits_u64* upper_bound_ex,
    const void* vars[]);
cn_bits_i64* bennet_specialized_cn_bits_i64(cn_bits_i64* lower_bound_ex,
    cn_bits_i64* lower_bound_inc,
    cn_bits_i64* upper_bound_inc,
    cn_bits_i64* upper_bound_ex,
    const void* vars[]);

#endif  // BENNET_SPECIALIZED_H
