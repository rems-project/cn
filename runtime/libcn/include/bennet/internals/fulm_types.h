
typedef struct cn_bits_u8 cn_bits_u8;
typedef struct cn_bits_i8 cn_bits_i8;
typedef struct cn_bits_u16 cn_bits_u16;
typedef struct cn_bits_i16 cn_bits_i16;
typedef struct cn_bits_u32 cn_bits_u32;
typedef struct cn_bits_i32 cn_bits_i32;
typedef struct cn_bits_u64 cn_bits_u64;
typedef struct cn_bits_i64 cn_bits_i64;
typedef struct cn_pointer cn_pointer;
typedef struct cn_bool cn_bool;

void* convert_from_cn_pointer(cn_pointer*);
uint8_t convert_from_cn_bits_u8(cn_bits_u8*);
uint16_t convert_from_cn_bits_u16(cn_bits_u16*);
uint32_t convert_from_cn_bits_u32(cn_bits_u32*);
uint64_t convert_from_cn_bits_u64(cn_bits_u64*);
int8_t convert_from_cn_bits_i8(cn_bits_i8*);
int16_t convert_from_cn_bits_i16(cn_bits_i16*);
int32_t convert_from_cn_bits_i32(cn_bits_i32*);
int64_t convert_from_cn_bits_i64(cn_bits_i64*);



