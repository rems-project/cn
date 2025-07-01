#ifndef BENNET_EXP_RAND_H
#define BENNET_EXP_RAND_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void bennet_srand(uint64_t seed);
uint64_t bennet_rand(void);

uint8_t bennet_uniform_u8(uint8_t);
uint16_t bennet_uniform_u16(uint16_t);
uint32_t bennet_uniform_u32(uint32_t);
uint64_t bennet_uniform_u64(uint64_t);

int8_t bennet_uniform_i8(uint8_t);
int16_t bennet_uniform_i16(uint16_t);
int32_t bennet_uniform_i32(uint32_t);
int64_t bennet_uniform_i64(uint64_t);

uint8_t bennet_uniform_u8_sized(uint8_t);
uint16_t bennet_uniform_u16_sized(uint16_t);
uint32_t bennet_uniform_u32_sized(uint32_t);
uint64_t bennet_uniform_u64_sized(uint64_t);

int8_t bennet_uniform_i8_sized(uint8_t);
int16_t bennet_uniform_i16_sized(uint16_t);
int32_t bennet_uniform_i32_sized(uint32_t);
int64_t bennet_uniform_i64_sized(uint64_t);

uint8_t bennet_range_u8(uint8_t, uint8_t);
uint16_t bennet_range_u16(uint16_t, uint16_t);
uint32_t bennet_range_u32(uint32_t, uint32_t);
uint64_t bennet_range_u64(uint64_t, uint64_t);

int8_t bennet_range_i8(int8_t, int8_t);
int16_t bennet_range_i16(int16_t, int16_t);
int32_t bennet_range_i32(int32_t, int32_t);
int64_t bennet_range_i64(int64_t, int64_t);

uint8_t bennet_le_u8(uint8_t);
uint16_t bennet_le_u16(uint16_t);
uint32_t bennet_le_u32(uint32_t);
uint64_t bennet_le_u64(uint64_t);

int8_t bennet_le_i8(int8_t);
int16_t bennet_le_i16(int16_t);
int32_t bennet_le_i32(int32_t);
int64_t bennet_le_i64(int64_t);

uint8_t bennet_ge_u8(uint8_t);
uint16_t bennet_ge_u16(uint16_t);
uint32_t bennet_ge_u32(uint32_t);
uint64_t bennet_ge_u64(uint64_t);

int8_t bennet_ge_i8(int8_t);
int16_t bennet_ge_i16(int16_t);
int32_t bennet_ge_i32(int32_t);
int64_t bennet_ge_i64(int64_t);

uint8_t bennet_mult_range_u8(uint8_t, uint8_t, uint8_t);
uint16_t bennet_mult_range_u16(uint16_t, uint16_t, uint16_t);
uint32_t bennet_mult_range_u32(uint32_t, uint32_t, uint32_t);
uint64_t bennet_mult_range_u64(uint64_t, uint64_t, uint64_t);

int8_t bennet_mult_range_i8(int8_t, int8_t, int8_t);
int16_t bennet_mult_range_i16(int16_t, int16_t, int16_t);
int32_t bennet_mult_range_i32(int32_t, int32_t, int32_t);
int64_t bennet_mult_range_i64(int64_t, int64_t, int64_t);

uint8_t bennet_mult_u8(uint8_t);
uint16_t bennet_mult_u16(uint16_t);
uint32_t bennet_mult_u32(uint32_t);
uint64_t bennet_mult_u64(uint64_t);

int8_t bennet_mult_i8(int8_t);
int16_t bennet_mult_i16(int16_t);
int32_t bennet_mult_i32(int32_t);
int64_t bennet_mult_i64(int64_t);

void bennet_shuffle(void* arr, size_t len, size_t size);

void bennet_split(size_t n, size_t* arr[], size_t len);

uint64_t bennet_rand_retry(void);

struct choice_list {
  uint64_t choice;
  struct choice_list* next;
  struct choice_list* prev;
};

typedef struct choice_list* bennet_rand_checkpoint;

bennet_rand_checkpoint bennet_rand_save(void);

void bennet_rand_restore(bennet_rand_checkpoint checkpoint);

void bennet_rand_replace(bennet_rand_checkpoint checkpoint);

void bennet_rand_skip_to(bennet_rand_checkpoint checkpoint);

char* bennet_rand_to_str(bennet_rand_checkpoint checkpoint);

void bennet_rand_start_injection(void);
void bennet_rand_end_injection(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_EXP_RAND_H
