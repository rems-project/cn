#ifndef BENNET_RAND_H
#define BENNET_RAND_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

void bennet_srand(uint64_t seed);
uint64_t bennet_rand(void);

uint8_t bennet_uniform_uint8_t(uint8_t);
uint16_t bennet_uniform_uint16_t(uint16_t);
uint32_t bennet_uniform_uint32_t(uint32_t);
uint64_t bennet_uniform_uint64_t(uint64_t);

int8_t bennet_uniform_int8_t(uint8_t);
int16_t bennet_uniform_int16_t(uint16_t);
int32_t bennet_uniform_int32_t(uint32_t);
int64_t bennet_uniform_int64_t(uint64_t);

uintptr_t bennet_uniform_uintptr_t(uintptr_t);

uint8_t bennet_range_uint8_t(uint8_t, uint8_t);
uint16_t bennet_range_uint16_t(uint16_t, uint16_t);
uint32_t bennet_range_uint32_t(uint32_t, uint32_t);
uint64_t bennet_range_uint64_t(uint64_t, uint64_t);

int8_t bennet_range_int8_t(int8_t, int8_t);
int16_t bennet_range_int16_t(int16_t, int16_t);
int32_t bennet_range_int32_t(int32_t, int32_t);
int64_t bennet_range_int64_t(int64_t, int64_t);

uint8_t bennet_le_uint8_t(uint8_t);
uint16_t bennet_le_uint16_t(uint16_t);
uint32_t bennet_le_uint32_t(uint32_t);
uint64_t bennet_le_uint64_t(uint64_t);

int8_t bennet_le_int8_t(int8_t);
int16_t bennet_le_int16_t(int16_t);
int32_t bennet_le_int32_t(int32_t);
int64_t bennet_le_int64_t(int64_t);

uint8_t bennet_ge_uint8_t(uint8_t);
uint16_t bennet_ge_uint16_t(uint16_t);
uint32_t bennet_ge_uint32_t(uint32_t);
uint64_t bennet_ge_uint64_t(uint64_t);

int8_t bennet_ge_int8_t(int8_t);
int16_t bennet_ge_int16_t(int16_t);
int32_t bennet_ge_int32_t(int32_t);
int64_t bennet_ge_int64_t(int64_t);

uint8_t bennet_mult_range_uint8_t(uint8_t, uint8_t, uint8_t);
uint16_t bennet_mult_range_uint16_t(uint16_t, uint16_t, uint16_t);
uint32_t bennet_mult_range_uint32_t(uint32_t, uint32_t, uint32_t);
uint64_t bennet_mult_range_uint64_t(uint64_t, uint64_t, uint64_t);

int8_t bennet_mult_range_int8_t(int8_t, int8_t, int8_t);
int16_t bennet_mult_range_int16_t(int16_t, int16_t, int16_t);
int32_t bennet_mult_range_int32_t(int32_t, int32_t, int32_t);
int64_t bennet_mult_range_int64_t(int64_t, int64_t, int64_t);

uint8_t bennet_mult_uint8_t(uint8_t);
uint16_t bennet_mult_uint16_t(uint16_t);
uint32_t bennet_mult_uint32_t(uint32_t);
uint64_t bennet_mult_uint64_t(uint64_t);

int8_t bennet_mult_int8_t(int8_t);
int16_t bennet_mult_int16_t(int16_t);
int32_t bennet_mult_int32_t(int32_t);
int64_t bennet_mult_int64_t(int64_t);

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

#endif  // BENNET_RAND_H
