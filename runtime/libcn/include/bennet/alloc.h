#ifndef BENNET_ALLOC_H
#define BENNET_ALLOC_H

#include <stdlib.h>

#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

uint8_t get_null_in_every(void);
void set_null_in_every(uint8_t n);

int is_sized_null(void);
void set_sized_null(void);
void unset_sized_null(void);

void bennet_alloc_reset(void);
void* bennet_alloc_save(void);
void bennet_alloc_restore(void* ptr);

void bennet_ownership_reset(void);

void* bennet_ownership_save(void);

void bennet_ownership_restore(void* ptr);

cn_pointer* bennet_alloc(cn_bits_u64* sz);

cn_pointer* bennet_aligned_alloc(cn_bits_u64* alignment, cn_bits_u64* sz);

int bennet_alloc_check(void* p, size_t sz);

void bennet_ownership_update(void* p, size_t sz);

int bennet_ownership_check(void* p, size_t sz);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_ALLOC_H
