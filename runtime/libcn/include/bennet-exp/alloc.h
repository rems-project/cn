#ifndef BENNET_EXP_ALLOC_H
#define BENNET_EXP_ALLOC_H

#include <stdlib.h>

#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

uint8_t get_null_in_every(void);
void set_null_in_every(uint8_t n);

void bennet_alloc_reset(void);
void* bennet_alloc_save(void);
void bennet_alloc_restore(void* ptr);

void bennet_ownership_reset(void);

void* bennet_ownership_save(void);

void bennet_ownership_restore(void* ptr);

cn_pointer* bennet_alloc(
    size_t lower_offset_bound, size_t upper_offset_bound, bool is_null);

int bennet_alloc_check(void* p, size_t sz);

void bennet_ownership_update(void* p, size_t sz);

int bennet_ownership_check(void* p, size_t sz);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_EXP_ALLOC_H
