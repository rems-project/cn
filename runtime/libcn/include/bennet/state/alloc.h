#ifndef BENNET_ALLOC_H
#define BENNET_ALLOC_H

#include <stdlib.h>

#include <bennet/internals/domain.h>
#include <bennet/utils/optional.h>
#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

void bennet_alloc_destroy(void);
void bennet_alloc_init(void);
size_t bennet_alloc_save(void);
void bennet_alloc_restore(size_t size);
int bennet_alloc_check(void* p, size_t sz);
void bennet_alloc_record(void* p, size_t sz);
void* bennet_alloc(size_t bytes);
void* bennet_alloc_bounded(size_t bytes, uintptr_t lower_bound, uintptr_t upper_bound);

void bennet_ownership_destroy(void);
void bennet_ownership_init(void);
size_t bennet_ownership_save(void);
void bennet_ownership_restore(size_t size);
int bennet_ownership_check(void* p, size_t sz);
void bennet_ownership_update(void* p, size_t sz);
size_t bennet_ownership_size(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_ALLOC_H
