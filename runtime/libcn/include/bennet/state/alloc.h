#ifndef BENNET_ALLOC_H
#define BENNET_ALLOC_H

#include <stdlib.h>

#include <bennet/internals/domain.h>
#include <bennet/utils/optional.h>
#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

void bennet_alloc_reset(void);
size_t bennet_alloc_save(void);
void bennet_alloc_restore(size_t size);
int bennet_alloc_check(void* p, size_t sz);
void bennet_alloc_record(void* p, size_t sz);
cn_pointer* bennet_alloc(bennet_domain(uintptr_t) * cs);

void bennet_ownership_reset(void);
size_t bennet_ownership_save(void);
void bennet_ownership_restore(size_t size);
int bennet_ownership_check(void* p, size_t sz);
void bennet_ownership_update(void* p, size_t sz);
size_t bennet_ownership_size(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_ALLOC_H
