#ifndef FULMINATE_USER_H
#define FULMINATE_USER_H

#include <cn-executable/rts_deps.h>

void* cn_aligned_alloc(size_t align, size_t size);
void* cn_unsafe_aligned_alloc(size_t align, size_t size);
void* cn_malloc(size_t size);
void* cn_unsafe_malloc(size_t size);
void* cn_calloc(size_t num, size_t size);
void* cn_unsafe_calloc(size_t num, size_t size);
void cn_free_sized(void*, size_t len);

#endif  // FULMINATE_USER_H
