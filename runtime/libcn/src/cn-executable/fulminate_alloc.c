#include <stdlib.h>

#include <cn-executable/fulminate_alloc.h>

allocator fulm_default_alloc =
    (allocator){.malloc = &malloc, .calloc = &calloc, .free = &free};

void *fulm_malloc(size_t size, allocator *alloc) {
  return alloc->malloc(size);
}

void *fulm_calloc(size_t count, size_t size, allocator *alloc) {
  return alloc->calloc(count, size);
}

void fulm_free(void *p, allocator *alloc) {
  alloc->free(p);
}
