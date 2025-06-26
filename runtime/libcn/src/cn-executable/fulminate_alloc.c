#include <stdlib.h>

#include <cn-executable/fulminate_alloc.h>

allocator flm_default_alloc =
    (allocator){.malloc = &malloc, .calloc = &calloc, .free = &free};

void *flm_malloc(size_t size, allocator *alloc) {
  return alloc->malloc(size);
}

void *flm_calloc(size_t count, size_t size, allocator *alloc) {
  return alloc->calloc(count, size);
}

void flm_free(void *p, allocator *alloc) {
  alloc->free(p);
}
