#include <stdlib.h>

#include <cn-executable/fulminate_alloc.h>
#include <cn-executable/utils.h>

allocator fulm_default_alloc =
    (allocator){.malloc = &malloc, .calloc = &calloc, .free = &free};

void *fulm_malloc(size_t size, allocator *alloc) {
  void *res = alloc->malloc(size);
  if (res == NULL && size != 0) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }

  return res;
}

void *fulm_calloc(size_t count, size_t size, allocator *alloc) {
  void *res = alloc->calloc(count, size);
  if (res == NULL && count != 0 && size != 0) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }

  return res;
}

void fulm_free(void *p, allocator *alloc) {
  alloc->free(p);
}
