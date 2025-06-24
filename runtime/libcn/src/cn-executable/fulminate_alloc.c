//////////////////////////////////
// Fulminate Allocator API Implementation //
// Calls into libc //
//////////////////////////////////

#include <stdlib.h>

#include <cn-executable/fulminate_alloc.h>

void* fulminate_malloc(size_t size) {
  return malloc(size);
}

void* fulminate_calloc(size_t count, size_t size) {
  return calloc(count, size);
}

void* fulminate_realloc(void* p, size_t size) {
  return realloc(p, size);
}

void fulminate_free(void* p) {
  return free(p);
}
