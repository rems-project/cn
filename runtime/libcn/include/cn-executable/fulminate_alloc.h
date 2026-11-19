#ifndef FULMINATE_ALLOC
#define FULMINATE_ALLOC

//////////////////////////////////
// Fulminate Allocator //
//////////////////////////////////

#include "rts_deps.h"

typedef struct allocator {
  void* (*malloc)(size_t size);
  void* (*calloc)(size_t count, size_t size);
  void (*free)(void* p);
} allocator;

extern allocator fulm_default_alloc;

void* fulm_malloc(size_t size, allocator* alloc);
void* fulm_calloc(size_t count, size_t size, allocator* alloc);
void fulm_free(void* p, allocator* alloc);

#endif  // FULMINATE_ALLOC
