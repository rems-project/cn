#ifndef FULMINATE_ALLOC
#define FULMINATE_ALLOC

//////////////////////////////////
// Fulminate Allocator API //
//////////////////////////////////

#include "rts_deps.h"

#ifdef __cplusplus
extern "C" {
#endif

void* fulminate_malloc(size_t size);

void* fulminate_calloc(size_t count, size_t size);

void* fulminate_realloc(void* p, size_t size);

void fulminate_free(void* p);

#ifdef __cplusplus
}
#endif

#endif  // FULMINATE_ALLOC
