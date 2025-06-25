#ifndef FULMINATE_ALLOC
#define FULMINATE_ALLOC

//////////////////////////////////
// Fulminate Allocator //
//////////////////////////////////

#ifdef __cplusplus
extern "C" {
#endif

#include "rts_deps.h"

struct alloc_fns {
  void *(*malloc)(size_t size);
  void *(*calloc)(size_t count, size_t size);
  void (*free)(void *p);
};

extern struct alloc_fns fulminate_internal_alloc;

#ifdef __cplusplus
}
#endif

#endif  // FULMINATE_ALLOC
