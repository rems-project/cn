#ifndef FULMINATE_ALLOC
#define FULMINATE_ALLOC

//////////////////////////////////
// Fulminate Allocator //
//////////////////////////////////

#include "rts_deps.h"

#ifdef __cplusplus
extern "C" {
#endif

struct alloc_fns {
  void* (*malloc)(size_t size);
  void* (*calloc)(size_t count, size_t size);
  void (*free)(void* p);
};

#ifdef __cplusplus
}
#endif

#endif  // FULMINATE_ALLOC
