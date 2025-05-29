#ifndef CN_FL_ALLOC
#define CN_FL_ALLOC

//////////////////////////////////
// Explicit Free List Allocator //
//////////////////////////////////

#include "rts_deps.h"

#ifdef __cplusplus
extern "C" {
#endif

void* cn_fl_aligned_alloc(size_t alignment, size_t size);

void* cn_fl_malloc(size_t size);

void* cn_fl_calloc(size_t count, size_t size);

void* cn_fl_realloc(void* p, size_t size);

void cn_fl_free(void* p);

void cn_fl_free_all();

void cn_fl_print();

#ifdef __cplusplus
}
#endif

#endif  // CN_FL_ALLOC
