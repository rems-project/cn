#ifndef CN_BUMP_ALLOC
#define CN_BUMP_ALLOC

////////////////////
// Bump Allocator //
////////////////////

#include "rts_deps.h"

#ifdef __cplusplus
extern "C" {
#endif

void* cn_bump_aligned_alloc(size_t alignment, size_t nbytes);

void* cn_bump_malloc(size_t nbytes);

void* cn_bump_calloc(size_t count, size_t size);

void cn_bump_free_all();

typedef struct {
  uint16_t block;
  char* pointer;
} cn_bump_frame_id;

cn_bump_frame_id cn_bump_get_frame_id(void);

void cn_bump_free_after(cn_bump_frame_id frame_id);

void cn_bump_print();

#ifdef __cplusplus
}
#endif

#endif  // CN_BUMP_ALLOC
