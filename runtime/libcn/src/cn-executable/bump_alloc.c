////////////////////
// Bump Allocator //
////////////////////

#include <assert.h>
#include <inttypes.h>
#include <stdalign.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/bump_alloc.h>
#include <cn-executable/fulminate_alloc.h>
#include <cn-executable/utils.h>

#define BUMP_BLOCK_SIZE  (1024 * 1024)
#define BUMP_BLOCK_COUNT 1024
static char* bump_blocks[BUMP_BLOCK_COUNT];
static uint16_t bump_curr_block;
static char* bump_curr;

#ifdef CN_DEBUG_PRINTING
void cn_bump_fprint(FILE* file) {
  fprintf(file,
      "Block: %" PRIu16 ", Start: %p, Next: %p\n",
      bump_curr_block,
      bump_blocks[bump_curr_block],
      bump_curr);
}

void cn_bump_print() {
  cn_bump_fprint(stdout);
}
#else
void cn_bump_fprint(FILE* file) {}

void cn_bump_print() {}
#endif

void cn_bump_init() {
  if (bump_curr == NULL) {
    bump_blocks[0] = fulminate_malloc(BUMP_BLOCK_SIZE);
    bump_curr = bump_blocks[0];
  }
}

bool bump_can_fit(size_t nbytes) {
  if (nbytes > BUMP_BLOCK_SIZE) {
    return 0;
  }

  if (bump_curr + nbytes > bump_blocks[bump_curr_block] + BUMP_BLOCK_SIZE) {
    return 0;
  }

  return 1;
}

bool bump_expand() {
  if (bump_curr_block + 1 >= BUMP_BLOCK_COUNT) {
    cn_failure(CN_FAILURE_ALLOC, NON_SPEC);
    return 0;
  }

  bump_curr_block++;

  if (bump_blocks[bump_curr_block] == NULL) {
    bump_blocks[bump_curr_block] = fulminate_malloc(BUMP_BLOCK_SIZE);
  }

  bump_curr = bump_blocks[bump_curr_block];

  return 1;
}

void* bump_by(size_t nbytes) {
  if (nbytes > BUMP_BLOCK_SIZE) {
    cn_printf(CN_LOGGING_INFO,
        "Attempted to bump allocate larger than maximum allocation size %d.\n",
        BUMP_BLOCK_SIZE);
    cn_failure(CN_FAILURE_ALLOC, NON_SPEC);
    return 0;
  }

  if (!bump_can_fit(nbytes)) {
    if (!bump_expand()) {
      return NULL;
    }
  }

  void* res = bump_curr;
  bump_curr += nbytes;

  return res;
}

void* cn_bump_aligned_alloc(size_t alignment, size_t nbytes) {
  if (nbytes == 0) {
    return NULL;
  }

  cn_bump_init();

  if ((uintptr_t)bump_curr % alignment != 0) {
    size_t padding = (alignment - (uintptr_t)bump_curr % alignment) % alignment;
    if (!bump_can_fit(padding + nbytes)) {
      if (!bump_expand()) {
        return NULL;
      }
      padding = (alignment - (uintptr_t)bump_curr % alignment) % alignment;
    }

    void* prev = bump_curr;
    void* res = bump_by(padding);
    if (res == NULL) {
      bump_curr = prev;
      return NULL;
    }
  }

  return bump_by(nbytes);
}

void* cn_bump_malloc(size_t nbytes) {
  return cn_bump_aligned_alloc(alignof(max_align_t), nbytes);
}

void* cn_bump_calloc(size_t count, size_t size) {
  size_t nbytes = count * size;

  void* p = cn_bump_malloc(nbytes);
  if (p != NULL) {
    memset(p, 0, nbytes);
  }
  return p;
}

void cn_bump_free_all(void) {
  for (uint16_t i = 0; bump_blocks[i] != NULL; i++) {
    fulminate_free(bump_blocks[i]);
    bump_blocks[i] = NULL;
  }
  bump_curr_block = 0;
  bump_curr = NULL;
}

cn_bump_frame_id cn_bump_get_frame_id(void) {
  cn_bump_init();

  return (cn_bump_frame_id){.block = bump_curr_block, .pointer = bump_curr};
}

void cn_bump_free_after(cn_bump_frame_id frame_id) {
  bump_curr = frame_id.pointer;
  bump_curr_block = frame_id.block;
}
