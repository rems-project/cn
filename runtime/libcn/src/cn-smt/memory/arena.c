#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/utils.h>
#include <cn-smt/memory/arena.h>
#include <cn-smt/memory/test_alloc.h>

/* Default block size: 8MB */
#define CN_ARENA_DEFAULT_BLOCK_SIZE (8 * 1024 * 1024)

/* Minimum block size: 4KB */
#define CN_ARENA_MIN_BLOCK_SIZE (4 * 1024)

/**
 * A block of memory in the arena.
 * Blocks are linked together to form a chain.
 */
typedef struct cn_arena_block {
  struct cn_arena_block* next; /* Next block in the chain */
  size_t size;                 /* Total size of this block (excluding header) */
  size_t used;                 /* Number of bytes used in this block */
  /* Data follows immediately after this header */
} cn_arena_block;

/**
 * The arena structure.
 */
struct cn_arena {
  cn_arena_block* first_block;   /* First block in the chain */
  cn_arena_block* current_block; /* Current block being allocated from */
  size_t block_size;             /* Size of each block */
  size_t total_allocated;        /* Total bytes allocated */
};

/**
 * Get a pointer to the data area of a block.
 */
static inline void* cn_arena_block_data(cn_arena_block* block) {
  return (void*)(block + 1);
}

/**
 * Allocate a new block.
 */
static cn_arena_block* cn_arena_allocate_block(size_t size) {
  /* Allocate block header plus data */
  cn_arena_block* block = (cn_arena_block*)malloc(sizeof(cn_arena_block) + size);
  if (!block) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    return NULL;
  }

  block->next = NULL;
  block->size = size;
  block->used = 0;

  return block;
}

/**
 * Check if a value is a power of 2.
 */
static inline bool is_power_of_2(size_t x) {
  return x > 0 && (x & (x - 1)) == 0;
}

/**
 * Align a value up to the specified alignment.
 */
static inline size_t align_up(size_t value, size_t alignment) {
  assert(is_power_of_2(alignment));
  return (value + alignment - 1) & ~(alignment - 1);
}

cn_arena* cn_arena_create(size_t block_size) {
  /* Use default block size if not specified */
  if (block_size == 0) {
    block_size = CN_ARENA_DEFAULT_BLOCK_SIZE;
  }

  /* Ensure minimum block size */
  if (block_size < CN_ARENA_MIN_BLOCK_SIZE) {
    block_size = CN_ARENA_MIN_BLOCK_SIZE;
  }

  /* Allocate arena structure */
  cn_arena* arena = (cn_arena*)malloc(sizeof(cn_arena));
  assert(arena);

  /* Allocate first block */
  cn_arena_block* first_block = cn_arena_allocate_block(block_size);
  if (!first_block) {
    free(arena);
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    return NULL;
  }

  arena->first_block = first_block;
  arena->current_block = first_block;
  arena->block_size = block_size;
  arena->total_allocated = 0;

  return arena;
}

void cn_arena_destroy(cn_arena* arena) {
  assert(arena != NULL);

  /* Free all blocks */
  cn_arena_block* block = arena->first_block;
  while (block) {
    cn_arena_block* next = block->next;
    free(block);
    block = next;
  }

  /* Free arena structure */
  free(arena);
}

void* cn_arena_malloc(cn_arena* arena, size_t size) {
  return cn_arena_aligned_alloc(arena, alignof(max_align_t), size);
}

void* cn_arena_calloc(cn_arena* arena, size_t count, size_t size) {
  assert(arena != NULL);

  /* Check for overflow */
  if (count > 0 && size > SIZE_MAX / count) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    return NULL;
  }

  size_t total_size = count * size;
  void* ptr = cn_arena_malloc(arena, total_size);

  if (ptr) {
    memset(ptr, 0, total_size);
  }

  return ptr;
}

void* cn_arena_aligned_alloc(cn_arena* arena, size_t alignment, size_t size) {
  assert(arena != NULL);
  assert(is_power_of_2(alignment));

  if (size == 0) {
    return NULL;
  }

  cn_arena_block* block = arena->current_block;

  /* Calculate aligned position in current block */
  uintptr_t block_data = (uintptr_t)cn_arena_block_data(block);
  uintptr_t current_pos = block_data + block->used;
  uintptr_t aligned_pos = align_up(current_pos, alignment);
  size_t padding = aligned_pos - current_pos;

  /* Check if current block has enough space (including padding) */
  if (block->used + padding + size <= block->size) {
    block->used += padding + size;
    arena->total_allocated += padding + size;
    return (void*)aligned_pos;
  }

  /* Need a new block */
  size_t new_block_size = arena->block_size;

  /* If the requested size plus max padding is larger than default,
   * allocate a larger block */
  size_t required_size = size + alignment - 1;
  if (required_size > new_block_size) {
    new_block_size = required_size;
  }

  cn_arena_block* new_block = cn_arena_allocate_block(new_block_size);
  if (!new_block) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    return NULL;
  }

  /* Link new block to the chain */
  block->next = new_block;
  arena->current_block = new_block;

  /* Calculate aligned position in new block */
  block_data = (uintptr_t)cn_arena_block_data(new_block);
  aligned_pos = align_up(block_data, alignment);
  padding = aligned_pos - block_data;

  new_block->used = padding + size;
  arena->total_allocated += padding + size;

  return (void*)aligned_pos;
}

cn_arena_frame_id cn_arena_get_frame(cn_arena* arena) {
  assert(arena != NULL);

  cn_arena_block* block = arena->current_block;

  /* Encode block pointer and offset into frame ID
   * High 32 bits: truncated block pointer (for validation)
   * Low 32 bits: offset within block */
  uintptr_t block_ptr = (uintptr_t)block;
  uint64_t frame = ((uint64_t)(block_ptr & 0xFFFFFFFF) << 32) | (uint64_t)block->used;

  return frame;
}

void cn_arena_restore_frame(cn_arena* arena, cn_arena_frame_id frame) {
  assert(arena != NULL);

  /* Extract block pointer hash and offset from frame ID */
  uint32_t block_ptr_hash = (uint32_t)(frame >> 32);
  uint32_t offset = (uint32_t)(frame & 0xFFFFFFFF);

  /* Find the block corresponding to this frame */
  cn_arena_block* block = arena->first_block;
  cn_arena_block* target_block = NULL;

  while (block) {
    uintptr_t current_ptr = (uintptr_t)block;
    if ((uint32_t)(current_ptr & 0xFFFFFFFF) == block_ptr_hash) {
      target_block = block;
      break;
    }
    block = block->next;
  }

  /* Frame must be valid */
  assert(target_block != NULL);
  assert(offset <= target_block->size);

  /* Free all blocks after the target block */
  cn_arena_block* next_block = target_block->next;
  while (next_block) {
    cn_arena_block* to_free = next_block;
    next_block = next_block->next;

    arena->total_allocated -= to_free->used;
    free(to_free);
  }

  target_block->next = NULL;

  /* Restore the offset in the target block */
  size_t bytes_freed = target_block->used - offset;
  target_block->used = offset;
  arena->total_allocated -= bytes_freed;
  arena->current_block = target_block;
}

void cn_arena_free_all(cn_arena* arena) {
  assert(arena != NULL);

  /* Free all blocks except the first */
  cn_arena_block* block = arena->first_block->next;
  while (block) {
    cn_arena_block* next = block->next;
    free(block);
    block = next;
  }

  /* Reset first block */
  arena->first_block->next = NULL;
  arena->first_block->used = 0;
  arena->current_block = arena->first_block;
  arena->total_allocated = 0;
}

size_t cn_arena_get_used(cn_arena* arena) {
  assert(arena != NULL);
  return arena->total_allocated;
}

void* cn_arena_realloc(cn_arena* arena, void* ptr, size_t size) {
  assert(arena != NULL);

  /* If ptr is NULL, act like malloc */
  if (ptr == NULL) {
    return cn_arena_malloc(arena, size);
  }

  /* If size is 0, we can't free in an arena, so just return NULL */
  if (size == 0) {
    return NULL;
  }

  /* Allocate new memory */
  void* new_ptr = cn_arena_malloc(arena, size);
  if (new_ptr == NULL) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
    return NULL;
  }

  /* Copy data from old to new
   * Note: We don't track individual allocation sizes, so we copy 'size' bytes.
   * The caller must ensure this doesn't exceed the original allocation size. */
  memcpy(new_ptr, ptr, size);

  /* Note: The old pointer is not freed since arenas don't support
   * individual deallocation */

  return new_ptr;
}

void cn_arena_free(cn_arena* arena, void* ptr) {
  assert(arena != NULL);
  /* No-op: arenas don't support individual deallocation */
  (void)ptr;
}

void cn_arena_set_default_alloc(cn_arena* arena) {
  assert(arena != NULL);

  cn_test_set_alloc(arena,
      (void* (*)(void*, size_t))cn_arena_malloc,
      (void* (*)(void*, size_t, size_t))cn_arena_calloc,
      (void* (*)(void*, void*, size_t))cn_arena_realloc,
      (void* (*)(void*, size_t, size_t))cn_arena_aligned_alloc,
      (void (*)(void*, void*))cn_arena_free,
      (void (*)(void*))cn_arena_free_all);
}

void cn_arena_push_alloc(cn_arena* arena) {
  assert(arena != NULL);

  cn_test_push_alloc(arena,
      (void* (*)(void*, size_t))cn_arena_malloc,
      (void* (*)(void*, size_t, size_t))cn_arena_calloc,
      (void* (*)(void*, void*, size_t))cn_arena_realloc,
      (void* (*)(void*, size_t, size_t))cn_arena_aligned_alloc,
      (void (*)(void*, void*))cn_arena_free,
      (void (*)(void*))cn_arena_free_all);
}
