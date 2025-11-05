#ifndef CN_TESTING_ALLOCATORS_ARENA_H
#define CN_TESTING_ALLOCATORS_ARENA_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Opaque arena allocator handle.
 *
 * An arena allocator provides fast memory allocation with deferred
 * deallocation. All allocations are freed together when the arena
 * is destroyed or reset.
 */
typedef struct cn_arena cn_arena;

/**
 * Frame ID for checkpoint/restore operations.
 *
 * A frame represents a point in the arena's allocation history.
 * You can restore to a previous frame to deallocate everything
 * allocated after that point.
 */
typedef uint64_t cn_arena_frame_id;

/**
 * Create a new arena allocator.
 *
 * @param block_size The size of each allocation block in bytes.
 *                   Use 0 for default (8MB).
 * @return A new arena, or NULL on allocation failure.
 */
cn_arena* cn_arena_create(size_t block_size);

/**
 * Destroy an arena and free all its memory.
 *
 * @param arena The arena to destroy. Must not be NULL.
 */
void cn_arena_destroy(cn_arena* arena);

/**
 * Allocate memory from an arena.
 *
 * @param arena The arena to allocate from. Must not be NULL.
 * @param size The number of bytes to allocate.
 * @return A pointer to the allocated memory, or NULL if allocation fails.
 */
void* cn_arena_malloc(cn_arena* arena, size_t size);

/**
 * Allocate and zero-initialize memory from an arena.
 *
 * @param arena The arena to allocate from. Must not be NULL.
 * @param count The number of elements to allocate.
 * @param size The size of each element in bytes.
 * @return A pointer to the allocated and zeroed memory, or NULL if allocation fails.
 */
void* cn_arena_calloc(cn_arena* arena, size_t count, size_t size);

/**
 * Allocate aligned memory from an arena.
 *
 * @param arena The arena to allocate from. Must not be NULL.
 * @param alignment The alignment requirement (must be a power of 2).
 * @param size The number of bytes to allocate.
 * @return A pointer to the aligned allocated memory, or NULL if allocation fails.
 */
void* cn_arena_aligned_alloc(cn_arena* arena, size_t alignment, size_t size);

/**
 * Get the current frame ID for checkpoint/restore operations.
 *
 * @param arena The arena. Must not be NULL.
 * @return A frame ID representing the current allocation state.
 */
cn_arena_frame_id cn_arena_get_frame(cn_arena* arena);

/**
 * Restore the arena to a previous frame, freeing all memory allocated
 * after that frame.
 *
 * @param arena The arena. Must not be NULL.
 * @param frame The frame ID to restore to (obtained from cn_arena_get_frame).
 */
void cn_arena_restore_frame(cn_arena* arena, cn_arena_frame_id frame);

/**
 * Reset the arena, freeing all allocated memory but keeping the arena
 * structure intact for reuse.
 *
 * @param arena The arena to reset. Must not be NULL.
 */
void cn_arena_reset(cn_arena* arena);

/**
 * Get the total number of bytes allocated from the arena.
 *
 * @param arena The arena. Must not be NULL.
 * @return The number of bytes currently allocated.
 */
size_t cn_arena_get_used(cn_arena* arena);

#ifdef __cplusplus
}
#endif

#endif /* CN_TESTING_ALLOCATORS_ARENA_H */
