#ifndef CN_TESTING_ALLOCATORS_STD_ALLOC_H
#define CN_TESTING_ALLOCATORS_STD_ALLOC_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Standard allocator - wraps the C standard library allocation functions
 * (malloc, calloc, realloc, aligned_alloc, free) while tracking all
 * allocations. This enables freeing all tracked allocations at once
 * and detecting attempts to free untracked pointers.
 *
 * The allocator is lazily initialized on first use.
 */

/**
 * Allocate memory using malloc and track the allocation.
 *
 * @param size The number of bytes to allocate.
 * @return A pointer to the allocated memory, or NULL if allocation fails.
 */
void* std_malloc(size_t size);

/**
 * Allocate and zero-initialize memory using calloc and track the allocation.
 *
 * @param count The number of elements to allocate.
 * @param size The size of each element in bytes.
 * @return A pointer to the allocated and zeroed memory, or NULL if allocation
 * fails.
 */
void* std_calloc(size_t count, size_t size);

/**
 * Reallocate memory using realloc and update tracking.
 *
 * @param ptr The pointer to the memory to reallocate, or NULL.
 * @param size The new size in bytes.
 * @return A pointer to the reallocated memory, or NULL if allocation fails.
 */
void* std_realloc(void* ptr, size_t size);

/**
 * Allocate aligned memory using aligned_alloc and track the allocation.
 *
 * @param alignment The alignment requirement (must be a power of 2).
 * @param size The number of bytes to allocate.
 * @return A pointer to the aligned allocated memory, or NULL if allocation
 * fails.
 */
void* std_aligned_alloc(size_t alignment, size_t size);

/**
 * Free a tracked allocation.
 *
 * Asserts if the pointer is not tracked (indicates a bug).
 * Calling with NULL is a no-op.
 *
 * @param ptr The pointer to the memory to free.
 */
void std_free(void* ptr);

/**
 * Free all tracked allocations but keep the allocator structure intact
 * for reuse.
 */
void std_free_all(void);

/**
 * Configure the test allocator to use the standard allocator.
 *
 * After calling this function, cn_test_malloc, cn_test_calloc, etc. will
 * use the standard library functions while tracking allocations.
 */
void std_set_default_alloc(void);

#ifdef __cplusplus
}
#endif

#endif /* CN_TESTING_ALLOCATORS_STD_ALLOC_H */
