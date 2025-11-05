#ifndef CN_TESTING_MEMORY_TEST_ALLOC_H
#define CN_TESTING_MEMORY_TEST_ALLOC_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Allocate memory using the configured test allocator.
 *
 * @param size The number of bytes to allocate.
 * @return A pointer to the allocated memory, or NULL if allocation fails.
 */
void* cn_test_malloc(size_t size);

/**
 * Allocate and zero-initialize memory using the configured test allocator.
 *
 * @param count The number of elements to allocate.
 * @param size The size of each element in bytes.
 * @return A pointer to the allocated and zeroed memory, or NULL if allocation fails.
 */
void* cn_test_calloc(size_t count, size_t size);

/**
 * Reallocate memory using the configured test allocator.
 *
 * @param ptr The pointer to the memory to reallocate, or NULL.
 * @param size The new size in bytes.
 * @return A pointer to the reallocated memory, or NULL if allocation fails.
 */
void* cn_test_realloc(void* ptr, size_t size);

/**
 * Allocate aligned memory using the configured test allocator.
 *
 * @param alignment The alignment requirement (must be a power of 2).
 * @param size The number of bytes to allocate.
 * @return A pointer to the aligned allocated memory, or NULL if allocation fails.
 */
void* cn_test_aligned_alloc(size_t alignment, size_t size);

/**
 * Free memory using the configured test allocator.
 *
 * @param ptr The pointer to the memory to free, or NULL.
 */
void cn_test_free(void* ptr);

/**
 * Free all tracked allocations using the configured test allocator.
 */
void cn_test_free_all(void);

/**
 * Configure the test allocator with custom function pointers.
 *
 * @param data Context data to pass to the allocator functions.
 * @param malloc_fn Function pointer for malloc.
 * @param calloc_fn Function pointer for calloc.
 * @param realloc_fn Function pointer for realloc.
 * @param aligned_alloc_fn Function pointer for aligned_alloc.
 * @param free_fn Function pointer for free.
 * @param free_all_fn Function pointer for free_all.
 */
void cn_test_set_alloc(void* data,
    void* (*malloc_fn)(void* data, size_t size),
    void* (*calloc_fn)(void* data, size_t count, size_t size),
    void* (*realloc_fn)(void* data, void* ptr, size_t size),
    void* (*aligned_alloc_fn)(void* data, size_t alignment, size_t size),
    void (*free_fn)(void* data, void* ptr),
    void (*free_all_fn)(void* data));

#ifdef __cplusplus
}
#endif

#endif /* CN_TESTING_MEMORY_TEST_ALLOC_H */
