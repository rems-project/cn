#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

#include <cn-smt/memory/test_alloc.h>

/**
 * Internal allocator structure holding function pointers and context data.
 */
static struct {
  void* data;
  void* (*malloc)(void* data, size_t size);
  void* (*calloc)(void* data, size_t count, size_t size);
  void* (*realloc)(void* data, void* ptr, size_t size);
  void* (*aligned_alloc)(void* data, size_t alignment, size_t size);
  void (*free)(void* data, void* ptr);
  void (*free_all)(void* data);
} cn_test_allocator = {
    .data = NULL,
    .malloc = NULL,
    .calloc = NULL,
    .realloc = NULL,
    .aligned_alloc = NULL,
    .free = NULL,
    .free_all = NULL,
};

void* cn_test_malloc(size_t size) {
  assert(cn_test_allocator.malloc != NULL);
  return cn_test_allocator.malloc(cn_test_allocator.data, size);
}

void* cn_test_calloc(size_t count, size_t size) {
  assert(cn_test_allocator.calloc != NULL);
  return cn_test_allocator.calloc(cn_test_allocator.data, count, size);
}

void* cn_test_realloc(void* ptr, size_t size) {
  assert(cn_test_allocator.realloc != NULL);
  return cn_test_allocator.realloc(cn_test_allocator.data, ptr, size);
}

void* cn_test_aligned_alloc(size_t alignment, size_t size) {
  assert(cn_test_allocator.aligned_alloc != NULL);
  return cn_test_allocator.aligned_alloc(cn_test_allocator.data, alignment, size);
}

void cn_test_free(void* ptr) {
  assert(cn_test_allocator.free != NULL);
  cn_test_allocator.free(cn_test_allocator.data, ptr);
}

void cn_test_free_all(void) {
  assert(cn_test_allocator.free_all != NULL);
  cn_test_allocator.free_all(cn_test_allocator.data);
}

void cn_test_set_alloc(void* data,
    void* (*malloc_fn)(void* data, size_t size),
    void* (*calloc_fn)(void* data, size_t count, size_t size),
    void* (*realloc_fn)(void* data, void* ptr, size_t size),
    void* (*aligned_alloc_fn)(void* data, size_t alignment, size_t size),
    void (*free_fn)(void* data, void* ptr),
    void (*free_all_fn)(void* data)) {
  cn_test_allocator.data = data;
  cn_test_allocator.malloc = malloc_fn;
  cn_test_allocator.calloc = calloc_fn;
  cn_test_allocator.realloc = realloc_fn;
  cn_test_allocator.aligned_alloc = aligned_alloc_fn;
  cn_test_allocator.free = free_fn;
  cn_test_allocator.free_all = free_all_fn;
}
