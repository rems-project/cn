#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/memory/test_alloc.h>

/**
 * Stack node for saving allocator configurations.
 * Uses system malloc/free (not test allocator) to avoid recursion.
 */
struct cn_test_allocator_node {
  void* data;
  void* (*malloc)(void* data, size_t size);
  void* (*calloc)(void* data, size_t count, size_t size);
  void* (*realloc)(void* data, void* ptr, size_t size);
  void* (*aligned_alloc)(void* data, size_t alignment, size_t size);
  void (*free)(void* data, void* ptr);
  void (*free_all)(void* data);
  struct cn_test_allocator_node* next;
};

/**
 * Stack head for pushed allocator configurations.
 */
static struct cn_test_allocator_node* cn_test_allocator_stack = NULL;

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

char* cn_test_strdup(const char* str) {
  assert(str != NULL);
  size_t len = strlen(str) + 1;
  char* dup = (char*)cn_test_malloc(len);
  assert(dup != NULL);
  strcpy(dup, str);
  return dup;
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
  // Clear the entire stack
  while (cn_test_allocator_stack != NULL) {
    struct cn_test_allocator_node* node = cn_test_allocator_stack;
    cn_test_allocator_stack = node->next;
    free(node);
  }

  // Set the new allocator
  cn_test_allocator.data = data;
  cn_test_allocator.malloc = malloc_fn;
  cn_test_allocator.calloc = calloc_fn;
  cn_test_allocator.realloc = realloc_fn;
  cn_test_allocator.aligned_alloc = aligned_alloc_fn;
  cn_test_allocator.free = free_fn;
  cn_test_allocator.free_all = free_all_fn;
}

void cn_test_push_alloc(void* data,
    void* (*malloc_fn)(void* data, size_t size),
    void* (*calloc_fn)(void* data, size_t count, size_t size),
    void* (*realloc_fn)(void* data, void* ptr, size_t size),
    void* (*aligned_alloc_fn)(void* data, size_t alignment, size_t size),
    void (*free_fn)(void* data, void* ptr),
    void (*free_all_fn)(void* data)) {
  // Allocate new stack node using system malloc
  struct cn_test_allocator_node* node =
      (struct cn_test_allocator_node*)malloc(sizeof(struct cn_test_allocator_node));
  assert(node != NULL);

  // Save current allocator state to the node
  node->data = cn_test_allocator.data;
  node->malloc = cn_test_allocator.malloc;
  node->calloc = cn_test_allocator.calloc;
  node->realloc = cn_test_allocator.realloc;
  node->aligned_alloc = cn_test_allocator.aligned_alloc;
  node->free = cn_test_allocator.free;
  node->free_all = cn_test_allocator.free_all;

  // Link to stack
  node->next = cn_test_allocator_stack;
  cn_test_allocator_stack = node;

  // Set new allocator
  cn_test_allocator.data = data;
  cn_test_allocator.malloc = malloc_fn;
  cn_test_allocator.calloc = calloc_fn;
  cn_test_allocator.realloc = realloc_fn;
  cn_test_allocator.aligned_alloc = aligned_alloc_fn;
  cn_test_allocator.free = free_fn;
  cn_test_allocator.free_all = free_all_fn;
}

void cn_test_pop_alloc(void) {
  // Assert stack is not empty
  assert(cn_test_allocator_stack != NULL);

  // Get the top node
  struct cn_test_allocator_node* node = cn_test_allocator_stack;

  // Restore allocator state from the node
  cn_test_allocator.data = node->data;
  cn_test_allocator.malloc = node->malloc;
  cn_test_allocator.calloc = node->calloc;
  cn_test_allocator.realloc = node->realloc;
  cn_test_allocator.aligned_alloc = node->aligned_alloc;
  cn_test_allocator.free = node->free;
  cn_test_allocator.free_all = node->free_all;

  // Pop the node from stack
  cn_test_allocator_stack = node->next;

  // Free the node using system free
  free(node);
}

void* cn_test_move_to_prev(void* ptr, size_t size) {
  // Assert there is a previous allocator on the stack
  assert(cn_test_allocator_stack != NULL);

  // Allocate in the previous allocator
  void* new_ptr = cn_test_allocator_stack->malloc(cn_test_allocator_stack->data, size);
  assert(new_ptr != NULL);

  // Copy data from old to new allocation
  memcpy(new_ptr, ptr, size);

  // Free original allocation in current allocator
  cn_test_allocator.free(cn_test_allocator.data, ptr);

  // Return new pointer
  return new_ptr;
}
