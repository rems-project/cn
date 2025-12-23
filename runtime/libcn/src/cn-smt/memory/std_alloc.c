#include <assert.h>
#include <stdalign.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/hash_table.h>
#include <cn-smt/memory/std_alloc.h>
#include <cn-smt/memory/test_alloc.h>
#include <fulminate/api.h>

// Define hash function for void* pointers (hash the address)
static inline size_t bennet_hash_voidptr(void* ptr) {
  return (size_t)ptr;
}

// Define equality function for void* pointers (compare addresses)
static inline bool bennet_eq_voidptr(void* a, void* b) {
  return a == b;
}

// Use typedef to work with the bennet hash table macros
typedef void* voidptr;

// Declare and implement hash table for void* -> size_t
BENNET_HASH_TABLE_DECL(voidptr, size_t)
BENNET_HASH_TABLE_IMPL(voidptr, size_t)

// The allocator data structure
typedef struct {
  bennet_hash_table(voidptr, size_t) table;
} std_alloc_data;

// Global allocator instance
static std_alloc_data g_std_alloc;
static bool g_std_alloc_initialized = false;

// Lazy initialization
static void ensure_initialized(void) {
  if (!g_std_alloc_initialized) {
    bennet_hash_table_init(voidptr, size_t)(
        &g_std_alloc.table, bennet_hash_voidptr, bennet_eq_voidptr);
    g_std_alloc_initialized = true;
  }
}

//
// Implementation functions (_aux) - pure implementations with explicit data
//

static void* std_malloc_aux(std_alloc_data* data, size_t size) {
  void* ptr = malloc(size);
  if (ptr != NULL) {
    bennet_hash_table_set(voidptr, size_t)(&data->table, ptr, size);
  } else if (size != 0) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }
  return ptr;
}

static void* std_calloc_aux(std_alloc_data* data, size_t count, size_t size) {
  void* ptr = calloc(count, size);
  if (ptr != NULL) {
    bennet_hash_table_set(voidptr, size_t)(&data->table, ptr, count * size);
  } else if (count != 0 && size != 0) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }
  return ptr;
}

static void* std_realloc_aux(std_alloc_data* data, void* ptr, size_t size) {
  // If ptr is NULL, behave like malloc
  if (ptr == NULL) {
    return std_malloc_aux(data, size);
  }

  // Look up the old size before we delete the entry
  bennet_optional(size_t) old_size_opt =
      bennet_hash_table_get(voidptr, size_t)(&data->table, ptr);
  assert(bennet_optional_is_some(old_size_opt) &&
         "Attempted to realloc an untracked pointer");
  size_t old_size = bennet_optional_unwrap(old_size_opt);

  // Remove old pointer from tracking BEFORE realloc
  bool removed = bennet_hash_table_delete(voidptr, size_t)(&data->table, ptr);
  assert(removed);  // Should always succeed since we just got the value

  // Now call realloc - ptr should not be used after this point (except for comparison)
  void* new_ptr = realloc(ptr, size);

  if (new_ptr != NULL) {
    // Realloc succeeded: track new pointer with new size
    bennet_hash_table_set(voidptr, size_t)(&data->table, new_ptr, size);
  } else {
    // Realloc failed: original pointer is still valid, re-insert with old size
    bennet_hash_table_set(voidptr, size_t)(&data->table, ptr, old_size);
  }

  return new_ptr;
}

static void* std_aligned_alloc_aux(std_alloc_data* data, size_t alignment, size_t size) {
  void* ptr = aligned_alloc(alignment, size);
  if (ptr != NULL) {
    bennet_hash_table_set(voidptr, size_t)(&data->table, ptr, size);
  } else if (size != 0) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }
  return ptr;
}

static void std_free_aux(std_alloc_data* data, void* ptr) {
  // free(NULL) is a no-op
  if (ptr == NULL) {
    return;
  }

  // Remove from tracking - assert if not found (indicates freeing untracked
  // pointer)
  bool removed = bennet_hash_table_delete(voidptr, size_t)(&data->table, ptr);
  assert(removed && "Attempted to free an untracked pointer");

  // Free the actual memory
  free(ptr);
}

static void std_free_all_aux(std_alloc_data* data) {
  // Iterate through all entries in the hash table
  for (size_t i = 0; i < data->table.capacity; i++) {
    if (data->table.entries[i].occupied) {
      void* ptr = data->table.entries[i].key;
      free(ptr);
    }
  }

  // Clear the hash table (marks all entries as unoccupied)
  bennet_hash_table_clear(voidptr, size_t)(&data->table);
}

//
// Public API implementation
//

void* std_malloc(size_t size) {
  ensure_initialized();
  return std_malloc_aux(&g_std_alloc, size);
}

void* std_calloc(size_t count, size_t size) {
  ensure_initialized();
  return std_calloc_aux(&g_std_alloc, count, size);
}

void* std_realloc(void* ptr, size_t size) {
  ensure_initialized();
  return std_realloc_aux(&g_std_alloc, ptr, size);
}

void* std_aligned_alloc(size_t alignment, size_t size) {
  ensure_initialized();
  return std_aligned_alloc_aux(&g_std_alloc, alignment, size);
}

void std_free(void* ptr) {
  ensure_initialized();
  std_free_aux(&g_std_alloc, ptr);
}

void std_free_all(void) {
  ensure_initialized();
  std_free_all_aux(&g_std_alloc);
}

void std_set_default_alloc(void) {
  ensure_initialized();

  cn_test_set_alloc(&g_std_alloc,
      (void* (*)(void*, size_t))std_malloc_aux,
      (void* (*)(void*, size_t, size_t))std_calloc_aux,
      (void* (*)(void*, void*, size_t))std_realloc_aux,
      (void* (*)(void*, size_t, size_t))std_aligned_alloc_aux,
      (void (*)(void*, void*))std_free_aux,
      (void (*)(void*))std_free_all_aux);
}
