#include <assert.h>
#include <string.h>

#include <bennet/utils.h>
#include <bennet/utils/hash_table.h>
#include <cn-smt/memory/arena.h>
#include <cn-smt/memory/intern.h>
#include <cn-smt/memory/test_alloc.h>

// Type alias for const char* to use with hash table macros
typedef const char* const_char_ptr;

// Declare optional type for const_char_ptr
BENNET_OPTIONAL_DECL(const_char_ptr);

// Declare hash table type for string interning
// Maps string -> string (key is for lookup, value is the canonical pointer)
BENNET_HASH_TABLE_DECL(const_char_ptr, const_char_ptr)

// Implement hash table functions
BENNET_HASH_TABLE_IMPL(const_char_ptr, const_char_ptr)

/**
 * Internal structure for the intern table.
 */
struct cn_intern_table {
  bennet_hash_table(const_char_ptr, const_char_ptr) ht;
};

// Static global arena for string storage
static cn_arena* arena = NULL;

// Static global intern table
static struct cn_intern_table* table = NULL;

/**
 * Ensure the intern table is initialized.
 * This is called lazily on first use.
 */
static void ensure_initialized(void) {
  if (table == NULL) {
    // Create arena with default block size
    arena = cn_arena_create(0);
    assert(arena != NULL);

    // Allocate the table structure
    table = cn_test_malloc(sizeof(*table));
    assert(table != NULL);

    // Initialize the hash table with string hash and equality functions
    bennet_hash_table_init(const_char_ptr, const_char_ptr)(
        &table->ht, string_hash, string_equal);
  }
}

const char* cn_intern_string(const char* str) {
  assert(str != NULL);

  ensure_initialized();

  // Check if the string is already interned
  bennet_optional(const_char_ptr) existing =
      bennet_hash_table_get(const_char_ptr, const_char_ptr)(&table->ht, str);

  if (bennet_optional_is_some(existing)) {
    // String already interned, return the canonical pointer
    return bennet_optional_unwrap(existing);
  }

  // String not interned yet, allocate a copy in the arena
  size_t len = strlen(str);
  char* copy = cn_arena_malloc(arena, len + 1);
  if (copy == NULL) {
    return NULL;
  }
  memcpy(copy, str, len + 1);

  // Add to hash table (both key and value point to the same copy)
  bennet_hash_table_set(const_char_ptr, const_char_ptr)(&table->ht, copy, copy);

  return copy;
}

const char* cn_intern_lookup(const char* str) {
  assert(str != NULL);

  // If not initialized, nothing is interned
  if (table == NULL) {
    return NULL;
  }

  bennet_optional(const_char_ptr) existing =
      bennet_hash_table_get(const_char_ptr, const_char_ptr)(&table->ht, str);

  if (bennet_optional_is_some(existing)) {
    return bennet_optional_unwrap(existing);
  }

  return NULL;
}

size_t cn_intern_count(void) {
  if (table == NULL) {
    return 0;
  }
  return bennet_hash_table_size(const_char_ptr, const_char_ptr)(&table->ht);
}

void cn_intern_destroy(void) {
  if (table != NULL) {
    // Free the hash table entries
    bennet_hash_table_free(const_char_ptr, const_char_ptr)(&table->ht);

    // Free the table structure itself
    cn_test_free(table);
    table = NULL;
  }

  if (arena != NULL) {
    // Destroy the arena (this frees all interned strings)
    cn_arena_destroy(arena);
    arena = NULL;
  }
}
