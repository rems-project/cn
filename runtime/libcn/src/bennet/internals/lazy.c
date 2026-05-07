#include <assert.h>
#include <stdbool.h>
#include <stdint.h>

#include <bennet/internals/lazy.h>
#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>

typedef void* void_ptr;

// Use uint8_t instead of bool to avoid macro expansion issues with _Bool
typedef uint8_t use_flag;

static inline size_t void_ptr_hash(void_ptr ptr) {
  return (size_t)ptr;
}

static inline bool void_ptr_equal(void_ptr a, void_ptr b) {
  return a == b;
}

// Declare optional type for use_flag (needed by hash table)
BENNET_OPTIONAL_DECL(use_flag);

BENNET_HASH_TABLE_DECL(void_ptr, use_flag)
BENNET_HASH_TABLE_IMPL(void_ptr, use_flag)

static bennet_hash_table(void_ptr, use_flag) lazy_table;
static bool lazy_initialized = false;

void bennet_lazy_new(void* ptr) {
  if (!lazy_initialized) {
    bennet_hash_table_init(void_ptr, use_flag)(
        &lazy_table, void_ptr_hash, void_ptr_equal);
    lazy_initialized = true;
  }
  bennet_hash_table_set(void_ptr, use_flag)(&lazy_table, ptr, false);
}

bool bennet_lazy_mark(void* ptr) {
  assert(lazy_initialized);
  assert(bennet_hash_table_contains(void_ptr, use_flag)(&lazy_table, ptr));
  use_flag current =
      bennet_optional_unwrap(bennet_hash_table_get(void_ptr, use_flag)(&lazy_table, ptr));

  if (current == 0) {
    bennet_hash_table_set(void_ptr, use_flag)(&lazy_table, ptr, true);
    return true;  // Value was updated (time to generate lazily)
  }

  return false;  // Already marked, no update needed
}

void bennet_lazy_unmark(void* ptr) {
  assert(lazy_initialized);
  assert(bennet_hash_table_contains(void_ptr, use_flag)(&lazy_table, ptr));
  use_flag current =
      bennet_optional_unwrap(bennet_hash_table_get(void_ptr, use_flag)(&lazy_table, ptr));
  assert(current != 0);
  bennet_hash_table_set(void_ptr, use_flag)(&lazy_table, ptr, false);
}

bool bennet_lazy_is_instantiated(void* ptr) {
  if (!lazy_initialized ||
      !bennet_hash_table_contains(void_ptr, use_flag)(&lazy_table, ptr)) {
    // Not lazy, so must be concrete
    return true;
  }

  use_flag current =
      bennet_optional_unwrap(bennet_hash_table_get(void_ptr, use_flag)(&lazy_table, ptr));
  return current != 0;
}

void bennet_lazy_reset(void) {
  if (lazy_initialized) {
    bennet_hash_table_free(void_ptr, use_flag)(&lazy_table);
    lazy_initialized = false;
  }
}
