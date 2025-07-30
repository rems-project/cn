#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/info/sizes.h>
#include <bennet/state/alloc.h>
#include <bennet/utils.h>
#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>

#define MAX_PRINTED 10

// Typedefs for hash table macros
typedef const char* const_str;
typedef void* pointer;

// Hash table types
#define bennet_info_sizes_table       bennet_hash_table(size_t, size_t)
#define bennet_info_function_to_sizes bennet_hash_table(const_str, pointer)

BENNET_HASH_TABLE_DECL(size_t, size_t);
BENNET_HASH_TABLE_DECL(const_str, pointer);
BENNET_OPTIONAL_DECL(pointer);
BENNET_HASH_TABLE_IMPL(size_t, size_t);
BENNET_HASH_TABLE_IMPL(const_str, pointer);

// Helper struct for sorting size counts
typedef struct {
  size_t size;
  size_t count;
} size_count_entry_t;

static int compare_size_count_desc(const void* a, const void* b) {
  const size_count_entry_t* ea = (const size_count_entry_t*)a;
  const size_count_entry_t* eb = (const size_count_entry_t*)b;
  if (ea->count < eb->count) {
    return 1;
  }

  if (ea->count > eb->count) {
    return -1;
  }

  return 0;
}

// Global state
static bool initialized = false;
static const char* current_function = NULL;
static bennet_info_function_to_sizes function_to_sizes;

void bennet_info_sizes_init(void) {
  if (!initialized) {
    bennet_hash_table_init(const_str, pointer)(
        &function_to_sizes, string_hash, string_equal);
    initialized = true;
  }
}

void bennet_info_sizes_set_function_under_test(const char* function_name) {
  if (!initialized) {
    return;
  }

  assert(function_name);
  current_function = function_name;

  // Insert an empty table into `function_to_generators`
  // if `current_function` doesn't exist
  if (current_function) {
    bennet_optional(pointer) size_table_opt =
        bennet_hash_table_get(const_str, pointer)(&function_to_sizes, current_function);

    if (bennet_optional_is_none(size_table_opt)) {
      // Create new empty counter for this function
      bennet_info_sizes_table* size_table = malloc(sizeof(bennet_info_sizes_table));
      bennet_hash_table_init(size_t, size_t)(
          size_table, bennet_hash_size_t, bennet_eq_size_t);
      bennet_hash_table_set(const_str, pointer)(
          &function_to_sizes, current_function, size_table);
    }
  }
}

static size_t last_size;

void bennet_info_sizes_log(void) {
  if (!initialized || !current_function) {
    return;
  }

  // Get or create size table for current function in temporary table
  bennet_info_sizes_table* size_table = bennet_optional_unwrap(
      bennet_hash_table_get(const_str, pointer)(&function_to_sizes, current_function));

  // Increment count for this size
  last_size = bennet_ownership_size();
  bennet_optional(size_t) count_opt =
      bennet_hash_table_get(size_t, size_t)(size_table, last_size);

  size_t current_count = bennet_optional_unwrap_or(size_t)(&count_opt, 0);
  bennet_hash_table_set(size_t, size_t)(size_table, last_size, current_count + 1);
}

size_t bennet_info_sizes_last_size(void) {
  return last_size;
}

void bennet_info_sizes_print_info(void) {
  if (!initialized) {
    return;
  }

  printf("=== SIZE STATISTICS ===\n\n");
  printf("====================\n");
  printf("FUNCTIONS UNDER TEST\n");
  printf("====================\n\n");

  // Iterate through all functions and their size tables
  for (size_t i = 0; i < function_to_sizes.capacity; ++i) {
    if (!function_to_sizes.entries[i].occupied) {
      continue;
    }

    const char* function_name = function_to_sizes.entries[i].key;
    bennet_info_sizes_table* size_table =
        (bennet_info_sizes_table*)function_to_sizes.entries[i].value;

    // Calculate total sizes and total count for this function
    size_t size_count = 0;
    size_t total_count = 0;
    for (size_t j = 0; j < size_table->capacity; ++j) {
      if (size_table->entries[j].occupied) {
        size_count++;
        total_count += size_table->entries[j].value;
      }
    }

    printf("%s: %zu inputs\n", function_name, total_count);
    if (total_count == 0) {
      continue;
    }

    // Collect size/count pairs for sorting
    size_count_entry_t* entries = malloc(size_count * sizeof(size_count_entry_t));
    size_t idx = 0;
    for (size_t j = 0; j < size_table->capacity; ++j) {
      if (size_table->entries[j].occupied) {
        entries[idx].size = size_table->entries[j].key;
        entries[idx].count = size_table->entries[j].value;
        idx++;
      }
    }

    // Sort descending by count
    qsort(entries, size_count, sizeof(size_count_entry_t), compare_size_count_desc);

    if (size_count > MAX_PRINTED) {
      size_count = MAX_PRINTED;
    }

    // Print sorted size counts
    for (size_t j = 0; j < size_count; ++j) {
      double percent = (double)entries[j].count / (double)total_count * 100.0;
      if (percent < 1.0) {
        continue;
      }

      printf("  %zu: %.0f%% (%zu inputs)\n", entries[j].size, percent, entries[j].count);
    }
    free(entries);
    printf("\n");
  }
}
