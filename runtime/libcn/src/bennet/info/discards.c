#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/info/discards.h>
#include <bennet/state/failure.h>
#include <bennet/utils.h>
#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>

#define MAX_PRINTED 10

// Typedefs for hash table macros
typedef const char* const_str;
typedef void* pointer;
typedef enum bennet_failure_type failure_type_t;

// Hash and equality functions for failure_type_t
static size_t failure_type_hash(failure_type_t key) {
  return (size_t)key;
}

static bool failure_type_equal(failure_type_t a, failure_type_t b) {
  return a == b;
}

// Hash table types
#define bennet_info_discards_table       bennet_hash_table(failure_type_t, uint64_t)
#define bennet_info_function_to_discards bennet_hash_table(const_str, pointer)

BENNET_HASH_TABLE_DECL(failure_type_t, uint64_t);
BENNET_HASH_TABLE_DECL(const_str, pointer);
BENNET_OPTIONAL_DECL(pointer);
BENNET_HASH_TABLE_IMPL(failure_type_t, uint64_t);
BENNET_HASH_TABLE_IMPL(const_str, pointer);

// Helper struct for sorting discard counts
typedef struct {
  enum bennet_failure_type failure_type;
  uint64_t count;
} discard_count_entry_t;

static int compare_discard_count_desc(const void* a, const void* b) {
  const discard_count_entry_t* ea = (const discard_count_entry_t*)a;
  const discard_count_entry_t* eb = (const discard_count_entry_t*)b;
  if (ea->count < eb->count) {
    return 1;
  }

  if (ea->count > eb->count) {
    return -1;
  }

  return 0;
}

// Helper function to convert failure type to string
static const char* failure_type_to_string(enum bennet_failure_type type) {
  switch (type) {
    case BENNET_FAILURE_NONE:
      return "NONE";
    case BENNET_FAILURE_ASSERT:
      return "ASSERT";
    case BENNET_FAILURE_ASSIGN:
      return "ASSIGN";
    case BENNET_FAILURE_DEPTH:
      return "DEPTH";
    case BENNET_FAILURE_TIMEOUT:
      return "TIMEOUT";
    case BENNET_FAILURE_UNSAT:
      return "UNSAT";
    default:
      assert(false);
      return NULL;
  }
}

// Global state
static bool initialized = false;
static const char* current_function = NULL;
static bennet_info_function_to_discards function_to_discards;

void bennet_info_discards_init(void) {
  if (!initialized) {
    bennet_hash_table_init(const_str, pointer)(
        &function_to_discards, string_hash, string_equal);
    initialized = true;
  }
}

void bennet_info_discards_set_function_under_test(const char* function_name) {
  if (!initialized) {
    return;
  }

  assert(function_name);
  current_function = function_name;

  // Insert an empty table into `function_to_discards`
  // if `current_function` doesn't exist
  if (current_function) {
    bennet_optional(pointer) discard_table_opt = bennet_hash_table_get(
        const_str, pointer)(&function_to_discards, current_function);

    if (bennet_optional_is_none(discard_table_opt)) {
      // Create new empty counter for this function
      bennet_info_discards_table* discard_table =
          malloc(sizeof(bennet_info_discards_table));
      bennet_hash_table_init(failure_type_t, uint64_t)(
          discard_table, failure_type_hash, failure_type_equal);
      bennet_hash_table_set(const_str, pointer)(
          &function_to_discards, current_function, discard_table);
    }
  }
}

void bennet_info_discards_log(enum bennet_failure_type failure_type) {
  if (!initialized || !current_function) {
    return;
  }

  // Get or create discard table for current function
  bennet_info_discards_table* discard_table = bennet_optional_unwrap(
      bennet_hash_table_get(const_str, pointer)(&function_to_discards, current_function));

  // Increment count for this failure type
  bennet_optional(uint64_t) count_opt =
      bennet_hash_table_get(failure_type_t, uint64_t)(discard_table, failure_type);

  uint64_t current_count = bennet_optional_unwrap_or(uint64_t)(&count_opt, 0);
  bennet_hash_table_set(failure_type_t, uint64_t)(
      discard_table, failure_type, current_count + 1);
}

void bennet_info_discards_print_info(void) {
  if (!initialized) {
    return;
  }

  printf("=== DISCARD STATISTICS ===\n\n");
  printf("====================\n");
  printf("FUNCTIONS UNDER TEST\n");
  printf("====================\n\n");

  // Iterate through all functions and their discard tables
  for (size_t i = 0; i < function_to_discards.capacity; ++i) {
    if (!function_to_discards.entries[i].occupied) {
      continue;
    }

    const char* function_name = function_to_discards.entries[i].key;
    bennet_info_discards_table* discard_table =
        (bennet_info_discards_table*)function_to_discards.entries[i].value;

    // Calculate total discards and total count for this function
    size_t failure_type_count = 0;
    uint64_t total_count = 0;
    for (size_t j = 0; j < discard_table->capacity; ++j) {
      if (discard_table->entries[j].occupied) {
        failure_type_count++;
        total_count += discard_table->entries[j].value;
      }
    }

    printf("%s: %" PRIu64 " discards\n", function_name, total_count);
    if (total_count == 0) {
      continue;
    }

    // Collect failure type/count pairs for sorting
    discard_count_entry_t* entries =
        malloc(failure_type_count * sizeof(discard_count_entry_t));
    size_t idx = 0;
    for (size_t j = 0; j < discard_table->capacity; ++j) {
      if (discard_table->entries[j].occupied) {
        entries[idx].failure_type = discard_table->entries[j].key;
        entries[idx].count = discard_table->entries[j].value;
        idx++;
      }
    }

    // Sort descending by count
    qsort(entries,
        failure_type_count,
        sizeof(discard_count_entry_t),
        compare_discard_count_desc);

    if (failure_type_count > MAX_PRINTED) {
      failure_type_count = MAX_PRINTED;
    }

    // Print sorted discard counts
    for (size_t j = 0; j < failure_type_count; ++j) {
      double percent = (double)entries[j].count / (double)total_count * 100.0;
      if (percent < 1.0) {
        continue;
      }

      printf("  %s: %.0f%% (%" PRIu64 " discards)\n",
          failure_type_to_string(entries[j].failure_type),
          percent,
          entries[j].count);
    }
    free(entries);
    printf("\n");
  }
}
