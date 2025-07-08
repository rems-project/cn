#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet-exp/info/backtracks.h>
#include <bennet-exp/utils/hash_table.h>

// String hash and equality functions
static size_t string_hash(const char* str) {
  size_t hash = 5381;
  int c;
  while ((c = *str++)) {
    hash = ((hash << 5) + hash) + c;  // hash * 33 + c
  }
  return hash;
}

static bool string_equal(const char* a, const char* b) {
  return strcmp(a, b) == 0;
}

// Structure to represent a location (filename + line number)
typedef struct {
  const char* filename;
  int line_number;
} location_key_t;

// Hash and equality functions for location_key_t
static size_t location_hash(location_key_t key) {
  size_t hash = string_hash(key.filename);
  hash = ((hash << 5) + hash) + (size_t)key.line_number;
  return hash;
}

static bool location_equal(location_key_t a, location_key_t b) {
  return string_equal(a.filename, b.filename) && a.line_number == b.line_number;
}

typedef const char* const_str;
typedef void* pointer;

// Declare hash table types
#define bennet_info_backtrack_locations bennet_hash_table(const_str, pointer)
#define bennet_info_backtrack_locations_counter                                          \
  bennet_hash_table(location_key_t, uint64_t)

#define bennet_info_backtrack_generators         bennet_hash_table(const_str, pointer)
#define bennet_info_backtrack_generators_counter bennet_hash_table(const_str, uint64_t)

BENNET_HASH_TABLE_DECL(const_str, pointer);
BENNET_HASH_TABLE_DECL(const_str, uint64_t);
BENNET_HASH_TABLE_DECL(location_key_t, uint64_t);

BENNET_OPTIONAL_DECL(pointer);
BENNET_HASH_TABLE_IMPL(const_str, pointer);
BENNET_HASH_TABLE_IMPL(const_str, uint64_t);
BENNET_HASH_TABLE_IMPL(location_key_t, uint64_t);

// Global state
static bool initialized = false;
static char* current_function = NULL;
static bennet_info_backtrack_generators function_to_generators;
static bennet_info_backtrack_locations generator_to_locations;

// Initialize the global state
void bennet_info_backtracks_init(void) {
  if (!initialized) {
    bennet_hash_table_init(const_str, pointer)(
        &function_to_generators, string_hash, string_equal);
    bennet_hash_table_init(const_str, pointer)(
        &generator_to_locations, string_hash, string_equal);
    initialized = true;
  }
}

void bennet_info_backtracks_set_function_under_test(const char* function_name) {
  if (!initialized) {
    return;
  }

  assert(function_name);
  current_function = strdup(function_name);
}

void bennet_info_backtracks_log(
    const char* generator, const char* filename, int line_number) {
  if (!initialized) {
    return;
  }

  assert(current_function && generator && filename);

  // Get or create generator counter for this function
  bennet_optional(pointer) gen_counter_opt = bennet_hash_table_get(const_str, pointer)(
      &function_to_generators, current_function);

  bennet_info_backtrack_generators_counter* gen_counter;
  if (bennet_optional_is_none(gen_counter_opt)) {
    // Create new counter for this function
    gen_counter = malloc(sizeof(bennet_info_backtrack_generators_counter));
    bennet_hash_table_init(const_str, uint64_t)(gen_counter, string_hash, string_equal);
    bennet_hash_table_set(const_str, pointer)(
        &function_to_generators, current_function, gen_counter);
  } else {
    gen_counter = (bennet_info_backtrack_generators_counter*)bennet_optional_unwrap(
        gen_counter_opt);
  }

  // Increment generator counter
  bennet_optional(uint64_t) count_opt =
      bennet_hash_table_get(const_str, uint64_t)(gen_counter, generator);
  uint64_t current_count =
      bennet_optional_is_some(count_opt) ? bennet_optional_unwrap(count_opt) : 0;
  bennet_hash_table_set(const_str, uint64_t)(gen_counter, generator, current_count + 1);

  // Get or create location counter for this generator
  bennet_optional(pointer) loc_counter_opt =
      bennet_hash_table_get(const_str, pointer)(&generator_to_locations, generator);

  bennet_info_backtrack_locations_counter* loc_counter;
  if (bennet_optional_is_none(loc_counter_opt)) {
    // Create new counter for this generator
    loc_counter = malloc(sizeof(bennet_info_backtrack_locations_counter));
    bennet_hash_table_init(location_key_t, uint64_t)(
        loc_counter, location_hash, location_equal);
    bennet_hash_table_set(const_str, pointer)(
        &generator_to_locations, generator, loc_counter);
  } else {
    loc_counter =
        (bennet_info_backtrack_locations_counter*)bennet_optional_unwrap(loc_counter_opt);
  }

  // Increment location counter
  location_key_t loc_key;
  loc_key.filename = strdup(filename);
  loc_key.line_number = line_number;

  bennet_optional(uint64_t) loc_count_opt =
      bennet_hash_table_get(location_key_t, uint64_t)(loc_counter, loc_key);
  uint64_t current_loc_count =
      bennet_optional_is_some(loc_count_opt) ? bennet_optional_unwrap(loc_count_opt) : 0;
  bennet_hash_table_set(location_key_t, uint64_t)(
      loc_counter, loc_key, current_loc_count + 1);
}

void bennet_info_backtracks_print_backtrack_info(void) {
  if (!initialized) {
    return;
  }

  printf("=== BACKTRACKING STATISTICS ===\n\n");

  printf("====================\n");
  printf("FUNCTIONS UNDER TEST\n");
  printf("====================\n\n");

  // Iterate through all functions and their generator counters
  for (size_t i = 0; i < function_to_generators.capacity; ++i) {
    if (function_to_generators.entries[i].occupied) {
      const char* function_name = function_to_generators.entries[i].key;
      bennet_info_backtrack_generators_counter* gen_counter =
          (bennet_info_backtrack_generators_counter*)function_to_generators.entries[i]
              .value;

      // Calculate total backtracks for this function
      uint64_t total_backtracks = 0;
      for (size_t j = 0; j < gen_counter->capacity; ++j) {
        if (gen_counter->entries[j].occupied) {
          total_backtracks += gen_counter->entries[j].value;
        }
      }

      printf("%s: %" PRIu64 " backtracks\n", function_name, total_backtracks);

      // Print individual generator counts
      for (size_t j = 0; j < gen_counter->capacity; ++j) {
        if (gen_counter->entries[j].occupied) {
          printf("  %s: %.0f%% (%" PRIu64 " backtracks)\n",
              gen_counter->entries[j].key,
              (double)gen_counter->entries[j].value / (double)total_backtracks * 100,
              gen_counter->entries[j].value);
        }
      }
      printf("\n");
    }
  }

  printf("===========\n");
  printf("GENERATORS\n");
  printf("===========\n\n");

  // Iterate through all generators and their location counters
  for (size_t i = 0; i < generator_to_locations.capacity; ++i) {
    if (generator_to_locations.entries[i].occupied) {
      const char* generator_name = generator_to_locations.entries[i].key;
      bennet_info_backtrack_locations_counter* loc_counter =
          (bennet_info_backtrack_locations_counter*)generator_to_locations.entries[i]
              .value;

      // Calculate total backtracks for this generator
      uint64_t total_backtracks = 0;
      for (size_t j = 0; j < loc_counter->capacity; ++j) {
        if (loc_counter->entries[j].occupied) {
          total_backtracks += loc_counter->entries[j].value;
        }
      }

      printf("%s: %" PRIu64 "\n", generator_name, total_backtracks);

      // Print location counts
      for (size_t j = 0; j < loc_counter->capacity; ++j) {
        if (loc_counter->entries[j].occupied) {
          location_key_t loc_key = loc_counter->entries[j].key;
          uint64_t count = loc_counter->entries[j].value;
          printf("  %s:%d: %.0f%% (%" PRIu64 " backtracks)\n",
              loc_key.filename,
              loc_key.line_number,
              (double)loc_counter->entries[j].value / (double)total_backtracks * 100,
              count);
        }
      }
      printf("\n");
    }
  }
}
