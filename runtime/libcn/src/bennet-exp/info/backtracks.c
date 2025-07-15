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
static const char* current_function = NULL;
static bennet_info_backtrack_generators function_to_generators;
static bennet_info_backtrack_locations generator_to_locations;

static bennet_info_backtrack_generators function_to_generators_tmp;
static bennet_info_backtrack_locations generator_to_locations_tmp;

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
  current_function = function_name;
}

void bennet_info_backtracks_begin_run(void) {
  if (!initialized) {
    return;
  }

  bennet_hash_table_init(const_str, pointer)(
      &function_to_generators_tmp, string_hash, string_equal);
  bennet_hash_table_init(const_str, pointer)(
      &generator_to_locations_tmp, string_hash, string_equal);

  // Insert an empty table into `function_to_generators`
  // if `current_function` doesn't exist
  if (current_function && initialized) {
    bennet_optional(pointer) gen_counter_opt = bennet_hash_table_get(const_str, pointer)(
        &function_to_generators, current_function);

    if (bennet_optional_is_none(gen_counter_opt)) {
      // Create new empty counter for this function
      bennet_info_backtrack_generators_counter* gen_counter =
          malloc(sizeof(bennet_info_backtrack_generators_counter));
      bennet_hash_table_init(const_str, uint64_t)(gen_counter, string_hash, string_equal);
      bennet_hash_table_set(const_str, pointer)(
          &function_to_generators, current_function, gen_counter);
    }
  }
}

static uint64_t last_backtrack_counts;

uint64_t bennet_info_backtracks_last_total(void) {
  return last_backtrack_counts;
}

void bennet_info_backtracks_end_run(bool record) {
  if (!initialized) {
    return;
  }

  last_backtrack_counts = 0;
  for (size_t i = 0; i < function_to_generators_tmp.capacity; ++i) {
    if (function_to_generators_tmp.entries[i].occupied) {
      const char* function_name = function_to_generators_tmp.entries[i].key;

      bennet_optional(pointer) gen_counter_opt = bennet_hash_table_get(
          const_str, pointer)(&function_to_generators_tmp, function_name);

      if (bennet_optional_is_none(gen_counter_opt)) {
        continue;
      }

      bennet_info_backtrack_generators_counter* gen_counter =
          bennet_optional_unwrap(gen_counter_opt);
      for (size_t j = 0; j < gen_counter->capacity; ++j) {
        if (gen_counter->entries[j].occupied) {
          last_backtrack_counts += gen_counter->entries[j].value;
        }
      }
    }
  }

  if (!record) {
    // Insert empty tables into generator_to_locations for any generators
    // that were encountered in the temporary table but don't exist in the permanent table
    for (size_t i = 0; i < generator_to_locations_tmp.capacity; ++i) {
      if (generator_to_locations_tmp.entries[i].occupied) {
        const char* generator_name = generator_to_locations_tmp.entries[i].key;

        // Check if this generator already exists in the permanent table
        bennet_optional(pointer) loc_counter_opt = bennet_hash_table_get(
            const_str, pointer)(&generator_to_locations, generator_name);

        if (bennet_optional_is_none(loc_counter_opt)) {
          // Create new empty counter for this generator
          bennet_info_backtrack_locations_counter* loc_counter =
              malloc(sizeof(bennet_info_backtrack_locations_counter));
          bennet_hash_table_init(location_key_t, uint64_t)(
              loc_counter, location_hash, location_equal);
          bennet_hash_table_set(const_str, pointer)(
              &generator_to_locations, generator_name, loc_counter);
        }
      }
    }

    bennet_hash_table_free(const_str, pointer)(&function_to_generators_tmp);
    bennet_hash_table_free(const_str, pointer)(&generator_to_locations_tmp);
    return;
  }

  // Merge function_to_generators_tmp into function_to_generators
  for (size_t i = 0; i < function_to_generators_tmp.capacity; ++i) {
    if (function_to_generators_tmp.entries[i].occupied) {
      const char* function_name = function_to_generators_tmp.entries[i].key;
      bennet_info_backtrack_generators_counter* tmp_gen_counter =
          (bennet_info_backtrack_generators_counter*)function_to_generators_tmp.entries[i]
              .value;

      // Get or create generator counter for this function in the permanent table
      bennet_optional(pointer) gen_counter_opt = bennet_hash_table_get(
          const_str, pointer)(&function_to_generators, function_name);

      bennet_info_backtrack_generators_counter* gen_counter;
      if (bennet_optional_is_none(gen_counter_opt)) {
        // Create new counter for this function
        gen_counter = malloc(sizeof(bennet_info_backtrack_generators_counter));
        bennet_hash_table_init(const_str, uint64_t)(
            gen_counter, string_hash, string_equal);
        bennet_hash_table_set(const_str, pointer)(
            &function_to_generators, function_name, gen_counter);
      } else {
        gen_counter = (bennet_info_backtrack_generators_counter*)bennet_optional_unwrap(
            gen_counter_opt);
      }

      // Merge generator counts from temporary to permanent
      for (size_t j = 0; j < tmp_gen_counter->capacity; ++j) {
        if (tmp_gen_counter->entries[j].occupied) {
          const char* generator = tmp_gen_counter->entries[j].key;
          uint64_t tmp_count = tmp_gen_counter->entries[j].value;

          // Get current count in permanent table
          bennet_optional(uint64_t) count_opt =
              bennet_hash_table_get(const_str, uint64_t)(gen_counter, generator);
          uint64_t current_count = bennet_optional_unwrap_or(uint64_t)(&count_opt, 0);

          // Add temporary count to permanent count
          bennet_hash_table_set(const_str, uint64_t)(
              gen_counter, generator, current_count + tmp_count);
        }
      }
    }
  }

  // Merge generator_to_locations_tmp into generator_to_locations
  for (size_t i = 0; i < generator_to_locations_tmp.capacity; ++i) {
    if (generator_to_locations_tmp.entries[i].occupied) {
      const char* generator_name = generator_to_locations_tmp.entries[i].key;
      bennet_info_backtrack_locations_counter* tmp_loc_counter =
          (bennet_info_backtrack_locations_counter*)generator_to_locations_tmp.entries[i]
              .value;

      // Get or create location counter for this generator in the permanent table
      bennet_optional(pointer) loc_counter_opt = bennet_hash_table_get(
          const_str, pointer)(&generator_to_locations, generator_name);

      bennet_info_backtrack_locations_counter* loc_counter;
      if (bennet_optional_is_none(loc_counter_opt)) {
        // Create new counter for this generator
        loc_counter = malloc(sizeof(bennet_info_backtrack_locations_counter));
        bennet_hash_table_init(location_key_t, uint64_t)(
            loc_counter, location_hash, location_equal);
        bennet_hash_table_set(const_str, pointer)(
            &generator_to_locations, generator_name, loc_counter);
      } else {
        loc_counter = (bennet_info_backtrack_locations_counter*)bennet_optional_unwrap(
            loc_counter_opt);
      }

      // Merge location counts from temporary to permanent
      for (size_t j = 0; j < tmp_loc_counter->capacity; ++j) {
        if (tmp_loc_counter->entries[j].occupied) {
          location_key_t loc_key = tmp_loc_counter->entries[j].key;
          uint64_t tmp_count = tmp_loc_counter->entries[j].value;

          // Get current count in permanent table
          bennet_optional(uint64_t) count_opt =
              bennet_hash_table_get(location_key_t, uint64_t)(loc_counter, loc_key);
          uint64_t current_count =
              bennet_optional_is_some(count_opt) ? bennet_optional_unwrap(count_opt) : 0;

          // Add temporary count to permanent count
          bennet_hash_table_set(location_key_t, uint64_t)(
              loc_counter, loc_key, current_count + tmp_count);
        }
      }
    }
  }

  // Free temporary tables
  bennet_hash_table_free(const_str, pointer)(&function_to_generators_tmp);
  bennet_hash_table_free(const_str, pointer)(&generator_to_locations_tmp);
}

void bennet_info_backtracks_log(
    const char* generator, const char* filename, int line_number) {
  if (!initialized) {
    return;
  }

  assert(current_function && generator && filename);

  // Get or create generator counter for this function
  bennet_optional(pointer) gen_counter_opt = bennet_hash_table_get(const_str, pointer)(
      &function_to_generators_tmp, current_function);

  bennet_info_backtrack_generators_counter* gen_counter;
  if (bennet_optional_is_none(gen_counter_opt)) {
    // Create new counter for this function
    gen_counter = malloc(sizeof(bennet_info_backtrack_generators_counter));
    bennet_hash_table_init(const_str, uint64_t)(gen_counter, string_hash, string_equal);
    bennet_hash_table_set(const_str, pointer)(
        &function_to_generators_tmp, current_function, gen_counter);
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
      bennet_hash_table_get(const_str, pointer)(&generator_to_locations_tmp, generator);

  bennet_info_backtrack_locations_counter* loc_counter;
  if (bennet_optional_is_none(loc_counter_opt)) {
    // Create new counter for this generator
    loc_counter = malloc(sizeof(bennet_info_backtrack_locations_counter));
    bennet_hash_table_init(location_key_t, uint64_t)(
        loc_counter, location_hash, location_equal);
    bennet_hash_table_set(const_str, pointer)(
        &generator_to_locations_tmp, generator, loc_counter);
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

// Helper struct for sorting generator counts
typedef struct {
  const char* key;
  uint64_t value;
} gen_count_entry_t;

static int compare_gen_count_desc(const void* a, const void* b) {
  const gen_count_entry_t* ea = (const gen_count_entry_t*)a;
  const gen_count_entry_t* eb = (const gen_count_entry_t*)b;
  if (ea->value < eb->value)
    return 1;
  if (ea->value > eb->value)
    return -1;
  return 0;
}

// Helper struct for sorting location counts
typedef struct {
  location_key_t key;
  uint64_t value;
} loc_count_entry_t;

static int compare_loc_count_desc(const void* a, const void* b) {
  const loc_count_entry_t* ea = (const loc_count_entry_t*)a;
  const loc_count_entry_t* eb = (const loc_count_entry_t*)b;
  if (ea->value < eb->value)
    return 1;
  if (ea->value > eb->value)
    return -1;
  return 0;
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
    if (!function_to_generators.entries[i].occupied) {
      continue;
    }

    const char* function_name = function_to_generators.entries[i].key;
    bennet_info_backtrack_generators_counter* gen_counter =
        (bennet_info_backtrack_generators_counter*)function_to_generators.entries[i]
            .value;

    // Calculate total generators and backtracks for this function
    size_t gen_count = 0;
    uint64_t total_backtracks = 0;
    for (size_t j = 0; j < gen_counter->capacity; ++j) {
      if (gen_counter->entries[j].occupied) {
        gen_count++;
        total_backtracks += gen_counter->entries[j].value;
      }
    }

    printf("%s: %" PRIu64 " backtracks\n", function_name, total_backtracks);
    if (total_backtracks == 0) {
      continue;
    }

    // Collect generator counts for sorting
    gen_count_entry_t* gen_entries = malloc(gen_count * sizeof(gen_count_entry_t));
    size_t idx = 0;
    for (size_t j = 0; j < gen_counter->capacity; ++j) {
      if (gen_counter->entries[j].occupied) {
        gen_entries[idx].key = gen_counter->entries[j].key;
        gen_entries[idx].value = gen_counter->entries[j].value;
        idx++;
      }
    }
    qsort(gen_entries, gen_count, sizeof(gen_count_entry_t), compare_gen_count_desc);

    // Print sorted generator counts
    for (size_t j = 0; j < gen_count; ++j) {
      double percent = (double)gen_entries[j].value / (double)total_backtracks * 100;
      if (percent < 1) {
        continue;
      }

      printf("  %s: %.0f%% (%" PRIu64 " backtracks)\n",
          gen_entries[j].key,
          percent,
          gen_entries[j].value);
    }
    free(gen_entries);
    printf("\n");
  }

  printf("===========\n");
  printf("GENERATORS\n");
  printf("===========\n\n");

  // Iterate through all generators and their location counters
  for (size_t i = 0; i < generator_to_locations.capacity; ++i) {
    if (!generator_to_locations.entries[i].occupied) {
      continue;
    }

    const char* generator_name = generator_to_locations.entries[i].key;
    bennet_info_backtrack_locations_counter* loc_counter =
        (bennet_info_backtrack_locations_counter*)generator_to_locations.entries[i].value;

    // Calculate total backtracks for this generator
    size_t loc_count = 0;
    uint64_t total_backtracks = 0;
    for (size_t j = 0; j < loc_counter->capacity; ++j) {
      if (loc_counter->entries[j].occupied) {
        loc_count++;
        total_backtracks += loc_counter->entries[j].value;
      }
    }

    printf("%s: %" PRIu64 " backtracks\n", generator_name, total_backtracks);
    if (total_backtracks == 0) {
      continue;
    }

    // Collect location counts for sorting
    loc_count_entry_t* loc_entries = malloc(loc_count * sizeof(loc_count_entry_t));
    size_t idx = 0;
    for (size_t j = 0; j < loc_counter->capacity; ++j) {
      if (loc_counter->entries[j].occupied) {
        loc_entries[idx].key = loc_counter->entries[j].key;
        loc_entries[idx].value = loc_counter->entries[j].value;
        idx++;
      }
    }

    qsort(loc_entries, loc_count, sizeof(loc_count_entry_t), compare_loc_count_desc);

    // Print sorted location counts
    for (size_t j = 0; j < loc_count; ++j) {
      double percent = (double)loc_entries[j].value / (double)total_backtracks * 100;
      if (percent < 1) {
        continue;
      }

      printf("  %s:%d: %.0f%% (%" PRIu64 " backtracks)\n",
          loc_entries[j].key.filename,
          loc_entries[j].key.line_number,
          percent,
          loc_entries[j].value);
    }
    free(loc_entries);
    printf("\n");
  }
}
