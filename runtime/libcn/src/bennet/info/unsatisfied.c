#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils.h>
#include <bennet/utils/hash_table.h>

#define MAX_PRINTED 10

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
typedef bool boolean;
typedef void* pointer;

// Struct to hold both unsatisfied and satisfied counters
typedef struct {
  uint32_t unsatisfied;
  uint32_t satisfied;
} satisfaction_counters_t;
// Hash table type: location key -> satisfaction_counters_t
#define bennet_info_unsatisfied_locations_count                                          \
  bennet_hash_table(location_key_t, satisfaction_counters_t)

BENNET_OPTIONAL_DECL(satisfaction_counters_t);
BENNET_HASH_TABLE_DECL(location_key_t, satisfaction_counters_t);
BENNET_HASH_TABLE_IMPL(location_key_t, satisfaction_counters_t);

// Struct to hold pointer to location table and a per-function run counter
typedef struct {
  bennet_info_unsatisfied_locations_count* loc_table;
  uint32_t run_count;
} function_unsatisfied_entry_t;

// Hash table type: function name -> function_unsatisfied_entry_t
#define bennet_info_unsatisfied_functions                                                \
  bennet_hash_table(const_str, function_unsatisfied_entry_t)

// Hash table type: location key -> bool
#define bennet_info_unsatisfied_locations bennet_hash_table(location_key_t, boolean)

BENNET_OPTIONAL_DECL(pointer);
BENNET_OPTIONAL_DECL(boolean);
BENNET_OPTIONAL_DECL(function_unsatisfied_entry_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(pointer);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(boolean);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(satisfaction_counters_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(function_unsatisfied_entry_t);
BENNET_HASH_TABLE_DECL(const_str, pointer);
BENNET_HASH_TABLE_DECL(location_key_t, boolean);
BENNET_HASH_TABLE_DECL(const_str, function_unsatisfied_entry_t);
BENNET_HASH_TABLE_IMPL(const_str, pointer);
BENNET_HASH_TABLE_IMPL(location_key_t, boolean);
BENNET_HASH_TABLE_IMPL(const_str, function_unsatisfied_entry_t);

// Global state
static bool initialized = false;
static const char* current_function = NULL;
static bennet_info_unsatisfied_functions function_to_unsatisfied;
static bennet_hash_table_const_str_pointer function_to_unsatisfied_tmp;

void bennet_info_unsatisfied_init(void) {
  if (!initialized) {
    bennet_hash_table_init(const_str, function_unsatisfied_entry_t)(
        &function_to_unsatisfied, string_hash, string_equal);
    initialized = true;
  }
}

void bennet_info_unsatisfied_set_function_under_test(const char* function_name) {
  if (!initialized) {
    return;
  }
  assert(function_name);
  current_function = function_name;
}

void bennet_info_unsatisfied_begin_run(void) {
  if (!initialized) {
    return;
  }
  bennet_hash_table_init(const_str, pointer)(
      &function_to_unsatisfied_tmp, string_hash, string_equal);
  // Insert an entry into function_to_unsatisfied if current_function doesn't exist
  if (current_function && initialized) {
    bennet_optional(function_unsatisfied_entry_t) entry_opt =
        bennet_hash_table_get(const_str, function_unsatisfied_entry_t)(
            &function_to_unsatisfied, current_function);
    if (bennet_optional_is_none(entry_opt)) {
      bennet_info_unsatisfied_locations_count* loc_table =
          malloc(sizeof(bennet_info_unsatisfied_locations_count));
      bennet_hash_table_init(location_key_t, satisfaction_counters_t)(
          loc_table, location_hash, location_equal);
      function_unsatisfied_entry_t entry = {.loc_table = loc_table, .run_count = 0};
      bennet_hash_table_set(const_str, function_unsatisfied_entry_t)(
          &function_to_unsatisfied, current_function, entry);
    }
  }
}

void bennet_info_unsatisfied_end_run(bool record) {
  if (!initialized) {
    return;
  }

  if (!record) {
    bennet_hash_table_free(const_str, pointer)(&function_to_unsatisfied_tmp);
    return;
  }

  // Merge function_to_unsatisfied_tmp into function_to_unsatisfied
  for (size_t i = 0; i < function_to_unsatisfied_tmp.capacity; ++i) {
    if (function_to_unsatisfied_tmp.entries[i].occupied) {
      const char* function_name = function_to_unsatisfied_tmp.entries[i].key;
      bennet_info_unsatisfied_locations* tmp_loc_table =
          (bennet_info_unsatisfied_locations*)function_to_unsatisfied_tmp.entries[i]
              .value;
      // Get or create entry for this function in the permanent table
      bennet_optional(function_unsatisfied_entry_t) entry_opt =
          bennet_hash_table_get(const_str, function_unsatisfied_entry_t)(
              &function_to_unsatisfied, function_name);
      function_unsatisfied_entry_t entry;
      if (bennet_optional_is_none(entry_opt)) {
        entry.loc_table = malloc(sizeof(bennet_info_unsatisfied_locations_count));
        bennet_hash_table_init(location_key_t, satisfaction_counters_t)(
            entry.loc_table, location_hash, location_equal);
        entry.run_count = 0;
        bennet_hash_table_set(const_str, function_unsatisfied_entry_t)(
            &function_to_unsatisfied, function_name, entry);
      } else {
        entry = entry_opt.body;
      }
      // Increment the per-function run counter
      entry.run_count++;
      // For each location in tmp_loc_table, increment the appropriate counter
      for (size_t j = 0; j < tmp_loc_table->capacity; ++j) {
        if (tmp_loc_table->entries[j].occupied) {
          location_key_t loc_key = tmp_loc_table->entries[j].key;
          boolean tmp_val = tmp_loc_table->entries[j].value;
          bennet_optional(satisfaction_counters_t) counters_opt = bennet_hash_table_get(
              location_key_t, satisfaction_counters_t)(entry.loc_table, loc_key);
          satisfaction_counters_t counters = bennet_optional_unwrap_or(
              satisfaction_counters_t)(&counters_opt, (satisfaction_counters_t){0, 0});
          if (tmp_val) {
            counters.unsatisfied++;
          } else {
            counters.satisfied++;
          }
          bennet_hash_table_set(location_key_t, satisfaction_counters_t)(
              entry.loc_table, loc_key, counters);
        }
      }
      // Update the entry in the hash table
      bennet_hash_table_set(const_str, function_unsatisfied_entry_t)(
          &function_to_unsatisfied, function_name, entry);
    }
  }
  bennet_hash_table_free(const_str, pointer)(&function_to_unsatisfied_tmp);
}

void bennet_info_unsatisfied_log(
    const char* filename, int line_number, bool unsatisfied) {
  if (!initialized || !current_function || !filename) {
    return;
  }

  filename = get_basename(filename);

  // Get or create location table for current function in the TMP table
  bennet_optional(pointer) loc_table_opt = bennet_hash_table_get(const_str, pointer)(
      &function_to_unsatisfied_tmp, current_function);
  bennet_info_unsatisfied_locations* loc_table;
  if (bennet_optional_is_none(loc_table_opt)) {
    loc_table = malloc(sizeof(bennet_info_unsatisfied_locations));
    bennet_hash_table_init(location_key_t, boolean)(
        loc_table, location_hash, location_equal);
    bennet_hash_table_set(const_str, pointer)(
        &function_to_unsatisfied_tmp, current_function, loc_table);
  } else {
    loc_table = (bennet_info_unsatisfied_locations*)bennet_optional_unwrap(loc_table_opt);
  }

  // Set or update the bool for this location
  location_key_t loc_key;
  loc_key.filename = strdup(filename);
  loc_key.line_number = line_number;
  bennet_optional(boolean) satisfied_opt =
      bennet_hash_table_get(location_key_t, boolean)(loc_table, loc_key);
  boolean current = bennet_optional_unwrap_or(boolean)(&satisfied_opt, true);
  boolean new_value = current && unsatisfied;
  bennet_hash_table_set(location_key_t, boolean)(loc_table, loc_key, new_value);
}

// Struct to hold all relevant info for sorting
typedef struct {
  location_key_t loc;
  double sat_percentage;
  double hit_percentage;
} loc_info_t;

// Custom comparator for sorting loc_info_t
static int cmp_loc_info(const void* a, const void* b) {
  const loc_info_t* la = (const loc_info_t*)a;
  const loc_info_t* lb = (const loc_info_t*)b;
  if (la->sat_percentage < lb->sat_percentage)
    return -1;
  if (la->sat_percentage > lb->sat_percentage)
    return 1;
  if (la->hit_percentage > lb->hit_percentage)
    return -1;
  if (la->hit_percentage < lb->hit_percentage)
    return 1;
  // If still equal, sort by filename then line number
  int cmp = strcmp(la->loc.filename, lb->loc.filename);
  if (cmp != 0)
    return cmp;
  return la->loc.line_number - lb->loc.line_number;
}

void bennet_info_unsatisfied_print_info(void) {
  if (!initialized) {
    return;
  }
  printf("=== SATISFACTION STATISTICS ===\n\n");
  printf("====================\n");
  printf("FUNCTIONS UNDER TEST\n");
  printf("====================\n\n");
  for (size_t i = 0; i < function_to_unsatisfied.capacity; ++i) {
    if (function_to_unsatisfied.entries[i].occupied) {
      const char* function_name = function_to_unsatisfied.entries[i].key;
      function_unsatisfied_entry_t entry = function_to_unsatisfied.entries[i].value;
      bennet_info_unsatisfied_locations_count* loc_table = entry.loc_table;
      printf("%s (%u runs):\n", function_name, entry.run_count);

      // Collect locations into an array for sorting
      size_t loc_count = 0;
      for (size_t j = 0; j < loc_table->capacity; ++j) {
        if (loc_table->entries[j].occupied) {
          loc_count++;
        }
      }

      loc_info_t* infos = malloc(loc_count * sizeof(loc_info_t));
      size_t idx = 0;
      for (size_t j = 0; j < loc_table->capacity; ++j) {
        if (loc_table->entries[j].occupied) {
          location_key_t loc = loc_table->entries[j].key;
          satisfaction_counters_t c = loc_table->entries[j].value;
          uint32_t total = c.unsatisfied + c.satisfied;

          assert(total > 0);
          double sat_percentage = (double)(c.satisfied * 100) / (double)total;

          assert(entry.run_count > 0);
          double hit_percentage = (double)(total * 100) / (double)entry.run_count;

          infos[idx].loc = loc;
          infos[idx].sat_percentage = sat_percentage;
          infos[idx].hit_percentage = hit_percentage;

          idx++;
        }
      }
      qsort(infos, loc_count, sizeof(loc_info_t), cmp_loc_info);

      if (loc_count > MAX_PRINTED) {
        loc_count = MAX_PRINTED;
      }

      // Print in sorted order
      for (size_t k = 0; k < loc_count; ++k) {
        if (infos[k].sat_percentage > .5) {
          continue;
        }

        if (infos[k].hit_percentage < .1) {
          continue;
        }

        printf("  %s:%d: %.1f%% satisfied, %.1f%% hit\n",
            infos[k].loc.filename,
            infos[k].loc.line_number,
            infos[k].sat_percentage,
            infos[k].hit_percentage);
      }
      free(infos);
      printf("\n");
    }
  }
}
