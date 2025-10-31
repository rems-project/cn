#include <sys/time.h>

#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/info/timing.h>
#include <bennet/internals/size.h>
#include <bennet/utils.h>
#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>

#define MAX_PRINTED 10

// Timing statistics for a single event
typedef struct {
  int64_t min;           // Minimum duration (microseconds)
  int64_t max;           // Maximum duration (microseconds)
  int64_t total;         // Total duration across all runs (microseconds)
  uint64_t count;        // Number of times this event was measured
  int64_t last;          // Last measured duration (microseconds)
  struct timeval start;  // Start time for current measurement
  bool is_running;       // Whether timing is currently active for this event
} timing_stats_t;

// Typedefs for hash table macros
typedef void* pointer;

// Hash table types
#define bennet_info_timing_stats_table bennet_hash_table(const_str, pointer)
#define bennet_info_function_to_timing bennet_hash_table(const_str, pointer)

BENNET_HASH_TABLE_DECL(const_str, pointer);
BENNET_OPTIONAL_DECL(pointer);
BENNET_HASH_TABLE_IMPL(const_str, pointer);
BENNET_HASH_TABLE_IMPL(const_str, int64_t);

// Helper struct for sorting timing entries
typedef struct {
  const char* event;
  int64_t avg_duration;
  uint64_t count;
} timing_entry_t;

static int compare_timing_desc(const void* a, const void* b) {
  const timing_entry_t* ea = (const timing_entry_t*)a;
  const timing_entry_t* eb = (const timing_entry_t*)b;
  if (ea->avg_duration < eb->avg_duration) {
    return 1;
  }
  if (ea->avg_duration > eb->avg_duration) {
    return -1;
  }
  return 0;
}

// Global state
static bool initialized = false;
static const char* current_function = NULL;
static bennet_info_function_to_timing function_to_timing;
static timing_events_t* last_timing_events = NULL;

void bennet_info_timing_init(void) {
  if (!initialized) {
    bennet_hash_table_init(const_str, pointer)(
        &function_to_timing, string_hash, string_equal);
    initialized = true;
  }
}

void bennet_info_timing_set_function_under_test(const char* function_name) {
  if (!initialized) {
    return;
  }

  assert(function_name);
  current_function = function_name;

  // Create an empty timing stats table for this function if it doesn't exist
  bennet_optional(pointer) stats_table_opt =
      bennet_hash_table_get(const_str, pointer)(&function_to_timing, current_function);

  if (bennet_optional_is_none(stats_table_opt)) {
    // Create new empty stats table for this function
    bennet_info_timing_stats_table* stats_table =
        malloc(sizeof(bennet_info_timing_stats_table));
    bennet_hash_table_init(const_str, pointer)(stats_table, string_hash, string_equal);
    bennet_hash_table_set(const_str, pointer)(
        &function_to_timing, current_function, stats_table);
  }
}

void bennet_info_timing_start(const char* event_name) {
  if (!initialized || !current_function) {
    return;
  }

  assert(event_name);

  // Get the stats table for current function
  bennet_optional(pointer) stats_table_opt =
      bennet_hash_table_get(const_str, pointer)(&function_to_timing, current_function);

  if (bennet_optional_is_none(stats_table_opt)) {
    return;
  }

  bennet_info_timing_stats_table* stats_table =
      (bennet_info_timing_stats_table*)bennet_optional_unwrap(stats_table_opt);

  // Get or create timing stats for this event
  bennet_optional(pointer) stats_opt =
      bennet_hash_table_get(const_str, pointer)(stats_table, event_name);

  timing_stats_t* stats;
  if (bennet_optional_is_none(stats_opt)) {
    // Create new stats for this event
    stats = malloc(sizeof(timing_stats_t));
    stats->min = INT64_MAX;
    stats->max = 0;
    stats->total = 0;
    stats->count = 0;
    stats->last = 0;
    stats->is_running = false;
    bennet_hash_table_set(const_str, pointer)(stats_table, strdup(event_name), stats);
  } else {
    stats = (timing_stats_t*)bennet_optional_unwrap(stats_opt);
  }

  // Record start time
  gettimeofday(&stats->start, NULL);
  stats->is_running = true;
}

void bennet_info_timing_end(const char* event_name) {
  struct timeval end_time;
  gettimeofday(&end_time, NULL);

  if (!initialized || !current_function) {
    return;
  }

  assert(event_name);

  // Get the stats table for current function
  bennet_optional(pointer) stats_table_opt =
      bennet_hash_table_get(const_str, pointer)(&function_to_timing, current_function);

  if (bennet_optional_is_none(stats_table_opt)) {
    return;
  }

  bennet_info_timing_stats_table* stats_table =
      (bennet_info_timing_stats_table*)bennet_optional_unwrap(stats_table_opt);

  // Get timing stats for this event
  bennet_optional(pointer) stats_opt =
      bennet_hash_table_get(const_str, pointer)(stats_table, event_name);

  if (bennet_optional_is_none(stats_opt)) {
    return;
  }

  timing_stats_t* stats = (timing_stats_t*)bennet_optional_unwrap(stats_opt);

  if (!stats->is_running) {
    return;
  }

  // Calculate duration
  int64_t duration = timediff_timeval(&stats->start, &end_time);

  // Update statistics
  if (duration < stats->min) {
    stats->min = duration;
  }
  if (duration > stats->max) {
    stats->max = duration;
  }
  stats->total += duration;
  stats->count++;
  stats->last = duration;
  stats->is_running = false;
}

timing_events_t* bennet_info_timing_get_last(void) {
  if (!initialized || !current_function) {
    return NULL;
  }

  // Get the stats table for current function
  bennet_optional(pointer) stats_table_opt =
      bennet_hash_table_get(const_str, pointer)(&function_to_timing, current_function);

  if (bennet_optional_is_none(stats_table_opt)) {
    return NULL;
  }

  bennet_info_timing_stats_table* stats_table =
      (bennet_info_timing_stats_table*)bennet_optional_unwrap(stats_table_opt);

  // Free previous last_timing_events if it exists
  if (last_timing_events) {
    bennet_hash_table_free(const_str, int64_t)(last_timing_events);
    free(last_timing_events);
  }

  // Create new hashtable for last timing values
  last_timing_events = malloc(sizeof(timing_events_t));
  bennet_hash_table_init(const_str, int64_t)(
      last_timing_events, string_hash, string_equal);

  // Populate the hashtable with event_name → last_duration
  for (size_t i = 0; i < stats_table->capacity; ++i) {
    if (stats_table->entries[i].occupied) {
      const char* event_name = stats_table->entries[i].key;
      timing_stats_t* stats = (timing_stats_t*)stats_table->entries[i].value;
      bennet_hash_table_set(const_str, int64_t)(
          last_timing_events, event_name, stats->last);
    }
  }

  return last_timing_events;
}

void bennet_info_timing_print_info(void) {
  if (!initialized) {
    return;
  }

  printf("=== TIMING STATISTICS ===\n\n");
  printf("====================\n");
  printf("FUNCTIONS UNDER TEST\n");
  printf("====================\n\n");

  // Iterate through all functions and their timing stats
  for (size_t i = 0; i < function_to_timing.capacity; ++i) {
    if (!function_to_timing.entries[i].occupied) {
      continue;
    }

    const char* function_name = function_to_timing.entries[i].key;
    bennet_info_timing_stats_table* stats_table =
        (bennet_info_timing_stats_table*)function_to_timing.entries[i].value;

    printf("%s:\n", function_name);

    // Count events for this function
    size_t event_count = 0;
    for (size_t j = 0; j < stats_table->capacity; ++j) {
      if (stats_table->entries[j].occupied) {
        event_count++;
      }
    }

    if (event_count == 0) {
      printf("  No timing data\n\n");
      continue;
    }

    // Collect timing entries for sorting
    timing_entry_t* entries = malloc(event_count * sizeof(timing_entry_t));
    size_t idx = 0;
    for (size_t j = 0; j < stats_table->capacity; ++j) {
      if (stats_table->entries[j].occupied) {
        const char* event_name = stats_table->entries[j].key;
        timing_stats_t* stats = (timing_stats_t*)stats_table->entries[j].value;

        entries[idx].event = event_name;
        entries[idx].avg_duration =
            (stats->count > 0) ? (stats->total / stats->count) : 0;
        entries[idx].count = stats->count;
        idx++;
      }
    }

    // Sort descending by average duration
    qsort(entries, event_count, sizeof(timing_entry_t), compare_timing_desc);

    size_t print_count = (event_count > MAX_PRINTED) ? MAX_PRINTED : event_count;

    // Print sorted timing stats
    for (size_t j = 0; j < print_count; ++j) {
      const char* event_name = entries[j].event;

      // Get the actual stats for detailed info
      bennet_optional(pointer) stats_opt =
          bennet_hash_table_get(const_str, pointer)(stats_table, event_name);
      timing_stats_t* stats = (timing_stats_t*)bennet_optional_unwrap(stats_opt);

      int64_t avg_us = (stats->count > 0) ? (stats->total / stats->count) : 0;

      printf("  %s:\n", event_name);
      printf("    Count: %" PRIu64 "\n", stats->count);
      printf("    Min: %.6lf s\n", stats->min / 1000000.0);
      printf("    Max: %.6lf s\n", stats->max / 1000000.0);
      printf("    Avg: %.6lf s\n", avg_us / 1000000.0);
      printf("    Last: %.6lf s\n", stats->last / 1000000.0);
    }

    free(entries);
    printf("\n");
  }
}
