#ifndef BENNET_INFO_TIMING_H
#define BENNET_INFO_TIMING_H

#include <stdint.h>

#include <bennet/utils/hash_table.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const char* const_str;

BENNET_HASH_TABLE_DECL(const_str, int64_t);

// Hashtable type: event_name (const char*) → duration (int64_t microseconds)
typedef bennet_hash_table(const_str, int64_t) timing_events_t;

/**
 * @brief Initialize the timing info subsystem
 *
 * Must be called once at program startup before any other timing functions.
 */
void bennet_info_timing_init(void);

/**
 * @brief Set the function currently under test
 *
 * @param function_name The name of the function being tested
 */
void bennet_info_timing_set_function_under_test(const char* function_name);

/**
 * @brief Start timing an event
 *
 * Records the current timestamp for the named event. If the event is already
 * started, the previous start time is overwritten.
 *
 * @param event_name The name of the event (e.g., "execute:test", "generate:total")
 */
void bennet_info_timing_start(const char* event_name);

/**
 * @brief End timing an event
 *
 * Calculates the duration since the corresponding start call and updates
 * statistics (min, max, avg, count, last) for the event.
 *
 * @param event_name The name of the event to end timing for
 */
void bennet_info_timing_end(const char* event_name);

/**
 * @brief Get the last timing measurements for the current function
 *
 * Returns a hashtable mapping event names to their most recent durations
 * (in microseconds) for the current function under test.
 *
 * @return Pointer to hashtable of event_name → last_duration, or NULL if no data
 */
timing_events_t* bennet_info_timing_get_last(void);

/**
 * @brief Print timing statistics for all functions
 *
 * Outputs timing information to stdout, showing min/max/avg/count for each
 * event across all functions.
 */
void bennet_info_timing_print_info(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_INFO_TIMING_H
