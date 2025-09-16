#ifndef BENNET_DISCARDS_H
#define BENNET_DISCARDS_H

#include <stdbool.h>
#include <stdint.h>

#include <bennet/state/failure.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize discards info */
void bennet_info_discards_init(void);

/**
 * Sets the current function under test
 * @param function_name The name of the function currently being tested
 */
void bennet_info_discards_set_function_under_test(const char* function_name);

/**
 * Log a discard with the specified failure type for the current function under test.
 * @param failure_type The type of failure that caused the discard
 */
void bennet_info_discards_log(enum bennet_failure_type failure_type);

/**
 * Prints discard statistics for all functions under test
 */
void bennet_info_discards_print_info(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_DISCARDS_H
