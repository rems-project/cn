#ifndef BENNET_EXP_SIZES_H
#define BENNET_EXP_SIZES_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize sizes info */
void bennet_info_sizes_init(void);

/**
 * Sets the current function under test
 * @param function_name The name of the function currently being tested
 */
void bennet_info_sizes_set_function_under_test(const char* function_name);

/**
 * Log an input size for the current function under test.
 */
void bennet_info_sizes_log(void);

/**
 * Prints size statistics for all functions under test
 */
void bennet_info_sizes_print_info(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_EXP_SIZES_H
