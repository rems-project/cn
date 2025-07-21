#ifndef BENNET_UNSATISFIED_H
#define BENNET_UNSATISFIED_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize satisfaction info */
void bennet_info_unsatisfied_init(void);

/**
 * Sets the current function under test
 * @param function_name The name of the function currently being tested
 */
void bennet_info_unsatisfied_set_function_under_test(const char* function_name);

/**
 * Logs whether the constraint was unsatisfied for a specific location
 * @param filename The source file where the unsatisfied constraint was
 * @param line_number The line number where the unsatisfied constraint was
 * @param unsatisfied True if the constraint was unsatisfied in this evaluation, false otherwise
 */
void bennet_info_unsatisfied_log(const char* filename, int line_number, bool unsatisfied);

/**
 * Prints satisfaction statistics for all functions under test
 */
void bennet_info_unsatisfied_print_info(void);

/**
 * @brief Begin a new run by initializing temporary hash tables for tracking satisfaction statistics.
 * This function should be called at the start of each run to prepare the temporary storage.
 */
void bennet_info_unsatisfied_begin_run(void);

/**
 * @brief End a run by merging temporary satisfaction statistics into permanent storage.
 * @param record If false, the temporary statistics are discarded. If true, they are merged into permanent storage.
 */
void bennet_info_unsatisfied_end_run(bool record);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_UNSATISFIED_H
