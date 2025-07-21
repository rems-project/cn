#ifndef BENNET_BACKTRACKS_H
#define BENNET_BACKTRACKS_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Initialize backtracking info */
void bennet_info_backtracks_init(void);

/**
 * Sets the current function under test
 * @param function_name The name of the function currently being tested
 */
void bennet_info_backtracks_set_function_under_test(const char* function_name);

/**
 * @brief Begin a new generator run by initializing temporary hash tables for tracking backtracking statistics.
 * 
 * This function should be called at the start of each generator run to prepare the temporary
 * storage for collecting backtracking information.
 * 
 * @note This function should be paired with `bennet_info_backtracks_end_run()` to properly
 *       merge and clean up the temporary data.
 */
void bennet_info_backtracks_begin_run(void);

/**
 * @brief End a generator run by merging temporary backtracking statistics into permanent storage.
 * 
 * This function should be called at the end of each generator run.
 * It either merges all data from the temporary hash tables into the permanent tables
 * (when record is `true`), or simply frees the temporary tables (when record is `false`).
 * 
 * After processing, the temporary tables are freed to prepare for the next generator run.
 * 
 * @param record If `false`, the temporary statistics are discarded without being added to the
 *                permanent storage. If `true`, the temporary statistics are merged into the
 *                permanent storage, accumulating counts.
 * @note This function should be called for all generator runs, with `record=true` for
 *       generator runs that should contribute to the final statistics, and
 *       `record=false` for generator runs that should not.
 * @note This function must be called after `bennet_info_backtracks_begin_run()` to ensure
 *       proper initialization of temporary data.
 */
void bennet_info_backtracks_end_run(bool record);

uint64_t bennet_info_backtracks_last_total(void);

/**
 * Logs a backtrack with generator name, filename, and line number
 * @param generator The name of the generator where the backtrack occurred
 * @param filename The source file where the backtrack occurred
 * @param line_number The line number where the backtrack occurred
 */
void bennet_info_backtracks_log(
    const char* generator, const char* filename, int line_number);

/**
 * Prints backtracking statistics for all functions under test
 * Shows backtrack counts by generator, for each function under test
 * Then, show counts by filename + line number, for each generator
 */
void bennet_info_backtracks_print_backtrack_info(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_BACKTRACKS_H
