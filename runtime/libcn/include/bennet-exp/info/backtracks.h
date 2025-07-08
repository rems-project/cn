#ifndef BENNET_EXP_BACKTRACKS_H
#define BENNET_EXP_BACKTRACKS_H

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

#endif  // BENNET_EXP_BACKTRACKS_H
