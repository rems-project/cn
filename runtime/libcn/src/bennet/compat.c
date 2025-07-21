#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

void bennet_info_backtracks_init(void) {}
void bennet_info_backtracks_print_backtrack_info(void) {}
void bennet_info_backtracks_set_function_under_test(const char* fut) {}
void bennet_info_backtracks_begin_run(void) {}
void bennet_info_backtracks_end_run(bool record) {}

void bennet_info_sizes_init(void) {}
void bennet_info_sizes_print_info(void) {}
void bennet_info_sizes_set_function_under_test(const char* fut) {}
void bennet_info_sizes_log(void) {}

void bennet_info_unsatisfied_init(void) {}
void bennet_info_unsatisfied_set_function_under_test(const char* function_name) {}
void bennet_info_unsatisfied_print_info(void) {}
void bennet_info_unsatisfied_begin_run(void) {}
void bennet_info_unsatisfied_end_run(bool record) {}

int is_bennet_experimental(void) {
  return 0;
}
