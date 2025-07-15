#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

void bennet_info_backtracks_init(void) {
  printf("UNSUPPORTED BACKTRACK INFO ON BASE RUNTIME");
  assert(false);
}

void bennet_info_backtracks_print_backtrack_info(void) {
  printf("UNSUPPORTED BACKTRACK INFO ON BASE RUNTIME");
  assert(false);
}

void bennet_info_backtracks_set_function_under_test(const char* fut) {
  printf("UNSUPPORTED BACKTRACK INFO ON BASE RUNTIME");
  assert(false);
}

void bennet_info_backtracks_begin_run(void) {}
void bennet_info_backtracks_end_run(bool record) {}

void bennet_info_sizes_init(void) {
  printf("UNSUPPORTED BACKTRACK INFO ON BASE RUNTIME");
  assert(false);
}

void bennet_info_sizes_print_info(void) {
  printf("UNSUPPORTED size INFO ON BASE RUNTIME");
  assert(false);
}

void bennet_info_sizes_set_function_under_test(const char* fut) {
  printf("UNSUPPORTED size INFO ON BASE RUNTIME");
  assert(false);
}

void bennet_info_sizes_log(void) {}

int is_bennet_experimental(void) {
  return 0;
}
