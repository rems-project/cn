#include <stdint.h>
#include <stdio.h>

struct tyche_line_info {
  char *test_suite;
  char *test_name;
  char *status;
  char *status_reason;
  uint64_t suite_begin_time;
  char *representation;
};

void print_test_summary_tyche(FILE *out, struct tyche_line_info *line_info);
