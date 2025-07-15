#include <stdint.h>
#include <stdio.h>

struct tyche_line_info {
  char *test_suite;
  char *test_name;
  char *status;
  uint64_t suite_begin_time;
  char *representation;
  int64_t init_time;
  int64_t runtime;
};

void print_test_summary_tyche(FILE *out, struct tyche_line_info *line_info);
