#ifndef CN_LINES_H
#define CN_LINES_H

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include <stdio.h>

void cn_replica_lines_append(char *line);
void cn_replica_lines_reset(void);
char *cn_replica_lines_to_str(void);
char *cn_replica_lines_to_json_literal(void);

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

#ifdef __cplusplus
}
#endif

#endif /* CN_LINES_H */
