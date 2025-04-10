#ifndef CN_LINES_H
#define CN_LINES_H

#ifdef __cplusplus
extern "C" {
#endif

void cn_replica_lines_append(char* line);
void cn_replica_lines_reset();
char* cn_replica_lines_to_str();
char* cn_replica_lines_to_json_literal();
void print_test_summary_tyche(FILE *out, char *test_suite, char *test_name, char *status, uint64_t suite_begin_time, char *representation, int64_t init_time, int64_t runtime);

#ifdef __cplusplus
}
#endif

#endif /* CN_LINES_H */
