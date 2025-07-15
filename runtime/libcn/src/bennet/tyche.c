#include <bennet-exp/info/tyche.h>

// 'djb' string hashing function
// Source: http://www.cse.yorku.ca/~oz/hash.html
static unsigned long hash(unsigned char *str) {
  unsigned long hash = 5381;
  int c;

  while ((c = *str++))
    hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}

void print_test_summary_tyche(FILE *out, struct tyche_line_info *line_info) {
  fprintf(out,
      "\n{ \"type\": \"test_case\", \"property\": \"%s-%s\", \"arguments\": { \"n\": "
      "\"%lx\" }, \"run_start\": %.6lf, \"status\": \"%s\", \"status_reason\": \"\", "
      "\"representation\": \"%s\", \"features\": {}, \"timing\": { \"execute:test\": "
      "%.6lf, \"overall:gc\": 0.0, \"generate:n\": %.6lf }, \"coverage\": {} }\n",
      line_info->test_suite,
      line_info->test_name,
      hash((unsigned char *)line_info->representation),
      line_info->suite_begin_time / 1000000.0,
      line_info->status,
      line_info->representation,
      line_info->runtime / 1000000.0,
      line_info->init_time / 1000000.0);
}
