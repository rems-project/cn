#include <bennet-exp/info/tyche.h>
#include <bennet-exp/utils.h>

void print_test_summary_tyche(FILE *out, struct tyche_line_info *line_info) {
  fprintf(out,
      "\n{ \"type\": \"test_case\", \"property\": \"%s::%s\", \"arguments\": { \"n\": "
      "\"%lx\" }, \"run_start\": %.6lf, \"status\": \"%s\", \"status_reason\": \"%s\", "
      "\"representation\": \"%s\", \"features\": {}, \"timing\": { \"execute:test\": "
      "%.6lf, \"overall:gc\": 0.0, \"generate:n\": %.6lf }, \"coverage\": {} }\n",
      line_info->test_suite,
      line_info->test_name,
      string_hash(line_info->representation),
      line_info->suite_begin_time / 1000000.0,
      line_info->status,
      line_info->status_reason,
      line_info->representation,
      line_info->runtime / 1000000.0,
      line_info->init_time / 1000000.0);
}
