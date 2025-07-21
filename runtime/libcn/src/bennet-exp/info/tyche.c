#include <inttypes.h>
#include <string.h>

#include <bennet-exp/info/backtracks.h>
#include <bennet-exp/info/sizes.h>
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
  if (strcmp(line_info->status, "gave_up") == 0) {
    fprintf(out,
        "\n{ \"type\": \"test_case\", \"property\": \"%s::%s\", \"arguments\": { \"n\": "
        "\"%lx\" }, \"run_start\": %.6lf, \"status\": \"%s\", \"status_reason\": \"%s\", "
        "\"representation\": \"%s\", \"features\": {}, "
        "\"timing\": { \"execute:test\": "
        "%.6lf, \"overall:gc\": 0.0, \"generate:n\": %.6lf }, \"coverage\": {} }\n",
        line_info->test_suite,
        line_info->test_name,
        hash((unsigned char *)line_info->representation),
        line_info->suite_begin_time / 1000000.0,
        line_info->status,
        line_info->status_reason,
        line_info->representation,
        line_info->runtime / 1000000.0,
        line_info->init_time / 1000000.0);

    return;
  }

  fprintf(out,
      "\n{ \"type\": \"test_case\", \"property\": \"%s::%s\", \"arguments\": { \"n\": "
      "\"%lx\" }, \"run_start\": %.6lf, \"status\": \"%s\", \"status_reason\": \"%s\", "
      "\"representation\": \"%s\", "
      "\"features\": {"
      "\"Memory Allocated (Bytes)\": %zu, "
      "\"Backtracks\": %" PRIu64
      ", "
      "}, "
      "\"timing\": { \"execute:test\": "
      "%.6lf, \"overall:gc\": 0.0, \"generate:n\": %.6lf }, \"coverage\": {} }\n",
      line_info->test_suite,
      line_info->test_name,
      hash((unsigned char *)line_info->representation),
      line_info->suite_begin_time / 1000000.0,
      line_info->status,
      line_info->status_reason,
      line_info->representation,
      bennet_info_sizes_last_size(),
      bennet_info_backtracks_last_total(),
      line_info->runtime / 1000000.0,
      line_info->init_time / 1000000.0);
}
