#include <inttypes.h>
#include <string.h>

#include <bennet/info/backtracks.h>
#include <bennet/info/sizes.h>
#include <bennet/info/timing.h>
#include <bennet/info/tyche.h>
#include <bennet/utils.h>

void print_test_summary_tyche(FILE *out, struct tyche_line_info *line_info) {
  // Get timing data from the timing subsystem
  timing_events_t *timings = bennet_info_timing_get_last();

  // Build timing JSON object by iterating over the hashtable
  const size_t timing_json_size = 2048;
  char *timing_json = malloc(timing_json_size);
  assert(timing_json);

  timing_json[0] = '\0';
  char *timing_ptr = timing_json;
  size_t timing_remaining = timing_json_size;
  bool first = true;

  if (timings) {
    for (size_t i = 0; i < timings->capacity; ++i) {
      if (timings->entries[i].occupied) {
        const char *event_name = timings->entries[i].key;
        int64_t duration_us = timings->entries[i].value;
        double duration_s = duration_us / 1000000.0;

        int written = snprintf(timing_ptr,
            timing_remaining,
            "%s\"%s\": %.6lf",
            first ? "" : ", ",
            event_name,
            duration_s);

        if (written > 0 && (size_t)written < timing_remaining) {
          timing_ptr += written;
          timing_remaining -= written;
          first = false;
        } else if (written >= (int)timing_remaining) {
          // Buffer full, stop adding more entries
          break;
        }
      }
    }
  }

  if (strcmp(line_info->status, "gave_up") == 0) {
    fprintf(out,
        "\n{ \"type\": \"test_case\", \"property\": \"%s::%s\", \"arguments\": { "
        "\"prog\": \"%lx\" }, \"run_start\": %.6lf, \"status\": \"%s\", "
        "\"status_reason\": \"%s\", \"representation\": \"%s\", \"features\": {}, "
        "\"timing\": { %s }, \"coverage\": {} }\n",
        line_info->test_suite,
        line_info->test_name,
        string_hash(line_info->representation),
        line_info->suite_begin_time / 1000000.0,
        line_info->status,
        line_info->status_reason,
        line_info->representation,
        timing_json);

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
      "\"timing\": { %s }, \"coverage\": {} }\n",
      line_info->test_suite,
      line_info->test_name,
      string_hash(line_info->representation),
      line_info->suite_begin_time / 1000000.0,
      line_info->status,
      line_info->status_reason,
      line_info->representation,
      bennet_info_sizes_last_size(),
      bennet_info_backtracks_last_total(),
      timing_json);
}
