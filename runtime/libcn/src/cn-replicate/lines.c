#include "cn-replicate/lines.h"

#include <ctype.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet-exp/utils/vector.h>

// Define a typedef for char *
typedef char *str;
BENNET_VECTOR_DECL(str)
BENNET_VECTOR_IMPL(str)

static bennet_vector(str) lines_vec;

void cn_replica_lines_append(char *line) {
  bennet_vector_push(str)(&lines_vec, line);
}

void cn_replica_lines_reset() {
  for (size_t i = 0; i < bennet_vector_size(str)(&lines_vec); i++) {
    // free(lines_vec.data[i]); // TODO: free lines
  }
  bennet_vector_free(str)(&lines_vec);
  bennet_vector_init(str)(&lines_vec);
}

char *cn_replica_lines_to_str() {
  size_t num_lines = bennet_vector_size(str)(&lines_vec);

  size_t sz = 0;
  for (size_t i = 0; i < num_lines; i++) {
    const char *line = *bennet_vector_get(str)(&lines_vec, i);
    sz += strlen(line) + 1;  // +1 for newline
  }

  char *res = malloc(sz + 1);  // +1 for string terminator
  res[0] = '\0';
  for (size_t i = 0; i < num_lines; i++) {
    const char *line = *bennet_vector_get(str)(&lines_vec, i);
    strcat(res, line);
    strcat(res, "\n");
  }

  return res;
}

char *cn_replica_lines_to_json_literal() {
  size_t num_lines = bennet_vector_size(str)(&lines_vec);

  size_t sz = 0;
  for (size_t i = 0; i < num_lines; i++) {
    const char *line = *bennet_vector_get(str)(&lines_vec, i);
    sz += strlen(line) + 2;  // +2 for newline
    for (size_t j = 0; j < strlen(line); j++) {
      char k = line[j];
      if (k == '"' || k == '\\' || k == '\b' || k == '\f' || k == '\r' || k == '\t') {
        sz++;
      } else if (!isprint(k)) {
        sz += 5;
      }
    }
  }

  char *res = malloc(sz + 1);
  res[0] = '\0';
  for (size_t i = 0; i < num_lines; i++) {
    const char *line = *bennet_vector_get(str)(&lines_vec, i);

    for (size_t j = 0; j < strlen(line); j++) {
      char k = line[j];
      if (k == '"') {
        strncat(res, "\\\"", 3);
      } else if (k == '\\') {
        strncat(res, "\\\\", 3);
      } else if (k == '\b') {
        strncat(res, "\\b", 3);
      } else if (k == '\f') {
        strncat(res, "\\f", 3);
      } else if (k == '\r') {
        strncat(res, "\\r", 3);
      } else if (k == '\t') {
        strncat(res, "\\t", 3);
      } else if (!isprint(k)) {
        snprintf(&res[strlen(res)], 7, "\\u%4x", (int)k);
      } else {
        snprintf(&res[strlen(res)], 2, "%c", k);
      }
    }
    strncat(res, "\\n", 3);
  }

  return res;
}

// 'djb' string hashing function
// Source: http://www.cse.yorku.ca/~oz/hash.html
unsigned long hash(unsigned char *str) {
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
