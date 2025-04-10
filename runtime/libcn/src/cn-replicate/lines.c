#include <ctype.h>
#include <inttypes.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static size_t size = 0;
static size_t capacity = 0;
static char** lines = NULL;

void cn_replica_lines_append(char* line) {
  if (size == capacity) {
    capacity = (capacity == 0) ? 8 : 2 * capacity;
    lines = realloc(lines, capacity * sizeof(char*));
    if (lines == NULL) {
      fprintf(stderr, "Failed to expand reproduction array\n");
      abort();
    }
  }

  lines[size++] = line;
}

void cn_replica_lines_reset() {
  for (int i = 0; i < size; i++) {
    // free(lines[i]); // TODO: free lines
  }
  free(lines);
  lines = NULL;

  size = 0;
  capacity = 0;
}

char* cn_replica_lines_to_str() {
  size_t sz = 0;
  for (int i = 0; i < size; i++) {
    sz += strlen(lines[i]) + 1;  // +1 for newline
  }

  char* res = malloc(sz + 1);
  for (int i = 0; i < size; i++) {
    strcat(res, lines[i]);
    strcat(res, "\n");
  }

  return res;
}

char *cn_replica_lines_to_json_literal() {
  size_t sz = 0;
  for (int i = 0; i < size; i++) {
    sz += strlen(lines[i]) + 2;  // +2 for newline
    for(int j = 0; j < strlen(lines[i]); j++) {
      char k = lines[i][j];
      if(k == '\"' || k == '\\' || k == '\b' || k == '\f' || k == '\r' || k == '\t') {
        sz++;
      } else if(!isprint(k)) {
        sz += 5;
      }
    }
  }

  char* res = malloc(sz + 1);
  for (int i = 0; i < size; i++) {
    for(int j = 0; j < strlen(lines[i]); j++) {
      char k = lines[i][j];
      if(k == '\"') {
        strlcat(res, "\\\"", sz+1);
      } else if(k == '\\') {
        strlcat(res, "\\\\", sz+1);
      } else if(k == '\b') {
        strlcat(res, "\\b", sz+1);
      } else if(k == '\f') {
        strlcat(res, "\\f", sz+1);
      } else if(k == '\r') {
        strlcat(res, "\\r", sz+1);
      } else if(k == '\t') {
        strlcat(res, "\\t", sz+1);
      } else if(!isprint(k)) {
        snprintf(&res[strlen(res)], 7, "\\u%4x", (int) k);
      } else {
        snprintf(&res[strlen(res)], 2, "%c", k);
      }
    }
    strlcat(res, "\\n", sz+1);
  }

  return res;
}

// 'djb' string hashing function
// Source: http://www.cse.yorku.ca/~oz/hash.html
unsigned long hash(unsigned char *str) {
  unsigned long hash = 5381;
  int c;

  while (c = *str++)
      hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

  return hash;
}

void print_test_summary_tyche(FILE *out, 
                              char *test_suite,
                              char *test_name,
                              char *status,
                              uint64_t suite_begin_time,
                              char *representation,
                              int64_t init_time,
                              int64_t runtime) {
  fprintf(out, "\n{ \"type\": \"test_case\", \"property\": \"%s-%s\", \"arguments\": { \"n\": \"%lx\" }, \"run_start\": %.6lf, \"status\": \"%s\", \"status_reason\": \"\", \"representation\": \"%s\", \"features\": {}, \"timing\": { \"execute:test\": %.6lf, \"overall:gc\": 0.0, \"generate:n\": %.6lf }, \"coverage\": {} }\n",
       test_suite, test_name, hash(representation), suite_begin_time / 1000000.0, status, representation, runtime / 1000000.0, init_time / 1000000.0);  
}