#ifndef BENNET_EXP_UTILS_H
#define BENNET_EXP_UTILS_H

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// Helper function to get the base name of a filename
static inline const char* get_basename(const char* filename) {
  if (!filename) {
    return NULL;
  }

  // Find the last occurrence of '/' or '\'
  const char* last_slash = strrchr(filename, '/');
  const char* last_backslash = strrchr(filename, '\\');

  // Use the later of the two (or whichever exists)
  const char* last_separator = NULL;
  if (last_slash && last_backslash) {
    last_separator = (last_slash > last_backslash) ? last_slash : last_backslash;
  } else if (last_slash) {
    last_separator = last_slash;
  } else if (last_backslash) {
    last_separator = last_backslash;
  }

  // Return the substring after the last separator, or the original string if no separator
  return last_separator ? last_separator + 1 : filename;
}

// String hash and equality functions

/**
 * @brief 'djb' string hashing function
 * 
 * Source: http://www.cse.yorku.ca/~oz/hash.html
 */
static inline size_t string_hash(const char* str) {
  size_t hash = 5381;
  int c;
  while ((c = *str++)) {
    hash = ((hash << 5) + hash) + c;  // hash * 33 + c
  }
  return hash;
}

static inline bool string_equal(const char* a, const char* b) {
  return strcmp(a, b) == 0;
}

#endif  // BENNET_EXP_UTILS_H
