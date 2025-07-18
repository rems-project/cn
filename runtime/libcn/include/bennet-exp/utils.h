#ifndef BENNET_EXP_UTILS_H
#define BENNET_EXP_UTILS_H

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

#endif  // BENNET_EXP_UTILS_H
