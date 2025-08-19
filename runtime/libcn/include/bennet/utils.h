#ifndef BENNET_UTILS_H
#define BENNET_UTILS_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Helper to conditionally include uintptr_t cases based on type compatibility
// We need to detect if uintptr_t is the same underlying type as any of the types we already handle

// Check if uintptr_t is the same as unsigned long long (typical on some Linux systems)
#if defined(__SIZEOF_POINTER__) && defined(__SIZEOF_LONG_LONG__) &&                      \
    __SIZEOF_POINTER__ == __SIZEOF_LONG_LONG__ && defined(__LP64__) &&                   \
    !defined(__APPLE__)
  // On 64-bit non-Apple systems where pointers are 8 bytes and long long is 8 bytes
  // uintptr_t might be unsigned long long (same as uint64_t)
  #define UINTPTR_MAX_CASE
  #define UINTPTR_MIN_CASE
  #define UINTPTR_C_CASE
#else
  // On most systems including macOS, uintptr_t is distinct
  #define UINTPTR_MAX_CASE                                                               \
  uintptr_t:                                                                             \
    UINTPTR_MAX,
  #define UINTPTR_MIN_CASE                                                               \
  uintptr_t:                                                                             \
    0,
  #define UINTPTR_C_CASE                                                                 \
  uintptr_t:                                                                             \
    UINTMAX_C(const),
#endif

// Helper macros for bitvector types
#define BV_MAX(cty)                                                                      \
  _Generic((cty)0,                                                                       \
      uint8_t: UINT8_MAX,                                                                \
      uint16_t: UINT16_MAX,                                                              \
      uint32_t: UINT32_MAX,                                                              \
      uint64_t: UINT64_MAX,                                                              \
      UINTPTR_MAX_CASE int8_t: INT8_MAX,                                                 \
      int16_t: INT16_MAX,                                                                \
      int32_t: INT32_MAX,                                                                \
      int64_t: INT64_MAX)

#define BV_MIN(cty)                                                                      \
  _Generic((cty)0,                                                                       \
      uint8_t: 0,                                                                        \
      uint16_t: 0,                                                                       \
      uint32_t: 0,                                                                       \
      uint64_t: 0,                                                                       \
      UINTPTR_MIN_CASE int8_t: INT8_MIN,                                                 \
      int16_t: INT16_MIN,                                                                \
      int32_t: INT32_MIN,                                                                \
      int64_t: INT64_MIN)

#define BV_C(cty, const)                                                                 \
  _Generic((cty)0,                                                                       \
      uintptr_t: UINTPTR_C(const),                                                       \
      uint8_t: UINT8_C(const),                                                           \
      uint16_t: UINT16_C(const),                                                         \
      uint32_t: UINT32_C(const),                                                         \
      uint64_t: UINT64_C(const),                                                         \
      UINTPTR_C_CASE int8_t: INT8_C(const),                                              \
      int16_t: INT16_C(const),                                                           \
      int32_t: INT32_C(const),                                                           \
      int64_t: INT64_C(const))

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

#endif  // BENNET_UTILS_H
