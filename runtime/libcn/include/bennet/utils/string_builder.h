#ifndef BENNET_STRING_BUILDER_H
#define BENNET_STRING_BUILDER_H

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/vector.h>

#ifdef __cplusplus
extern "C" {
#endif

// Typedef for const char* to work with bennet_vector macros
typedef const char *cstring;

// Declare vector type for cstring
BENNET_VECTOR_DECL(cstring)

// Implement vector functions for cstring
BENNET_VECTOR_IMPL(cstring)

// String builder structure
typedef struct bennet_string_builder {
  bennet_vector(cstring) strings;
} bennet_string_builder;

// Initialize a string builder
static inline void bennet_sb_init(bennet_string_builder *sb) {
  assert(sb != NULL);
  bennet_vector_init(cstring)(&sb->strings);
}

// Free a string builder (does not free the individual strings)
static inline void bennet_sb_free(bennet_string_builder *sb) {
  assert(sb != NULL);
  bennet_vector_free(cstring)(&sb->strings);
}

// Append a string to the string builder
static inline void bennet_sb_append(bennet_string_builder *sb, const char *str) {
  assert(sb != NULL);
  assert(str != NULL);
  bennet_vector_push(cstring)(&sb->strings, str);
}

// Build the final string by concatenating all strings
static inline char *bennet_sb_build(bennet_string_builder *sb) {
  assert(sb != NULL);

  if (bennet_vector_size(cstring)(&sb->strings) == 0) {
    char *result = (char *)malloc(1);
    assert(result != NULL);
    result[0] = '\0';
    return result;
  }

  // Calculate total length needed
  size_t total_length = 0;
  for (size_t i = 0; i < bennet_vector_size(cstring)(&sb->strings); i++) {
    const char *str = *bennet_vector_get(cstring)(&sb->strings, i);
    total_length += strlen(str);
  }

  // Allocate buffer (+1 for null terminator)
  char *result = (char *)malloc(total_length + 1);
  assert(result != NULL);
  result[0] = '\0';  // Start with empty string for strcat

  // Concatenate all strings
  for (size_t i = 0; i < bennet_vector_size(cstring)(&sb->strings); i++) {
    const char *str = *bennet_vector_get(cstring)(&sb->strings, i);
    strcat(result, str);
  }

  return result;
}

// Get the number of strings in the builder
static inline size_t bennet_sb_count(bennet_string_builder *sb) {
  assert(sb != NULL);
  return bennet_vector_size(cstring)(&sb->strings);
}

#ifdef __cplusplus
}
#endif

#endif  // BENNET_STRING_BUILDER_H
