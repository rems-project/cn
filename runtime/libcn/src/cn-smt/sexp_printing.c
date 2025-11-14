#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/vector.h>
#include <cn-smt/memory/test_alloc.h>
#include <cn-smt/sexp.h>

typedef const char *const_char_ptr;
BENNET_VECTOR_DECL(const_char_ptr)
BENNET_VECTOR_IMPL(const_char_ptr)

static void sexp_to_string_aux(
    bennet_vector(const_char_ptr) * vec, size_t *total_length, sexp_t *sexp) {
  if (!sexp) {
    return;
  }

  if (sexp_is_atom(sexp)) {
    const char *atom = sexp->data.atom;
    bennet_vector_push(const_char_ptr)(vec, atom);
    *total_length += strlen(atom);
    return;
  }

  // For lists, append opening paren, elements, and closing paren
  size_t count;
  sexp_t **elements = sexp_to_list(sexp, &count);

  bennet_vector_push(const_char_ptr)(vec, "(");
  *total_length += 1;

  for (size_t i = 0; i < count; i++) {
    if (i > 0) {
      bennet_vector_push(const_char_ptr)(vec, " ");
      *total_length += 1;
    }
    sexp_to_string_aux(vec, total_length, elements[i]);
  }

  bennet_vector_push(const_char_ptr)(vec, ")");
  *total_length += 1;
}

char *sexp_to_string(sexp_t *sexp) {
  // Initialize vector to collect string fragments
  bennet_vector(const_char_ptr) vec;
  bennet_vector_init(const_char_ptr)(&vec);

  // Recursively build up vector of string fragments and track total length
  size_t total_length = 0;
  sexp_to_string_aux(&vec, &total_length, sexp);

  // Allocate final buffer and concatenate all fragments
  char *result = cn_test_malloc(total_length + 1);
  assert(result && "Failed to allocate memory for sexp string");

  char *ptr = result;
  for (size_t i = 0; i < bennet_vector_size(const_char_ptr)(&vec); i++) {
    const char *str = *bennet_vector_get(const_char_ptr)(&vec, i);
    size_t len = strlen(str);
    memcpy(ptr, str, len);
    ptr += len;
  }
  *ptr = '\0';

  // Clean up vector
  bennet_vector_free(const_char_ptr)(&vec);

  return result;
}
