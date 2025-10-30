#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/sexp.h>

// Helper functions for parsing
static const char *skip_whitespace(const char *input) {
  while (*input && isspace(*input)) {
    input++;
  }

  return input;
}

static const char *parse_atom(const char *input, char **atom) {
  const char *start = input;
  while (*input && !isspace(*input) && *input != '(' && *input != ')') {
    input++;
  }

  size_t len = input - start;
  *atom = malloc(len + 1);
  assert(*atom);

  strncpy(*atom, start, len);
  (*atom)[len] = '\0';
  return input;
}

static const char *parse_sexp_impl(const char *input, sexp_t **result);

static const char *parse_list(const char *input, sexp_t **result) {
  assert(*input == '(');
  input++;  // skip '('
  input = skip_whitespace(input);

  sexp_t **elements = NULL;
  size_t count = 0;
  size_t capacity = 0;

  while (*input && *input != ')') {
    if (count >= capacity) {
      capacity = capacity ? capacity * 2 : 4;
      sexp_t **new_elements = realloc(elements, sizeof(sexp_t *) * capacity);
      assert(new_elements);

      elements = new_elements;
    }

    input = parse_sexp_impl(input, &elements[count]);
    if (!input) {
      free(elements);
      return NULL;
    }
    count++;
    input = skip_whitespace(input);
  }

  if (*input != ')') {
    free(elements);
    return NULL;
  }
  input++;  // skip ')'

  *result = sexp_list(elements, count);
  free(elements);  // sexp_list copies the elements
  return input;
}

static const char *parse_sexp_impl(const char *input, sexp_t **result) {
  input = skip_whitespace(input);
  if (!*input) {
    return NULL;
  }

  if (*input == '(') {
    return parse_list(input, result);
  } else {
    char *atom;
    input = parse_atom(input, &atom);
    if (!input)
      return NULL;

    *result = sexp_atom(atom);
    free(atom);
    return input;
  }
}

// Parsing
sexp_t *sexp_parse(const char *input) {
  assert(input);

  sexp_t *result;
  const char *end = parse_sexp_impl(input, &result);
  if (!end) {
    return NULL;
  }

  return result;
}
