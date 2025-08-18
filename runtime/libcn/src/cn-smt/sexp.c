#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/string_builder.h>
#include <cn-smt/sexp.h>

char *sexp_to_string(sexp_t *sexp) {
  if (!sexp) {
    return "";
  }

  if (sexp_is_atom(sexp)) {
    return sexp->data.atom;
  } else {
    // For lists, build a string representation
    size_t count;
    sexp_t **elements = sexp_to_list(sexp, &count);
    if (!elements) {
      return "()";
    }

    bennet_string_builder *sb = malloc(sizeof(bennet_string_builder));
    bennet_sb_init(sb);

    bennet_sb_append(sb, "(");

    for (size_t i = 0; i < count; i++) {
      if (i > 0) {
        bennet_sb_append(sb, " ");
      }

      bennet_sb_append(sb, sexp_to_string(elements[i]));
    }
    bennet_sb_append(sb, ")");

    char *result = bennet_sb_build(sb);
    bennet_sb_free(sb);

    return result;
  }
}

////////////////
/* SMT Basics */
////////////////

// Constructor functions
sexp_t *sexp_atom(const char *str) {
  sexp_t *sexp = malloc(sizeof(sexp_t));
  if (!sexp) {
    return NULL;
  }

  sexp->type = SEXP_ATOM;
  sexp->data.atom = malloc(strlen(str) + 1);
  if (!sexp->data.atom) {
    free(sexp);
    return NULL;
  }
  strcpy(sexp->data.atom, str);
  return sexp;
}

sexp_t *sexp_list(sexp_t **elements, size_t count) {
  sexp_t *sexp = malloc(sizeof(sexp_t));
  if (!sexp) {
    return NULL;
  }

  sexp->type = SEXP_LIST;
  sexp->data.list.count = count;
  sexp->data.list.capacity = count;

  if (count > 0) {
    sexp->data.list.elements = malloc(sizeof(sexp_t *) * count);
    if (!sexp->data.list.elements) {
      free(sexp);
      return NULL;
    }
    for (size_t i = 0; i < count; i++) {
      sexp->data.list.elements[i] = elements[i];
    }
  } else {
    sexp->data.list.elements = NULL;
  }

  return sexp;
}

// Query functions
bool sexp_is_atom(const sexp_t *sexp) {
  return sexp && sexp->type == SEXP_ATOM;
}

sexp_t **sexp_to_list(const sexp_t *sexp, size_t *count) {
  if (!sexp || sexp->type != SEXP_LIST) {
    if (count) {
      *count = 0;
    }

    return NULL;
  }

  if (count) {
    *count = sexp->data.list.count;
  }

  return sexp->data.list.elements;
}

// Pattern matching functions
sexp_t *sexp_to_assert(const sexp_t *sexp) {
  if (!sexp || sexp->type != SEXP_LIST) {
    return NULL;
  }
  if (sexp->data.list.count != 2) {
    return NULL;
  }

  sexp_t *first = sexp->data.list.elements[0];
  if (!first || first->type != SEXP_ATOM) {
    return NULL;
  }
  if (strcmp(first->data.atom, "assert") != 0) {
    return NULL;
  }

  return sexp->data.list.elements[1];
}

// Memory management
void sexp_free(sexp_t *sexp) {
  // FIXME: Actually figure out memory management
  // if (!sexp) {
  //   return;
  // }

  // if (sexp->type == SEXP_ATOM) {
  //   free(sexp->data.atom);
  // } else if (sexp->type == SEXP_LIST) {
  //   for (size_t i = 0; i < sexp->data.list.count; i++) {
  //     sexp_free(sexp->data.list.elements[i]);
  //   }
  //   free(sexp->data.list.elements);
  // }
  // free(sexp);
}

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
  if (!*atom) {
    return NULL;
  }

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
      if (!new_elements) {
        for (size_t i = 0; i < count; i++) {
          sexp_free(elements[i]);
        }
        free(elements);
        return NULL;
      }
      elements = new_elements;
    }

    input = parse_sexp_impl(input, &elements[count]);
    if (!input) {
      for (size_t i = 0; i <= count; i++) {
        sexp_free(elements[i]);
      }
      free(elements);
      return NULL;
    }
    count++;
    input = skip_whitespace(input);
  }

  if (*input != ')') {
    for (size_t i = 0; i < count; i++) {
      sexp_free(elements[i]);
    }
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
  if (!input) {
    return NULL;
  }

  sexp_t *result;
  const char *end = parse_sexp_impl(input, &result);
  if (!end) {
    return NULL;
  }

  return result;
}

// SMT-LIB construction helpers

/** Apply a function to some arguments. */
sexp_t *sexp_app(sexp_t *f, sexp_t **args, size_t arg_count) {
  if (!f) {
    return NULL;
  }

  if (arg_count == 0) {
    // If no args, return a copy of f
    if (f->type == SEXP_ATOM) {
      return sexp_atom(f->data.atom);
    } else {
      // For lists, we need to deep copy
      sexp_t **new_elements = malloc(sizeof(sexp_t *) * f->data.list.count);
      if (!new_elements) {
        return NULL;
      }

      for (size_t i = 0; i < f->data.list.count; i++) {
        new_elements[i] = sexp_app(f->data.list.elements[i], NULL, 0);
        if (!new_elements[i]) {
          for (size_t j = 0; j < i; j++) {
            sexp_free(new_elements[j]);
          }
          free(new_elements);
          return NULL;
        }
      }

      sexp_t *result = sexp_list(new_elements, f->data.list.count);
      free(new_elements);
      return result;
    }
  }

  // Create a list with f as first element, followed by args
  sexp_t **elements = malloc(sizeof(sexp_t *) * (arg_count + 1));
  if (!elements) {
    return NULL;
  }

  elements[0] = sexp_app(f, NULL, 0);  // Copy f
  if (!elements[0]) {
    free(elements);
    return NULL;
  }

  for (size_t i = 0; i < arg_count; i++) {
    elements[i + 1] = sexp_app(args[i], NULL, 0);  // Deep copy each argument
    if (!elements[i + 1]) {
      // Cleanup on failure
      sexp_free(elements[0]);
      for (size_t j = 1; j < i + 1; j++) {
        sexp_free(elements[j]);
      }
      free(elements);
      return NULL;
    }
  }

  sexp_t *result = sexp_list(elements, arg_count + 1);
  free(elements);
  return result;
}

/** Apply a function to some arguments */
sexp_t *sexp_app_str(const char *f, sexp_t **args, size_t arg_count) {
  sexp_t *f_atom = sexp_atom(f);
  if (!f_atom) {
    return NULL;
  }

  sexp_t *result = sexp_app(f_atom, args, arg_count);
  sexp_free(f_atom);
  return result;
}

/** Type annotation */
sexp_t *sexp_as_type(sexp_t *x, sexp_t *t) {
  if (!x || !t) {
    return NULL;
  }

  sexp_t *args[] = {x, t};
  return sexp_app_str("as", args, 2);
}

/** Let expression */
sexp_t *sexp_let(sexp_t **bindings, size_t binding_count, sexp_t *e) {
  if (!e) {
    return NULL;
  }

  if (binding_count == 0) {
    // Return a copy of e
    return sexp_app(e, NULL, 0);
  }

  // Create bindings list
  sexp_t *bindings_list = sexp_list(bindings, binding_count);
  if (!bindings_list) {
    return NULL;
  }

  sexp_t *args[] = {bindings_list, e};
  sexp_t *result = sexp_app_str("let", args, 2);
  sexp_free(bindings_list);
  return result;
}

/** Non-negative numeric constant. */
sexp_t *sexp_nat_k(int x) {
  char buffer[32];
  snprintf(buffer, sizeof(buffer), "%d", x);
  return sexp_atom(buffer);
}

/** Indexed family */
sexp_t *sexp_fam(const char *f, sexp_t **indices, size_t index_count) {
  if (!f) {
    return NULL;
  }

  // Create list: (_ f indices...)
  sexp_t **elements = malloc(sizeof(sexp_t *) * (index_count + 2));
  if (!elements) {
    return NULL;
  }

  elements[0] = sexp_atom("_");
  if (!elements[0]) {
    free(elements);
    return NULL;
  }

  elements[1] = sexp_atom(f);
  if (!elements[1]) {
    sexp_free(elements[0]);
    free(elements);
    return NULL;
  }

  for (size_t i = 0; i < index_count; i++) {
    elements[i + 2] = indices[i];
  }

  sexp_t *result = sexp_list(elements, index_count + 2);
  free(elements);
  return result;
}

/** Int-indexed family */
sexp_t *sexp_ifam(const char *f, int *indices, size_t index_count) {
  if (!f) {
    return NULL;
  }

  // Convert int indices to sexp atoms
  sexp_t **sexp_indices = malloc(sizeof(sexp_t *) * index_count);
  if (!sexp_indices) {
    return NULL;
  }

  for (size_t i = 0; i < index_count; i++) {
    sexp_indices[i] = sexp_nat_k(indices[i]);
    if (!sexp_indices[i]) {
      for (size_t j = 0; j < i; j++) {
        sexp_free(sexp_indices[j]);
      }
      free(sexp_indices);
      return NULL;
    }
  }

  sexp_t *result = sexp_fam(f, sexp_indices, index_count);

  // Free the temporary sexp indices
  for (size_t i = 0; i < index_count; i++) {
    sexp_free(sexp_indices[i]);
  }
  free(sexp_indices);

  return result;
}

/** Attribute */
sexp_t *sexp_named(const char *name, sexp_t *e) {
  if (!name || !e) {
    return NULL;
  }

  sexp_t *name_atom = sexp_atom(name);
  if (!name_atom) {
    return NULL;
  }

  sexp_t *named_atom = sexp_atom(":named");
  if (!named_atom) {
    sexp_free(name_atom);
    return NULL;
  }

  sexp_t *args[] = {e, named_atom, name_atom};
  sexp_t *result = sexp_app_str("!", args, 3);

  sexp_free(name_atom);
  sexp_free(named_atom);
  return result;
}

/** Check if a character is allowed in a simple symbol.
    Symbols are either simple or quoted (c.f. SMTLIB v2.6 S3.1).
    This predicate indicates whether a character is allowed in a simple
    symbol. Note that only ASCII letters are allowed. */
bool allowed_simple_char(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
         strchr("~!@$%^&*_-+=<>.?/", c) != NULL;
}

/** Check if a string is a simple symbol */
bool is_simple_symbol(const char *s) {
  if (!s || *s == '\0') {
    return false;
  }

  while (*s) {
    if (!allowed_simple_char(*s)) {
      return false;
    }

    s++;
  }
  return true;
}

/** Quote a symbol if needed for SMTLIB */
char *quote_symbol(const char *s) {
  if (!s) {
    return NULL;
  }

  if (is_simple_symbol(s)) {
    // Return a copy of the original string
    size_t len = strlen(s);
    char *result = malloc(len + 1);
    if (!result) {
      return NULL;
    }
    strcpy(result, s);
    return result;
  } else {
    // Return quoted version: |s|
    size_t len = strlen(s);
    char *result = malloc(len + 3);  // +2 for pipes, +1 for null terminator
    if (!result) {
      return NULL;
    }

    result[0] = '|';
    strcpy(result + 1, s);
    result[len + 1] = '|';
    result[len + 2] = '\0';
    return result;
  }
}

/** Make an SMT name, quoting if needed. Note that even with quoting
    the name should not contain pipe (|) or backslash (\) */
sexp_t *symbol(const char *x) {
  if (!x) {
    return NULL;
  }

  char *quoted = quote_symbol(x);
  if (!quoted) {
    return NULL;
  }

  sexp_t *result = sexp_atom(quoted);
  free(quoted);
  return result;
}

////////////////////////
/* Integers and Reals */
////////////////////////

/** The type of booleans. */
sexp_t *t_bool(void) {
  return sexp_atom("Bool");
}

/** Boolean constant */
sexp_t *bool_k(bool b) {
  return sexp_atom(b ? "true" : "false");
}

/** If-then-else. This is polymorphic and can be used to construct any term. */
sexp_t *ite(sexp_t *x, sexp_t *y, sexp_t *z) {
  if (!x || !y || !z) {
    return NULL;
  }

  sexp_t *args[] = {x, y, z};
  return sexp_app_str("ite", args, 3);
}

/** Arguments are equal. */
sexp_t *eq(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("=", args, 2);
}

/** All arguments are pairwise different. */
sexp_t *distinct(sexp_t **xs, size_t count) {
  if (count == 0) {
    return bool_k(true);
  }

  return sexp_app_str("distinct", xs, count);
}

/** Logical negation. */
sexp_t *bool_not(sexp_t *p) {
  if (!p) {
    return NULL;
  }

  sexp_t *args[] = {p};
  return sexp_app_str("not", args, 1);
}

/** Conjunction. */
sexp_t *bool_and(sexp_t *p, sexp_t *q) {
  if (!p || !q) {
    return NULL;
  }

  sexp_t *args[] = {p, q};
  return sexp_app_str("and", args, 2);
}

/** Conjunction. */
sexp_t *bool_ands(sexp_t **ps, size_t count) {
  if (count == 0) {
    return bool_k(true);
  }

  return sexp_app_str("and", ps, count);
}

/** Disjunction. */
sexp_t *bool_or(sexp_t *p, sexp_t *q) {
  if (!p || !q) {
    return NULL;
  }

  sexp_t *args[] = {p, q};
  return sexp_app_str("or", args, 2);
}

/** Disjunction. */
sexp_t *bool_ors(sexp_t **ps, size_t count) {
  if (count == 0) {
    return bool_k(false);
  }

  return sexp_app_str("or", ps, count);
}

/** Exclusive-or. */
sexp_t *bool_xor(sexp_t *p, sexp_t *q) {
  if (!p || !q) {
    return NULL;
  }

  sexp_t *args[] = {p, q};
  return sexp_app_str("xor", args, 2);
}

/** Implication. */
sexp_t *bool_implies(sexp_t *p, sexp_t *q) {
  if (!p || !q) {
    return NULL;
  }

  sexp_t *args[] = {p, q};
  return sexp_app_str("=>", args, 2);
}

/** The type of integers. */
sexp_t *t_int(void) {
  return sexp_atom("Int");
}

/** The type of reals. */
sexp_t *t_real(void) {
  return sexp_atom("Real");
}

/** Numeric negation. */
sexp_t *num_neg(sexp_t *x) {
  if (!x) {
    return NULL;
  }

  sexp_t *args[] = {x};
  return sexp_app_str("-", args, 1);
}

/** Integer constant */
sexp_t *int_k(int x) {
  if (x < 0) {
    return num_neg(sexp_nat_k(-x));
  }

  return sexp_nat_k(x);
}

/** Division of real numbers. */
sexp_t *real_div(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("/", args, 2);
}

/** Greater-then for numbers. */
sexp_t *num_gt(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str(">", args, 2);
}

/** Less-then for numbers. */
sexp_t *num_lt(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("<", args, 2);
}

/** Greater-than-or-equal-to for numbers. */
sexp_t *num_geq(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str(">=", args, 2);
}

/** Less-than-or-equal-to for numbers. */
sexp_t *num_leq(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("<=", args, 2);
}

/** Numeric addition. */
sexp_t *num_add(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("+", args, 2);
}

/** Numeric subtraction. */
sexp_t *num_sub(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("-", args, 2);
}

/** Numeric multiplication. */
sexp_t *num_mul(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("*", args, 2);
}

/** Numeric absolute value. */
sexp_t *num_abs(sexp_t *x) {
  if (!x) {
    return NULL;
  }

  sexp_t *args[] = {x};
  return sexp_app_str("abs", args, 1);
}

/** Numeric division. */
sexp_t *num_div(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("div", args, 2);
}

/** Numeric modulus. */
sexp_t *num_mod(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("mod", args, 2);
}

/** Numeric reminder. Nonstandard. */
sexp_t *num_rem(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("rem", args, 2);
}

/** Is the number divisible by the given constant? */
sexp_t *num_divisible(sexp_t *x, int n) {
  if (!x) {
    return NULL;
  }

  int indices[] = {n};
  sexp_t *divisible_indexed = sexp_ifam("divisible", indices, 1);
  if (!divisible_indexed) {
    return NULL;
  }

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(divisible_indexed, args, 1);
  sexp_free(divisible_indexed);
  return result;
}

/** Satisfies [real_to_int x <= x] (i.e., this is like [floor]) */
sexp_t *real_to_int(sexp_t *e) {
  if (!e) {
    return NULL;
  }

  sexp_t *args[] = {e};
  return sexp_app_str("to_int", args, 1);
}

/** Promote an integer to a real. */
sexp_t *int_to_real(sexp_t *e) {
  if (!e) {
    return NULL;
  }

  sexp_t *args[] = {e};
  return sexp_app_str("to_real", args, 1);
}

/////////////////
/* Bit-vectors */
/////////////////

/** The type of bit vectors of width w. */
sexp_t *t_bits(int w) {
  int indices[] = {w};
  return sexp_ifam("BitVec", indices, 1);
}

/** A bit-vector represented in binary.
    - The number should be non-negative.
    - The number should not exceed the number of bits. */
sexp_t *bv_nat_bin(int w, long long v) {
  if (v < 0) {
    return NULL;
  }

  // Create binary string with proper width
  char *binary_str = malloc(w + 1);
  if (!binary_str) {
    return NULL;
  }

  // Convert to binary
  for (int i = w - 1; i >= 0; i--) {
    binary_str[w - 1 - i] = ((v >> i) & 1) ? '1' : '0';
  }
  binary_str[w] = '\0';

  // Create #b prefix + binary string
  size_t total_len = 2 + w + 1;  // #b + binary + null
  char *result_str = malloc(total_len);
  if (!result_str) {
    free(binary_str);
    return NULL;
  }

  snprintf(result_str, total_len, "#b%s", binary_str);
  free(binary_str);

  sexp_t *result = sexp_atom(result_str);
  free(result_str);
  return result;
}

/** A bit-vector represented in hex.
    - The number should be non-negative.
    - The number should not exceed the number of bits.
    - The width should be a multiple of 4. */
sexp_t *bv_nat_hex(int w, long long v) {
  if (v < 0 || w % 4 != 0) {
    return NULL;
  }

  int hex_digits = w / 4;
  char *format_str = malloc(32);
  if (!format_str) {
    return NULL;
  }

  snprintf(format_str, 32, "#x%%0%dllx", hex_digits);

  char *result_str = malloc(hex_digits + 3);  // #x + digits + null
  if (!result_str) {
    free(format_str);
    return NULL;
  }

  snprintf(result_str, hex_digits + 3, format_str, v);
  free(format_str);

  sexp_t *result = sexp_atom(result_str);
  free(result_str);
  return result;
}

/** Bit vector arithmetic negation. */
sexp_t *bv_neg(sexp_t *x) {
  if (!x) {
    return NULL;
  }

  sexp_t *args[] = {x};
  return sexp_app_str("bvneg", args, 1);
}

/** Bit vector bitwise complement. */
sexp_t *bv_compl(sexp_t *x) {
  if (!x) {
    return NULL;
  }

  sexp_t *args[] = {x};
  return sexp_app_str("bvnot", args, 1);
}

/** A bit-vector represented in binary.
    The number should fit in the given number of bits. */
sexp_t *bv_bin(int w, long long v) {
  if (v >= 0) {
    return bv_nat_bin(w, v);
  } else {
    sexp_t *pos_bv = bv_nat_bin(w, -v);
    if (!pos_bv) {
      return NULL;
    }

    sexp_t *result = bv_neg(pos_bv);
    sexp_free(pos_bv);
    return result;
  }
}

/** A bit-vector represented in hex.
    - The number should fit in the given number of bits.
    - The width should be a multiple of 4. */
sexp_t *bv_hex(int w, long long v) {
  if (v >= 0) {
    return bv_nat_hex(w, v);
  } else {
    sexp_t *pos_bv = bv_nat_hex(w, -v);
    if (!pos_bv) {
      return NULL;
    }

    sexp_t *result = bv_neg(pos_bv);
    sexp_free(pos_bv);
    return result;
  }
}

/** Make a bit-vector literal. Uses hex representation if the size
    is a multiple of 4, and binary otherwise. */
sexp_t *bv_k(int w, long long v) {
  if (w % 4 == 0) {
    return bv_hex(w, v);
  } else {
    return bv_bin(w, v);
  }
}

sexp_t *loc_k(uintptr_t ptr) {
  return bv_k(CHAR_BIT * sizeof(uintptr_t), ptr);
}

/** Unsigned less-than on bit-vectors. */
sexp_t *bv_ult(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvult", args, 2);
}

/** Unsigned less-than-or-equal on bit-vectors. */
sexp_t *bv_uleq(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvule", args, 2);
}

/** Signed less-than on bit-vectors. */
sexp_t *bv_slt(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvslt", args, 2);
}

/** Signed less-than-or-equal on bit-vectors. */
sexp_t *bv_sleq(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsle", args, 2);
}

/** Bit vector concatenation. */
sexp_t *bv_concat(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("concat", args, 2);
}

/** Extend to the signed equivalent bitvector by the given number of bits. */
sexp_t *bv_sign_extend(int i, sexp_t *x) {
  if (!x) {
    return NULL;
  }

  int indices[] = {i};
  sexp_t *sign_extend_indexed = sexp_ifam("sign_extend", indices, 1);
  if (!sign_extend_indexed) {
    return NULL;
  }

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(sign_extend_indexed, args, 1);
  sexp_free(sign_extend_indexed);
  return result;
}

/** Zero extend by the given number of bits. */
sexp_t *bv_zero_extend(int i, sexp_t *x) {
  if (!x) {
    return NULL;
  }

  int indices[] = {i};
  sexp_t *zero_extend_indexed = sexp_ifam("zero_extend", indices, 1);
  if (!zero_extend_indexed) {
    return NULL;
  }

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(zero_extend_indexed, args, 1);
  sexp_free(zero_extend_indexed);
  return result;
}

/** [bv_extract last_ix first_ix x] is a sub-vector of [x].
    [last_ix] is the larger bit index, [first_ix] is the smaller one, and indexing
    is inclusive. */
sexp_t *bv_extract(int last_ix, int first_ix, sexp_t *x) {
  if (!x) {
    return NULL;
  }

  int indices[] = {last_ix, first_ix};
  sexp_t *extract_indexed = sexp_ifam("extract", indices, 2);
  if (!extract_indexed) {
    return NULL;
  }

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(extract_indexed, args, 1);
  sexp_free(extract_indexed);
  return result;
}

/** Bitwise negation. */
sexp_t *bv_not(sexp_t *x) {
  if (!x) {
    return NULL;
  }

  sexp_t *args[] = {x};
  return sexp_app_str("bvnot", args, 1);
}

/** Bitwise conjunction. */
sexp_t *bv_and(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvand", args, 2);
}

/** Bitwise disjunction. */
sexp_t *bv_or(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvor", args, 2);
}

/** Bitwise exclusive or. */
sexp_t *bv_xor(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvxor", args, 2);
}

/** Addition of bit vectors. */
sexp_t *bv_add(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvadd", args, 2);
}

/** Subtraction of bit vectors. */
sexp_t *bv_sub(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsub", args, 2);
}

/** Multiplication of bit vectors. */
sexp_t *bv_mul(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvmul", args, 2);
}

/** Bit vector unsigned division. */
sexp_t *bv_udiv(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvudiv", args, 2);
}

/** Bit vector unsigned remainder. */
sexp_t *bv_urem(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvurem", args, 2);
}

/** Bit vector signed division. */
sexp_t *bv_sdiv(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsdiv", args, 2);
}

/** Bit vector signed remainder. */
sexp_t *bv_srem(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsrem", args, 2);
}

/** Bit vector signed modulus. Nonstandard? */
sexp_t *bv_smod(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsmod", args, 2);
}

/** Shift left. */
sexp_t *bv_shl(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvshl", args, 2);
}

/** Logical shift right. */
sexp_t *bv_lshr(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvlshr", args, 2);
}

/** Arithmetic shift right (copies most significant bit). */
sexp_t *bv_ashr(sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvashr", args, 2);
}

////////////
/* Arrays */
////////////

/** The type of arrays with keys kt and values vt */
sexp_t *t_array(sexp_t *kt, sexp_t *vt) {
  if (!kt || !vt) {
    return NULL;
  }

  sexp_t *args[] = {kt, vt};
  return sexp_app_str("Array", args, 2);
}

/** An array of type Array kt vt where all elements are v.
    v should be of type vt. */
sexp_t *arr_const(sexp_t *kt, sexp_t *vt, sexp_t *v) {
  if (!kt || !vt || !v) {
    return NULL;
  }

  sexp_t *const_atom = sexp_atom("const");
  if (!const_atom) {
    return NULL;
  }

  sexp_t *array_type = t_array(kt, vt);
  if (!array_type) {
    sexp_free(const_atom);
    return NULL;
  }

  sexp_t *typed_const = sexp_as_type(const_atom, array_type);
  sexp_free(const_atom);
  sexp_free(array_type);
  if (!typed_const) {
    return NULL;
  }

  sexp_t *args[] = {v};
  sexp_t *result = sexp_app(typed_const, args, 1);
  sexp_free(typed_const);
  return result;
}

/** The element stored at index i of arr. */
sexp_t *arr_select(sexp_t *arr, sexp_t *i) {
  if (!arr || !i) {
    return NULL;
  }

  sexp_t *args[] = {arr, i};
  return sexp_app_str("select", args, 2);
}

/** An array that is the same as arr, except at index i it has v */
sexp_t *arr_store(sexp_t *arr, sexp_t *i, sexp_t *v) {
  if (!arr || !i || !v) {
    return NULL;
  }

  sexp_t *args[] = {arr, i, v};
  return sexp_app_str("store", args, 3);
}

//////////
/* Sets */
//////////

/** The type of sets with elements of type t */
sexp_t *t_set(sexp_t *x) {
  if (!x) {
    return NULL;
  }

  sexp_t *args[] = {x};
  return sexp_app_str("Set", args, 1);
}

/** The empty set with elements of type t */
sexp_t *set_empty(solver_extensions_t ext, sexp_t *t) {
  if (!t) {
    return NULL;
  }

  if (ext == SOLVER_CVC5) {
    sexp_t *empty_atom = sexp_atom("set.empty");
    if (!empty_atom) {
      return NULL;
    }

    sexp_t *set_type = t_set(t);
    if (!set_type) {
      sexp_free(empty_atom);
      return NULL;
    }

    sexp_t *args[] = {empty_atom, set_type};
    sexp_t *result = sexp_app_str("as", args, 2);
    sexp_free(empty_atom);
    sexp_free(set_type);
    return result;
  } else {
    sexp_t *const_atom = sexp_atom("const");
    if (!const_atom) {
      return NULL;
    }

    sexp_t *set_type = t_set(t);
    if (!set_type) {
      sexp_free(const_atom);
      return NULL;
    }

    sexp_t *const_args[] = {const_atom, set_type};
    sexp_t *typed_const = sexp_app_str("as", const_args, 2);
    sexp_free(const_atom);
    sexp_free(set_type);
    if (!typed_const) {
      return NULL;
    }

    sexp_t *false_val = bool_k(false);
    if (!false_val) {
      sexp_free(typed_const);
      return NULL;
    }

    sexp_t *app_args[] = {false_val};
    sexp_t *result = sexp_app(typed_const, app_args, 1);
    sexp_free(typed_const);
    sexp_free(false_val);
    return result;
  }
}

/** The universal set with elements of type t */
sexp_t *set_universe(solver_extensions_t ext, sexp_t *t) {
  if (!t) {
    return NULL;
  }

  if (ext == SOLVER_CVC5) {
    sexp_t *universe_atom = sexp_atom("set.universe");
    if (!universe_atom) {
      return NULL;
    }

    sexp_t *set_type = t_set(t);
    if (!set_type) {
      sexp_free(universe_atom);
      return NULL;
    }

    sexp_t *args[] = {universe_atom, set_type};
    sexp_t *result = sexp_app_str("as", args, 2);
    sexp_free(universe_atom);
    sexp_free(set_type);
    return result;
  } else {
    sexp_t *const_atom = sexp_atom("const");
    if (!const_atom) {
      return NULL;
    }

    sexp_t *set_type = t_set(t);
    if (!set_type) {
      sexp_free(const_atom);
      return NULL;
    }

    sexp_t *const_args[] = {const_atom, set_type};
    sexp_t *typed_const = sexp_app_str("as", const_args, 2);
    sexp_free(const_atom);
    sexp_free(set_type);
    if (!typed_const) {
      return NULL;
    }

    sexp_t *true_val = bool_k(true);
    if (!true_val) {
      sexp_free(typed_const);
      return NULL;
    }

    sexp_t *app_args[] = {true_val};
    sexp_t *result = sexp_app(typed_const, app_args, 1);
    sexp_free(typed_const);
    sexp_free(true_val);
    return result;
  }
}

/** A set that has all elements of xs and also x */
sexp_t *set_insert(solver_extensions_t ext, sexp_t *x, sexp_t *xs) {
  if (!x || !xs) {
    return NULL;
  }

  if (ext == SOLVER_CVC5) {
    sexp_t *args[] = {x, xs};
    return sexp_app_str("set.insert", args, 2);
  } else {
    sexp_t *true_val = bool_k(true);
    if (!true_val) {
      return NULL;
    }

    sexp_t *result = arr_store(xs, x, true_val);
    sexp_free(true_val);
    return result;
  }
}

/** A set that has all elements from x and also from y */
sexp_t *set_union(solver_extensions_t ext, sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  const char *nm = (ext == SOLVER_CVC5) ? "set.union" : "union";
  sexp_t *args[] = {x, y};
  return sexp_app_str(nm, args, 2);
}

/** A set that has the elements that are in both x and y */
sexp_t *set_intersection(solver_extensions_t ext, sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  const char *nm = (ext == SOLVER_CVC5) ? "set.inter" : "intersection";
  sexp_t *args[] = {x, y};
  return sexp_app_str(nm, args, 2);
}

/** A set that has the elements of x that are not in y */
sexp_t *set_difference(solver_extensions_t ext, sexp_t *x, sexp_t *y) {
  if (!x || !y) {
    return NULL;
  }

  const char *nm = (ext == SOLVER_CVC5) ? "set.minus" : "setminus";
  sexp_t *args[] = {x, y};
  return sexp_app_str(nm, args, 2);
}

/** A set that has all elements that are not in x */
sexp_t *set_complement(solver_extensions_t ext, sexp_t *x) {
  if (!x) {
    return NULL;
  }

  const char *nm = (ext == SOLVER_CVC5) ? "set.complement" : "complement";
  sexp_t *args[] = {x};
  return sexp_app_str(nm, args, 1);
}

/** True if x is a member of xs */
sexp_t *set_member(solver_extensions_t ext, sexp_t *x, sexp_t *xs) {
  if (!x || !xs) {
    return NULL;
  }

  if (ext == SOLVER_CVC5) {
    sexp_t *args[] = {x, xs};
    return sexp_app_str("set.member", args, 2);
  } else {
    return arr_select(xs, x);
  }
}

/** True if all elements of xs are also in ys */
sexp_t *set_subset(solver_extensions_t ext, sexp_t *xs, sexp_t *ys) {
  if (!xs || !ys) {
    return NULL;
  }

  const char *nm = (ext == SOLVER_CVC5) ? "set.subset" : "subset";
  sexp_t *args[] = {xs, ys};
  return sexp_app_str(nm, args, 2);
}

/////////////////
/* Quantifiers */
/////////////////

/** Universal quantification over the given variable bindings. */
sexp_t *forall(sexp_t **bindings, size_t binding_count, sexp_t *p) {
  if (!p) {
    return NULL;
  }

  if (binding_count == 0) {
    // Return a copy of p if no bindings
    return sexp_app(p, NULL, 0);
  }

  // Create bindings list
  sexp_t *bindings_list = sexp_list(bindings, binding_count);
  if (!bindings_list) {
    return NULL;
  }

  sexp_t *args[] = {bindings_list, p};
  sexp_t *result = sexp_app_str("forall", args, 2);
  sexp_free(bindings_list);
  return result;
}

////////////
/* Commands */
////////////

/** A command made out of just atoms. */
sexp_t *simple_command(const char **strs, size_t count) {
  if (!strs) {
    return NULL;
  }

  sexp_t **atoms = malloc(sizeof(sexp_t *) * count);
  if (!atoms) {
    return NULL;
  }

  for (size_t i = 0; i < count; i++) {
    atoms[i] = sexp_atom(strs[i]);
    if (!atoms[i]) {
      for (size_t j = 0; j < i; j++) {
        sexp_free(atoms[j]);
      }
      free(atoms);
      return NULL;
    }
  }

  sexp_t *result = sexp_list(atoms, count);
  free(atoms);
  return result;
}

/** Set option to value. */
sexp_t *set_option(const char *opt, const char *val) {
  const char *strs[] = {"set-option", opt, val};
  return simple_command(strs, 3);
}

/** Set the logic to use. */
sexp_t *set_logic(const char *logic) {
  const char *strs[] = {"set-logic", logic};
  return simple_command(strs, 2);
}

/** Push a new scope. */
sexp_t *push(int n) {
  char n_str[32];
  snprintf(n_str, sizeof(n_str), "%d", n);
  const char *strs[] = {"push", n_str};
  return simple_command(strs, 2);
}

/** Pop a scope. */
sexp_t *pop(int n) {
  char n_str[32];
  snprintf(n_str, sizeof(n_str), "%d", n);
  const char *strs[] = {"pop", n_str};
  return simple_command(strs, 2);
}

/** Declares a type with given arity. */
sexp_t *declare_sort(const char *name, int arity) {
  char arity_str[32];
  snprintf(arity_str, sizeof(arity_str), "%d", arity);

  sexp_t *name_atom = sexp_atom(name);
  sexp_t *arity_atom = sexp_atom(arity_str);
  if (!name_atom || !arity_atom) {
    sexp_free(name_atom);
    sexp_free(arity_atom);
    return NULL;
  }

  sexp_t *args[] = {name_atom, arity_atom};
  sexp_t *result = sexp_app_str("declare-sort", args, 2);
  sexp_free(name_atom);
  sexp_free(arity_atom);
  return result;
}

/** Declares a function with given parameter types and result type. */
sexp_t *declare_fun(
    const char *name, sexp_t **param_types, size_t param_count, sexp_t *result_type) {
  if (!name || !result_type) {
    return NULL;
  }

  sexp_t *name_atom = sexp_atom(name);
  if (!name_atom) {
    return NULL;
  }

  sexp_t *params_list = sexp_list(param_types, param_count);
  if (!params_list) {
    sexp_free(name_atom);
    return NULL;
  }

  sexp_t *args[] = {name_atom, params_list, result_type};
  sexp_t *result = sexp_app_str("declare-fun", args, 3);
  sexp_free(name_atom);
  sexp_free(params_list);
  return result;
}

/** Declares a constant of given type. */
sexp_t *declare_const(const char *name, sexp_t *type) {
  return declare_fun(name, NULL, 0, type);
}

/** Defines a function with parameters, result type and definition. */
sexp_t *define_fun(const char *name,
    sexp_t **params,
    size_t param_count,
    sexp_t *result_type,
    sexp_t *definition) {
  if (!name || !result_type || !definition) {
    return NULL;
  }

  sexp_t *name_atom = sexp_atom(name);
  if (!name_atom) {
    return NULL;
  }

  sexp_t *params_list = sexp_list(params, param_count);
  if (!params_list) {
    sexp_free(name_atom);
    return NULL;
  }

  sexp_t *args[] = {name_atom, params_list, result_type, definition};
  sexp_t *result = sexp_app_str("define-fun", args, 4);
  sexp_free(name_atom);
  sexp_free(params_list);
  return result;
}

/** Defines a constant of given type and definition. */
sexp_t *define_const(const char *name, sexp_t *type, sexp_t *definition) {
  return define_fun(name, NULL, 0, type, definition);
}

/** Constructor field and constructor types are defined in the header */

/** Declares a single datatype. */
sexp_t *declare_datatype(const char *name,
    const char **type_params,
    size_t type_param_count,
    constructor_t *constructors,
    size_t constructor_count) {
  if (!name || !constructors) {
    return NULL;
  }

  sexp_t *name_atom = sexp_atom(name);
  if (!name_atom) {
    return NULL;
  }

  // Build constructors list
  sexp_t **cons_exprs = malloc(sizeof(sexp_t *) * constructor_count);
  if (!cons_exprs) {
    sexp_free(name_atom);
    return NULL;
  }

  for (size_t i = 0; i < constructor_count; i++) {
    // Build field list for this constructor
    sexp_t **field_exprs = malloc(sizeof(sexp_t *) * (constructors[i].field_count + 1));
    if (!field_exprs) {
      for (size_t j = 0; j < i; j++) {
        sexp_free(cons_exprs[j]);
      }
      free(cons_exprs);
      sexp_free(name_atom);
      return NULL;
    }

    // Constructor name
    field_exprs[0] = sexp_atom(constructors[i].name);
    if (!field_exprs[0]) {
      free(field_exprs);
      for (size_t j = 0; j < i; j++) {
        sexp_free(cons_exprs[j]);
      }
      free(cons_exprs);
      sexp_free(name_atom);
      return NULL;
    }

    // Fields
    for (size_t f = 0; f < constructors[i].field_count; f++) {
      sexp_t *field_name = sexp_atom(constructors[i].fields[f].name);
      if (!field_name) {
        for (size_t k = 0; k <= f; k++) {
          sexp_free(field_exprs[k]);
        }
        free(field_exprs);
        for (size_t j = 0; j < i; j++) {
          sexp_free(cons_exprs[j]);
        }
        free(cons_exprs);
        sexp_free(name_atom);
        return NULL;
      }

      sexp_t *field_args[] = {field_name, constructors[i].fields[f].type};
      field_exprs[f + 1] = sexp_list(field_args, 2);
      sexp_free(field_name);
      if (!field_exprs[f + 1]) {
        for (size_t k = 0; k <= f; k++) {
          sexp_free(field_exprs[k]);
        }
        free(field_exprs);
        for (size_t j = 0; j < i; j++) {
          sexp_free(cons_exprs[j]);
        }
        free(cons_exprs);
        sexp_free(name_atom);
        return NULL;
      }
    }

    cons_exprs[i] = sexp_list(field_exprs, constructors[i].field_count + 1);
    free(field_exprs);
    if (!cons_exprs[i]) {
      for (size_t j = 0; j < i; j++) {
        sexp_free(cons_exprs[j]);
      }
      free(cons_exprs);
      sexp_free(name_atom);
      return NULL;
    }
  }

  sexp_t *cons_list = sexp_list(cons_exprs, constructor_count);
  free(cons_exprs);
  if (!cons_list) {
    sexp_free(name_atom);
    return NULL;
  }

  sexp_t *def;
  if (type_param_count == 0) {
    def = cons_list;
  } else {
    // Build type parameters list
    sexp_t **type_param_atoms = malloc(sizeof(sexp_t *) * type_param_count);
    if (!type_param_atoms) {
      sexp_free(name_atom);
      sexp_free(cons_list);
      return NULL;
    }

    for (size_t i = 0; i < type_param_count; i++) {
      type_param_atoms[i] = sexp_atom(type_params[i]);
      if (!type_param_atoms[i]) {
        for (size_t j = 0; j < i; j++) {
          sexp_free(type_param_atoms[j]);
        }
        free(type_param_atoms);
        sexp_free(name_atom);
        sexp_free(cons_list);
        return NULL;
      }
    }

    sexp_t *type_params_list = sexp_list(type_param_atoms, type_param_count);
    free(type_param_atoms);
    if (!type_params_list) {
      sexp_free(name_atom);
      sexp_free(cons_list);
      return NULL;
    }

    sexp_t *par_args[] = {type_params_list, cons_list};
    def = sexp_app_str("par", par_args, 2);
    sexp_free(type_params_list);
    sexp_free(cons_list);
    if (!def) {
      sexp_free(name_atom);
      return NULL;
    }
  }

  sexp_t *args[] = {name_atom, def};
  sexp_t *result = sexp_app_str("declare-datatype", args, 2);
  sexp_free(name_atom);
  if (type_param_count > 0) {
    sexp_free(def);
  }
  return result;
}

/** Pattern and match alternative types are defined in the header */

/** Match datatype expression. */
sexp_t *match_datatype(sexp_t *expr, match_alt_t *alts, size_t alt_count) {
  if (!expr || !alts) {
    return NULL;
  }

  sexp_t **alt_exprs = malloc(sizeof(sexp_t *) * alt_count);
  if (!alt_exprs) {
    return NULL;
  }

  for (size_t i = 0; i < alt_count; i++) {
    sexp_t *pat_expr;

    if (alts[i].pattern.type == PAT_VAR) {
      pat_expr = sexp_atom(alts[i].pattern.data.var_name);
    } else {
      // PAT_CON
      sexp_t **con_elements =
          malloc(sizeof(sexp_t *) * (alts[i].pattern.data.con.var_count + 1));
      if (!con_elements) {
        for (size_t j = 0; j < i; j++) {
          sexp_free(alt_exprs[j]);
        }
        free(alt_exprs);
        return NULL;
      }

      con_elements[0] = sexp_atom(alts[i].pattern.data.con.con_name);
      if (!con_elements[0]) {
        free(con_elements);
        for (size_t j = 0; j < i; j++) {
          sexp_free(alt_exprs[j]);
        }
        free(alt_exprs);
        return NULL;
      }

      for (size_t v = 0; v < alts[i].pattern.data.con.var_count; v++) {
        con_elements[v + 1] = sexp_atom(alts[i].pattern.data.con.var_names[v]);
        if (!con_elements[v + 1]) {
          for (size_t k = 0; k <= v; k++) {
            sexp_free(con_elements[k]);
          }
          free(con_elements);
          for (size_t j = 0; j < i; j++) {
            sexp_free(alt_exprs[j]);
          }
          free(alt_exprs);
          return NULL;
        }
      }

      pat_expr = sexp_list(con_elements, alts[i].pattern.data.con.var_count + 1);
      free(con_elements);
    }

    if (!pat_expr) {
      for (size_t j = 0; j < i; j++) {
        sexp_free(alt_exprs[j]);
      }
      free(alt_exprs);
      return NULL;
    }

    sexp_t *alt_args[] = {pat_expr, alts[i].expr};
    alt_exprs[i] = sexp_list(alt_args, 2);
    sexp_free(pat_expr);
    if (!alt_exprs[i]) {
      for (size_t j = 0; j < i; j++) {
        sexp_free(alt_exprs[j]);
      }
      free(alt_exprs);
      return NULL;
    }
  }

  sexp_t *alts_list = sexp_list(alt_exprs, alt_count);
  free(alt_exprs);
  if (!alts_list) {
    return NULL;
  }

  sexp_t *args[] = {expr, alts_list};
  sexp_t *result = sexp_app_str("match", args, 2);
  sexp_free(alts_list);
  return result;
}

/** Test if expression is constructed with given constructor. */
sexp_t *is_con(const char *constructor, sexp_t *expr) {
  if (!constructor || !expr) {
    return NULL;
  }

  sexp_t *con_atom = sexp_atom(constructor);
  if (!con_atom) {
    return NULL;
  }

  sexp_t *indices[] = {con_atom};
  sexp_t *is_indexed = sexp_fam("is", indices, 1);
  sexp_free(con_atom);
  if (!is_indexed) {
    return NULL;
  }

  sexp_t *args[] = {expr};
  sexp_t *result = sexp_app(is_indexed, args, 1);
  sexp_free(is_indexed);
  return result;
}

/** Add an assertion to the current scope. */
sexp_t *assume(sexp_t *expr) {
  if (!expr) {
    return NULL;
  }

  sexp_t *args[] = {expr};
  return sexp_app_str("assert", args, 1);
}
