#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/bump_alloc.h>
#include <cn-smt/memory/intern.h>
#include <cn-smt/sexp.h>

////////////////
/* SMT Basics */
////////////////

// Constructor functions
sexp_t *sexp_atom(const char *str) {
  sexp_t *sexp = cn_bump_malloc(sizeof(sexp_t));
  assert(sexp);

  sexp->type = SEXP_ATOM;
  sexp->data.atom = cn_intern_string(str);

  return sexp;
}

sexp_t *sexp_list(sexp_t **elements, size_t count) {
  sexp_t *sexp = cn_bump_malloc(sizeof(sexp_t));
  assert(sexp);

  sexp->type = SEXP_LIST;
  sexp->data.list.count = count;
  sexp->data.list.capacity = count;

  if (count > 0) {
    sexp->data.list.elements = cn_bump_malloc(sizeof(sexp_t *) * count);
    assert(sexp->data.list.elements);

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
bennet_optional(sexp_ptr) sexp_to_assert(const sexp_t *sexp) {
  if (!sexp || sexp->type != SEXP_LIST) {
    return bennet_optional_none(sexp_ptr);
  }
  if (sexp->data.list.count != 2) {
    return bennet_optional_none(sexp_ptr);
  }

  sexp_t *first = sexp->data.list.elements[0];
  if (!first || first->type != SEXP_ATOM) {
    return bennet_optional_none(sexp_ptr);
  }
  if (strcmp(first->data.atom, "assert") != 0) {
    return bennet_optional_none(sexp_ptr);
  }

  return bennet_optional_some(sexp_ptr, sexp->data.list.elements[1]);
}

// SMT-LIB construction helpers

/** Apply a function to some arguments. */
sexp_t *sexp_app(sexp_t *f, sexp_t **args, size_t arg_count) {
  assert(f);

  if (arg_count == 0) {
    // If no args, return a copy of f
    if (f->type == SEXP_ATOM) {
      return sexp_atom(f->data.atom);
    } else {
      // For lists, we need to deep copy
      if (f->data.list.count == 0) {
        // Empty list - just create a new empty list
        return sexp_list(NULL, 0);
      }

      sexp_t **new_elements = cn_bump_malloc(sizeof(sexp_t *) * f->data.list.count);
      assert(new_elements);

      for (size_t i = 0; i < f->data.list.count; i++) {
        new_elements[i] = sexp_app(f->data.list.elements[i], NULL, 0);
        assert(new_elements[i]);
      }

      sexp_t *result = sexp_list(new_elements, f->data.list.count);
      return result;
    }
  }

  // Create a list with f as first element, followed by args
  sexp_t **elements = cn_bump_malloc(sizeof(sexp_t *) * (arg_count + 1));
  assert(elements);

  elements[0] = sexp_app(f, NULL, 0);  // Copy f
  assert(elements[0]);

  for (size_t i = 0; i < arg_count; i++) {
    elements[i + 1] = sexp_app(args[i], NULL, 0);  // Deep copy each argument
    assert(elements[i + 1]);
  }

  sexp_t *result = sexp_list(elements, arg_count + 1);
  return result;
}

/** Apply a function to some arguments */
sexp_t *sexp_app_str(const char *f, sexp_t **args, size_t arg_count) {
  sexp_t *f_atom = sexp_atom(f);
  assert(f_atom);

  sexp_t *result = sexp_app(f_atom, args, arg_count);
  return result;
}

/** Type annotation */
sexp_t *sexp_as_type(sexp_t *x, sexp_t *t) {
  assert(x && t);

  sexp_t *args[] = {x, t};
  return sexp_app_str("as", args, 2);
}

/** Let expression */
sexp_t *sexp_let(sexp_t **bindings, size_t binding_count, sexp_t *e) {
  assert(e);

  if (binding_count == 0) {
    // Return a copy of e
    return sexp_app(e, NULL, 0);
  }

  // Create bindings list
  sexp_t *bindings_list = sexp_list(bindings, binding_count);
  assert(bindings_list);

  sexp_t *args[] = {bindings_list, e};
  sexp_t *result = sexp_app_str("let", args, 2);
  return result;
}

/** Non-negative numeric constant. */
sexp_t *sexp_nat_k(int x) {
  char *buffer = malloc(32);
  snprintf(buffer, 32, "%d", x);
  return sexp_atom(buffer);
}

/** Indexed family */
sexp_t *sexp_fam(const char *f, sexp_t **indices, size_t index_count) {
  assert(f);

  // Create list: (_ f indices...)
  sexp_t **elements = cn_bump_malloc(sizeof(sexp_t *) * (index_count + 2));
  assert(elements);

  elements[0] = sexp_atom("_");
  assert(elements[0]);

  elements[1] = sexp_atom(f);
  assert(elements[1]);

  for (size_t i = 0; i < index_count; i++) {
    elements[i + 2] = indices[i];
  }

  sexp_t *result = sexp_list(elements, index_count + 2);
  return result;
}

/** Int-indexed family */
sexp_t *sexp_ifam(const char *f, int *indices, size_t index_count) {
  assert(f);

  // Convert int indices to sexp atoms
  sexp_t **sexp_indices = cn_bump_malloc(sizeof(sexp_t *) * index_count);
  assert(sexp_indices);

  for (size_t i = 0; i < index_count; i++) {
    sexp_indices[i] = sexp_nat_k(indices[i]);
    assert(sexp_indices[i]);
  }

  sexp_t *result = sexp_fam(f, sexp_indices, index_count);

  // Free the temporary sexp indices
  for (size_t i = 0; i < index_count; i++) {
  }

  return result;
}

/** Attribute */
sexp_t *sexp_named(const char *name, sexp_t *e) {
  assert(name && e);

  sexp_t *name_atom = sexp_atom(name);
  assert(name_atom);

  sexp_t *named_atom = sexp_atom(":named");
  assert(named_atom);

  sexp_t *args[] = {e, named_atom, name_atom};
  sexp_t *result = sexp_app_str("!", args, 3);

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
const char *quote_symbol(const char *s) {
  assert(s);

  if (is_simple_symbol(s)) {
    // Return the original string
    return s;
  } else {
    // Return quoted version: |s|
    size_t len = strlen(s);
    char *temp = cn_bump_malloc(len + 3);  // +2 for pipes, +1 for null terminator
    assert(temp);

    temp[0] = '|';
    strcpy(temp + 1, s);
    temp[len + 1] = '|';
    temp[len + 2] = '\0';

    // Intern the quoted symbol
    const char *result = cn_intern_string(temp);
    // Note: temp is allocated with cn_bump_malloc, so we don't free it
    // (bump allocator doesn't support individual frees)
    return result;
  }
}

/** Make an SMT name, quoting if needed. Note that even with quoting
    the name should not contain pipe (|) or backslash (\) */
sexp_t *symbol(const char *x) {
  assert(x);

  const char *quoted = quote_symbol(x);
  assert(quoted);

  sexp_t *result = sexp_atom(quoted);
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
  assert(x && y && z);

  sexp_t *args[] = {x, y, z};
  return sexp_app_str("ite", args, 3);
}

/** Arguments are equal. */
sexp_t *eq(sexp_t *x, sexp_t *y) {
  assert(x && y);

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
  assert(p);

  sexp_t *args[] = {p};
  return sexp_app_str("not", args, 1);
}

/** Conjunction. */
sexp_t *bool_and(sexp_t *p, sexp_t *q) {
  assert(p && q);

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
  assert(p && q);

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
  assert(p && q);

  sexp_t *args[] = {p, q};
  return sexp_app_str("xor", args, 2);
}

/** Implication. */
sexp_t *bool_implies(sexp_t *p, sexp_t *q) {
  assert(p && q);

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
  assert(x);

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
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("/", args, 2);
}

/** Greater-then for numbers. */
sexp_t *num_gt(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str(">", args, 2);
}

/** Less-then for numbers. */
sexp_t *num_lt(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("<", args, 2);
}

/** Greater-than-or-equal-to for numbers. */
sexp_t *num_geq(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str(">=", args, 2);
}

/** Less-than-or-equal-to for numbers. */
sexp_t *num_leq(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("<=", args, 2);
}

/** Numeric addition. */
sexp_t *num_add(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("+", args, 2);
}

/** Numeric subtraction. */
sexp_t *num_sub(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("-", args, 2);
}

/** Numeric multiplication. */
sexp_t *num_mul(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("*", args, 2);
}

/** Numeric absolute value. */
sexp_t *num_abs(sexp_t *x) {
  assert(x);

  sexp_t *args[] = {x};
  return sexp_app_str("abs", args, 1);
}

/** Numeric division. */
sexp_t *num_div(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("div", args, 2);
}

/** Numeric modulus. */
sexp_t *num_mod(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("mod", args, 2);
}

/** Numeric reminder. Nonstandard. */
sexp_t *num_rem(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("rem", args, 2);
}

/** Is the number divisible by the given constant? */
sexp_t *num_divisible(sexp_t *x, int n) {
  assert(x);

  int indices[] = {n};
  sexp_t *divisible_indexed = sexp_ifam("divisible", indices, 1);
  assert(divisible_indexed);

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(divisible_indexed, args, 1);
  return result;
}

/** Satisfies [real_to_int x <= x] (i.e., this is like [floor]) */
sexp_t *real_to_int(sexp_t *e) {
  assert(e);

  sexp_t *args[] = {e};
  return sexp_app_str("to_int", args, 1);
}

/** Promote an integer to a real. */
sexp_t *int_to_real(sexp_t *e) {
  assert(e);

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
    - The number should not exceed the number of bits. */
sexp_t *bv_nat_bin(int w, unsigned long long v) {
  // Create binary string with proper width
  char *binary_str = cn_bump_malloc(w + 1);
  assert(binary_str);

  // Convert to binary
  for (int i = w - 1; i >= 0; i--) {
    binary_str[w - 1 - i] = ((v >> i) & 1) ? '1' : '0';
  }
  binary_str[w] = '\0';

  // Create #b prefix + binary string
  size_t total_len = 2 + w + 1;  // #b + binary + null
  char *result_str = cn_bump_malloc(total_len);
  assert(result_str);

  snprintf(result_str, total_len, "#b%s", binary_str);
  sexp_t *result = sexp_atom(result_str);
  return result;
}

/** A bit-vector represented in hex.
    - The number should not exceed the number of bits.
    - The width should be a multiple of 4. */
sexp_t *bv_nat_hex(int w, unsigned long long v) {
  assert(w % 4 == 0);

  int hex_digits = w / 4;
  char *format_str = cn_bump_malloc(32);
  assert(format_str);

  snprintf(format_str, 32, "#x%%0%dllx", hex_digits);

  char *result_str = cn_bump_malloc(hex_digits + 3);  // #x + digits + null
  assert(result_str);

  snprintf(result_str, hex_digits + 3, format_str, v);

  sexp_t *result = sexp_atom(result_str);
  return result;
}

/** Bit vector arithmetic negation. */
sexp_t *bv_neg(sexp_t *x) {
  assert(x);

  sexp_t *args[] = {x};
  return sexp_app_str("bvneg", args, 1);
}

/** Bit vector bitwise complement. */
sexp_t *bv_compl(sexp_t *x) {
  assert(x);

  sexp_t *args[] = {x};
  return sexp_app_str("bvnot", args, 1);
}

/** A bit-vector represented in binary.
    The number should fit in the given number of bits. */
sexp_t *bv_bin(int w, long long v) {
  if (v >= 0) {
    return bv_nat_bin(w, (unsigned long long)v);
  } else {
    // Check if v is the minimum signed value for width w
    // For minimum signed value: v == -(2^(w-1))
    // Negating this value causes overflow, so convert to unsigned bit pattern
    if ((w > 0 && w <= 63 && v == -(1LL << (w - 1))) || (w == 64 && v == LLONG_MIN)) {
      // Use unsigned bit pattern: 2^(w-1) = 0b100...0
      unsigned long long unsigned_val = (w == 64) ? (1ULL << 63) : (1ULL << (w - 1));
      return bv_nat_bin(w, unsigned_val);
    } else {
      // Regular negative number: convert -v to unsigned
      sexp_t *pos_bv = bv_nat_bin(w, (unsigned long long)(-v));
      sexp_t *result = bv_neg(pos_bv);
      return result;
    }
  }
}

/** A bit-vector represented in hex.
    - The number should fit in the given number of bits.
    - The width should be a multiple of 4. */
sexp_t *bv_hex(int w, long long v) {
  if (v >= 0) {
    return bv_nat_hex(w, (unsigned long long)v);
  } else {
    // Check if v is the minimum signed value for width w
    // For minimum signed value: v == -(2^(w-1))
    // Negating this value causes overflow, so convert to unsigned bit pattern
    if ((w > 0 && w <= 63 && v == -(1LL << (w - 1))) || (w == 64 && v == LLONG_MIN)) {
      // Use unsigned bit pattern: 2^(w-1) = 0x800...0
      unsigned long long unsigned_val = (w == 64) ? (1ULL << 63) : (1ULL << (w - 1));
      return bv_nat_hex(w, unsigned_val);
    } else {
      // Regular negative number: convert -v to unsigned
      sexp_t *pos_bv = bv_nat_hex(w, (unsigned long long)(-v));
      sexp_t *result = bv_neg(pos_bv);
      return result;
    }
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

/** Unsigned less-than on bit-vectors. */
sexp_t *bv_ult(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvult", args, 2);
}

/** Unsigned less-than-or-equal on bit-vectors. */
sexp_t *bv_uleq(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvule", args, 2);
}

/** Signed less-than on bit-vectors. */
sexp_t *bv_slt(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvslt", args, 2);
}

/** Signed less-than-or-equal on bit-vectors. */
sexp_t *bv_sleq(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsle", args, 2);
}

/** Bit vector concatenation. */
sexp_t *bv_concat(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("concat", args, 2);
}

/** Extend to the signed equivalent bitvector by the given number of bits. */
sexp_t *bv_sign_extend(int i, sexp_t *x) {
  assert(x);

  int indices[] = {i};
  sexp_t *sign_extend_indexed = sexp_ifam("sign_extend", indices, 1);
  assert(sign_extend_indexed);

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(sign_extend_indexed, args, 1);
  return result;
}

/** Zero extend by the given number of bits. */
sexp_t *bv_zero_extend(int i, sexp_t *x) {
  assert(x);

  int indices[] = {i};
  sexp_t *zero_extend_indexed = sexp_ifam("zero_extend", indices, 1);
  assert(zero_extend_indexed);

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(zero_extend_indexed, args, 1);
  return result;
}

/** [bv_extract last_ix first_ix x] is a sub-vector of [x].
    [last_ix] is the larger bit index, [first_ix] is the smaller one, and indexing
    is inclusive. */
sexp_t *bv_extract(int last_ix, int first_ix, sexp_t *x) {
  assert(x);

  int indices[] = {last_ix, first_ix};
  sexp_t *extract_indexed = sexp_ifam("extract", indices, 2);
  assert(extract_indexed);

  sexp_t *args[] = {x};
  sexp_t *result = sexp_app(extract_indexed, args, 1);
  return result;
}

/** Bitwise negation. */
sexp_t *bv_not(sexp_t *x) {
  assert(x);

  sexp_t *args[] = {x};
  return sexp_app_str("bvnot", args, 1);
}

/** Bitwise conjunction. */
sexp_t *bv_and(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvand", args, 2);
}

/** Bitwise disjunction. */
sexp_t *bv_or(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvor", args, 2);
}

/** Bitwise exclusive or. */
sexp_t *bv_xor(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvxor", args, 2);
}

/** Addition of bit vectors. */
sexp_t *bv_add(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvadd", args, 2);
}

/** Subtraction of bit vectors. */
sexp_t *bv_sub(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsub", args, 2);
}

/** Multiplication of bit vectors. */
sexp_t *bv_mul(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvmul", args, 2);
}

/** Bit vector unsigned division. */
sexp_t *bv_udiv(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvudiv", args, 2);
}

/** Bit vector unsigned remainder. */
sexp_t *bv_urem(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvurem", args, 2);
}

/** Bit vector signed division. */
sexp_t *bv_sdiv(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsdiv", args, 2);
}

/** Bit vector signed remainder. */
sexp_t *bv_srem(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsrem", args, 2);
}

/** Bit vector signed modulus. Nonstandard? */
sexp_t *bv_smod(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvsmod", args, 2);
}

/** Shift left. */
sexp_t *bv_shl(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvshl", args, 2);
}

/** Logical shift right. */
sexp_t *bv_lshr(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvlshr", args, 2);
}

/** Arithmetic shift right (copies most significant bit). */
sexp_t *bv_ashr(sexp_t *x, sexp_t *y) {
  assert(x && y);

  sexp_t *args[] = {x, y};
  return sexp_app_str("bvashr", args, 2);
}

//////////////
/* Pointers */
//////////////

sexp_t *t_loc(void) {
  return t_bits(CHAR_BIT * sizeof(uintptr_t));
}

sexp_t *loc_k(uintptr_t ptr) {
  return bv_k(CHAR_BIT * sizeof(uintptr_t), ptr);
}

////////////
/* Arrays */
////////////

/** The type of arrays with keys kt and values vt */
sexp_t *t_array(sexp_t *kt, sexp_t *vt) {
  assert(kt && vt);

  sexp_t *args[] = {kt, vt};
  return sexp_app_str("Array", args, 2);
}

/** An array of type Array kt vt where all elements are v.
    v should be of type vt. */
sexp_t *arr_const(sexp_t *kt, sexp_t *vt, sexp_t *v) {
  assert(kt && vt && v);

  sexp_t *const_atom = sexp_atom("const");
  assert(const_atom);

  sexp_t *array_type = t_array(kt, vt);
  assert(array_type);

  sexp_t *typed_const = sexp_as_type(const_atom, array_type);
  assert(typed_const);

  sexp_t *args[] = {v};
  sexp_t *result = sexp_app(typed_const, args, 1);
  return result;
}

/** The element stored at index i of arr. */
sexp_t *arr_select(sexp_t *arr, sexp_t *i) {
  assert(arr && i);

  sexp_t *args[] = {arr, i};
  return sexp_app_str("select", args, 2);
}

/** An array that is the same as arr, except at index i it has v */
sexp_t *arr_store(sexp_t *arr, sexp_t *i, sexp_t *v) {
  assert(arr && i && v);

  sexp_t *args[] = {arr, i, v};
  return sexp_app_str("store", args, 3);
}

//////////
/* Sets */
//////////

/** The type of sets with elements of type t */
sexp_t *t_set(sexp_t *x) {
  assert(x);

  sexp_t *args[] = {x};
  return sexp_app_str("Set", args, 1);
}

/** The empty set with elements of type t */
sexp_t *set_empty(solver_extensions_t ext, sexp_t *t) {
  assert(t);

  if (ext == SOLVER_CVC5) {
    sexp_t *empty_atom = sexp_atom("set.empty");
    assert(empty_atom);

    sexp_t *set_type = t_set(t);
    assert(set_type);

    sexp_t *args[] = {empty_atom, set_type};
    sexp_t *result = sexp_app_str("as", args, 2);
    return result;
  } else {
    sexp_t *const_atom = sexp_atom("const");
    assert(const_atom);

    sexp_t *set_type = t_set(t);
    assert(set_type);

    sexp_t *const_args[] = {const_atom, set_type};
    sexp_t *typed_const = sexp_app_str("as", const_args, 2);
    assert(typed_const);

    sexp_t *false_val = bool_k(false);
    assert(false_val);

    sexp_t *app_args[] = {false_val};
    sexp_t *result = sexp_app(typed_const, app_args, 1);
    return result;
  }
}

/** The universal set with elements of type t */
sexp_t *set_universe(solver_extensions_t ext, sexp_t *t) {
  assert(t);

  if (ext == SOLVER_CVC5) {
    sexp_t *universe_atom = sexp_atom("set.universe");
    assert(universe_atom);

    sexp_t *set_type = t_set(t);
    assert(set_type);

    sexp_t *args[] = {universe_atom, set_type};
    sexp_t *result = sexp_app_str("as", args, 2);
    return result;
  } else {
    sexp_t *const_atom = sexp_atom("const");
    assert(const_atom);

    sexp_t *set_type = t_set(t);
    assert(set_type);

    sexp_t *const_args[] = {const_atom, set_type};
    sexp_t *typed_const = sexp_app_str("as", const_args, 2);
    assert(typed_const);

    sexp_t *true_val = bool_k(true);
    assert(true_val);

    sexp_t *app_args[] = {true_val};
    sexp_t *result = sexp_app(typed_const, app_args, 1);
    return result;
  }
}

/** A set that has all elements of xs and also x */
sexp_t *set_insert(solver_extensions_t ext, sexp_t *x, sexp_t *xs) {
  assert(x && xs);

  if (ext == SOLVER_CVC5) {
    sexp_t *args[] = {x, xs};
    return sexp_app_str("set.insert", args, 2);
  } else {
    sexp_t *true_val = bool_k(true);
    assert(true_val);

    sexp_t *result = arr_store(xs, x, true_val);
    return result;
  }
}

/** A set that has all elements from x and also from y */
sexp_t *set_union(solver_extensions_t ext, sexp_t *x, sexp_t *y) {
  assert(x && y);

  const char *nm = (ext == SOLVER_CVC5) ? "set.union" : "union";
  sexp_t *args[] = {x, y};
  return sexp_app_str(nm, args, 2);
}

/** A set that has the elements that are in both x and y */
sexp_t *set_intersection(solver_extensions_t ext, sexp_t *x, sexp_t *y) {
  assert(x && y);

  const char *nm = (ext == SOLVER_CVC5) ? "set.inter" : "intersection";
  sexp_t *args[] = {x, y};
  return sexp_app_str(nm, args, 2);
}

/** A set that has the elements of x that are not in y */
sexp_t *set_difference(solver_extensions_t ext, sexp_t *x, sexp_t *y) {
  assert(x && y);

  const char *nm = (ext == SOLVER_CVC5) ? "set.minus" : "setminus";
  sexp_t *args[] = {x, y};
  return sexp_app_str(nm, args, 2);
}

/** A set that has all elements that are not in x */
sexp_t *set_complement(solver_extensions_t ext, sexp_t *x) {
  assert(x);

  const char *nm = (ext == SOLVER_CVC5) ? "set.complement" : "complement";
  sexp_t *args[] = {x};
  return sexp_app_str(nm, args, 1);
}

/** True if x is a member of xs */
sexp_t *set_member(solver_extensions_t ext, sexp_t *x, sexp_t *xs) {
  assert(x && xs);

  if (ext == SOLVER_CVC5) {
    sexp_t *args[] = {x, xs};
    return sexp_app_str("set.member", args, 2);
  } else {
    return arr_select(xs, x);
  }
}

/** True if all elements of xs are also in ys */
sexp_t *set_subset(solver_extensions_t ext, sexp_t *xs, sexp_t *ys) {
  assert(xs && ys);

  const char *nm = (ext == SOLVER_CVC5) ? "set.subset" : "subset";
  sexp_t *args[] = {xs, ys};
  return sexp_app_str(nm, args, 2);
}

/////////////////
/* Quantifiers */
/////////////////

/** Universal quantification over the given variable bindings. */
sexp_t *forall(sexp_t **bindings, size_t binding_count, sexp_t *p) {
  assert(p);

  if (binding_count == 0) {
    // Return a copy of p if no bindings
    return sexp_app(p, NULL, 0);
  }

  // Create bindings list
  sexp_t *bindings_list = sexp_list(bindings, binding_count);
  assert(bindings_list);

  sexp_t *args[] = {bindings_list, p};
  sexp_t *result = sexp_app_str("forall", args, 2);
  return result;
}

////////////
/* Commands */
////////////

/** A command made out of just atoms. */
sexp_t *simple_command(const char **strs, size_t count) {
  assert(strs);

  sexp_t **atoms = cn_bump_malloc(sizeof(sexp_t *) * count);
  assert(atoms);

  for (size_t i = 0; i < count; i++) {
    atoms[i] = sexp_atom(strs[i]);
    assert(atoms[i]);
  }

  sexp_t *result = sexp_list(atoms, count);
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
sexp_t *sexp_push(int n) {
  char n_str[32];
  snprintf(n_str, sizeof(n_str), "%d", n);
  const char *strs[] = {"push", n_str};
  return simple_command(strs, 2);
}

/** Pop a scope. */
sexp_t *sexp_pop(int n) {
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
  assert(name_atom);
  sexp_t *arity_atom = sexp_atom(arity_str);
  assert(arity_atom);

  sexp_t *args[] = {name_atom, arity_atom};
  sexp_t *result = sexp_app_str("declare-sort", args, 2);
  return result;
}

/** Declares a function with given parameter types and result type. */
sexp_t *declare_fun(
    const char *name, sexp_t **param_types, size_t param_count, sexp_t *result_type) {
  assert(name && result_type);

  sexp_t *name_atom = sexp_atom(name);
  assert(name_atom);

  sexp_t *params_list = sexp_list(param_types, param_count);
  assert(params_list);

  sexp_t *args[] = {name_atom, params_list, result_type};
  sexp_t *result = sexp_app_str("declare-fun", args, 3);
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
    bool recursive,
    sexp_t *definition) {
  assert(name && result_type && definition);

  sexp_t *name_atom = sexp_atom(name);
  assert(name_atom);

  sexp_t *params_list = sexp_list(params, param_count);
  assert(params_list);

  const char *command = recursive ? "define-fun-rec" : "define-fun";
  sexp_t *args[] = {name_atom, params_list, result_type, definition};
  sexp_t *result = sexp_app_str(command, args, 4);
  return result;
}

/** Defines a constant of given type and definition. */
sexp_t *define_const(const char *name, sexp_t *type, sexp_t *definition) {
  return define_fun(name, NULL, 0, type, false, definition);
}

/** Constructor field and constructor types are defined in the header */

/** Declares a single datatype. */
sexp_t *declare_datatype(const char *name,
    const char **type_params,
    size_t type_param_count,
    constructor_t *constructors,
    size_t constructor_count) {
  assert(name && constructors);

  sexp_t *name_atom = sexp_atom(name);
  assert(name_atom);

  // Build constructors list
  sexp_t **cons_exprs = cn_bump_malloc(sizeof(sexp_t *) * constructor_count);
  assert(cons_exprs);

  for (size_t i = 0; i < constructor_count; i++) {
    // Build field list for this constructor
    sexp_t **field_exprs =
        cn_bump_malloc(sizeof(sexp_t *) * (constructors[i].field_count + 1));
    assert(field_exprs);

    // Constructor name
    field_exprs[0] = sexp_atom(constructors[i].name);
    assert(field_exprs[0]);

    // Fields
    for (size_t f = 0; f < constructors[i].field_count; f++) {
      sexp_t *field_name = sexp_atom(constructors[i].fields[f].name);
      assert(field_name);

      sexp_t *field_args[] = {field_name, constructors[i].fields[f].type};
      field_exprs[f + 1] = sexp_list(field_args, 2);
      assert(field_exprs[f + 1]);
    }

    cons_exprs[i] = sexp_list(field_exprs, constructors[i].field_count + 1);
    assert(cons_exprs[i]);
  }

  sexp_t *cons_list = sexp_list(cons_exprs, constructor_count);
  assert(cons_list);

  sexp_t *def;
  if (type_param_count == 0) {
    def = cons_list;
  } else {
    // Build type parameters list
    sexp_t **type_param_atoms = cn_bump_malloc(sizeof(sexp_t *) * type_param_count);
    assert(type_param_atoms);

    for (size_t i = 0; i < type_param_count; i++) {
      type_param_atoms[i] = sexp_atom(type_params[i]);
      assert(type_param_atoms[i]);
    }

    sexp_t *type_params_list = sexp_list(type_param_atoms, type_param_count);
    assert(type_params_list);

    sexp_t *par_args[] = {type_params_list, cons_list};
    def = sexp_app_str("par", par_args, 2);
    assert(def);
  }

  sexp_t *args[] = {name_atom, def};
  sexp_t *result = sexp_app_str("declare-datatype", args, 2);
  if (type_param_count > 0) {
  }
  return result;
}

/** [declare_datatypes tys] defines a group of mutually recursive ADTs.
    Each element of `tys` is (name,type params,cons). */
sexp_t *declare_datatypes(datatype_def_t *datatypes, size_t datatype_count) {
  assert(datatypes && datatype_count > 0);

  // Create arity list: [(name arity), ...]
  sexp_t **arity_list = cn_bump_malloc(sizeof(sexp_t *) * datatype_count);
  assert(arity_list);

  for (size_t i = 0; i < datatype_count; i++) {
    datatype_def_t *dt = &datatypes[i];

    // Create (name arity) pair
    sexp_t *name_atom = sexp_atom(dt->name);
    assert(name_atom);

    char arity_str[32];
    snprintf(arity_str, sizeof(arity_str), "%zu", dt->type_param_count);
    sexp_t *arity_atom = sexp_atom(arity_str);
    assert(arity_atom);

    sexp_t *pair_elements[] = {name_atom, arity_atom};
    arity_list[i] = sexp_list(pair_elements, 2);
    assert(arity_list[i]);
  }

  // Create definition list
  sexp_t **def_list = cn_bump_malloc(sizeof(sexp_t *) * datatype_count);
  assert(def_list);

  for (size_t i = 0; i < datatype_count; i++) {
    datatype_def_t *dt = &datatypes[i];

    // mk_field: create field from (name, type) pair
    // mk_con: create constructor from (name, fields) pair
    // mk_cons: create list of constructors

    // Build constructor list
    sexp_t **cons_exprs = cn_bump_malloc(sizeof(sexp_t *) * dt->constructor_count);
    assert(cons_exprs);

    for (size_t c = 0; c < dt->constructor_count; c++) {
      constructor_t *con = &dt->constructors[c];

      // Create elements: [con_name, field1, field2, ...]
      sexp_t **con_elements = cn_bump_malloc(sizeof(sexp_t *) * (1 + con->field_count));
      assert(con_elements);

      // Constructor name
      con_elements[0] = sexp_atom(con->name);
      assert(con_elements[0]);

      // Fields: each field is (field_name field_type)
      for (size_t f = 0; f < con->field_count; f++) {
        sexp_t *field_name = sexp_atom(con->fields[f].name);
        assert(field_name);

        sexp_t *field_args[] = {field_name, con->fields[f].type};
        con_elements[f + 1] = sexp_list(field_args, 2);
        assert(con_elements[f + 1]);
      }

      cons_exprs[c] = sexp_list(con_elements, con->field_count + 1);
      assert(cons_exprs[c]);
    }

    sexp_t *cons_list = sexp_list(cons_exprs, dt->constructor_count);
    assert(cons_list);

    // def: handle type parameters
    if (dt->type_param_count == 0) {
      def_list[i] = cons_list;
    } else {
      // Build type parameters list
      sexp_t **type_param_atoms = cn_bump_malloc(sizeof(sexp_t *) * dt->type_param_count);
      assert(type_param_atoms);

      for (size_t p = 0; p < dt->type_param_count; p++) {
        type_param_atoms[p] = sexp_atom(dt->type_params[p]);
        assert(type_param_atoms[p]);
      }

      sexp_t *type_params_list = sexp_list(type_param_atoms, dt->type_param_count);
      assert(type_params_list);

      sexp_t *par_args[] = {type_params_list, cons_list};
      def_list[i] = sexp_app_str("par", par_args, 2);
      assert(def_list[i]);
    }
  }

  // Create the final command
  sexp_t *arity_sexp = sexp_list(arity_list, datatype_count);
  assert(arity_sexp);

  sexp_t *def_sexp = sexp_list(def_list, datatype_count);
  assert(def_sexp);

  sexp_t *args[] = {arity_sexp, def_sexp};
  sexp_t *result = sexp_app_str("declare-datatypes", args, 2);
  assert(result);

  return result;
}

/** Pattern and match alternative types are defined in the header */

/** Match datatype expression. */
sexp_t *match_datatype(sexp_t *expr, match_alt_t *alts, size_t alt_count) {
  assert(expr && alts);

  sexp_t **alt_exprs = cn_bump_malloc(sizeof(sexp_t *) * alt_count);
  assert(alt_exprs);

  for (size_t i = 0; i < alt_count; i++) {
    sexp_t *pat_expr;

    if (alts[i].pattern.type == PAT_VAR) {
      pat_expr = sexp_atom(alts[i].pattern.data.var_name);
    } else {
      // PAT_CON
      sexp_t **con_elements =
          cn_bump_malloc(sizeof(sexp_t *) * (alts[i].pattern.data.con.var_count + 1));
      assert(con_elements);

      con_elements[0] = sexp_atom(alts[i].pattern.data.con.con_name);
      assert(con_elements[0]);

      for (size_t v = 0; v < alts[i].pattern.data.con.var_count; v++) {
        con_elements[v + 1] = sexp_atom(alts[i].pattern.data.con.var_names[v]);
        assert(con_elements[v + 1]);
      }

      pat_expr = sexp_list(con_elements, alts[i].pattern.data.con.var_count + 1);
    }

    assert(pat_expr);

    sexp_t *alt_args[] = {pat_expr, alts[i].expr};
    alt_exprs[i] = sexp_list(alt_args, 2);
    assert(alt_exprs[i]);
  }

  sexp_t *alts_list = sexp_list(alt_exprs, alt_count);
  assert(alts_list);

  sexp_t *args[] = {expr, alts_list};
  sexp_t *result = sexp_app_str("match", args, 2);
  return result;
}

/** Test if expression is constructed with given constructor. */
sexp_t *is_con(const char *constructor, sexp_t *expr) {
  assert(constructor && expr);

  sexp_t *con_atom = sexp_atom(constructor);
  assert(con_atom);

  sexp_t *indices[] = {con_atom};
  sexp_t *is_indexed = sexp_fam("is", indices, 1);
  assert(is_indexed);

  sexp_t *args[] = {expr};
  sexp_t *result = sexp_app(is_indexed, args, 1);
  return result;
}

/** Add an assertion to the current scope. */
sexp_t *assume(sexp_t *expr) {
  assert(expr);

  sexp_t *args[] = {expr};
  return sexp_app_str("assert", args, 1);
}

/** Add an assertion to the current scope. */
sexp_t *assume_soft(sexp_t *expr) {
  assert(expr);

  sexp_t *args[] = {expr};
  return sexp_app_str("assert-soft", args, 1);
}
