#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/hash_table.h>
#include <cn-replicate/lines.h>
#include <cn-replicate/shape.h>

/*
 * First Pass
 *
 * Builds a table storing how many bytes should
 * be allocated for each pointer.
 */

#define MEM_SIZE (1024 * 1024 * 256)
static struct hash_table* alloc_sizes;

void* cn_replica_alloc_get_parent(void* ptr) {
  int64_t* key = malloc(sizeof(int64_t));
  assert(key);
  *key = (int64_t)ptr;

  size_t* old_sz = ht_get(alloc_sizes, key);
  free(key);
  if (old_sz != NULL) {
    return ptr;
  }

  uintptr_t p = (uintptr_t)ptr;

  hash_table_iterator it = ht_iterator(alloc_sizes);
  while (ht_next(&it)) {
    uintptr_t lb = (uintptr_t)*it.key;       // Inclusive
    uintptr_t ub = lb + *(size_t*)it.value;  // Exclusive

    if (lb <= p && p <= ub) {
      return (void*)lb;
    }
  }

  return ptr;
}

size_t cn_replica_alloc_get(void* ptr) {
  void* parent = cn_replica_alloc_get_parent(ptr);
  uintptr_t dist = (uintptr_t)ptr - (uintptr_t)parent;

  int64_t* key = malloc(sizeof(int64_t));
  assert(key);
  *key = (int64_t)parent;

  size_t* value = ht_get(alloc_sizes, key);
  free(key);

  if (value == NULL) {
    return -1;
  }

  return *value - dist;
}

void cn_analyze_shape_owned(void* ptr, size_t sz) {
  void* parent = cn_replica_alloc_get_parent(ptr);
  uintptr_t dist = (uintptr_t)ptr - (uintptr_t)parent;
  sz += dist;

  int64_t* key = malloc(sizeof(int64_t));
  assert(key);
  *key = (int64_t)parent;

  size_t* old_sz = ht_get(alloc_sizes, key);
  if (old_sz == NULL) {
    size_t* new_sz = malloc(sizeof(size_t));
    assert(new_sz);
    *new_sz = sz;

    ht_set(alloc_sizes, key, new_sz);
  } else if (*old_sz < sz) {
    *old_sz = sz;
  }

  free(key);
}

/*
 * Second Pass
 *
 * If pointer hasn't been seen before, allocate
 * the number of bytes from the first pass, if it exists.
 * 
 * If it was not seen in the first pass, just set it to
 * the constant.
 * 
 * Set the pointer to the stored name referring to the
 * allocation from the first pass.
 */

static struct hash_table* allocated = NULL;

static int pointer_count = 0;

static const char* cn_replicate_get(void* p) {
  p = cn_replica_alloc_get_parent(p);

  int64_t* key = malloc(sizeof(int64_t));
  assert(key);
  *key = (int64_t)p;

  char* name = ht_get(allocated, key);
  if (name != NULL) {
    free(key);

    return name;
  }

  name = malloc(22);
  assert(name);
  snprintf(name, 22, "p%d", pointer_count++);

  ht_set(allocated, key, name);
  free(key);

  size_t sz = cn_replica_alloc_get(p);
  assert(sz != -1);

  size_t bytes = 6 + strlen(name) + 10 + 20 + 3;
  char* buf = malloc(bytes);
  assert(buf);
  snprintf(buf, bytes, "void* %s = malloc(%" PRIuPTR ");", name, sz);

  cn_replica_lines_append(buf);

  return name;
}

void cn_replicate_owned(char* addr_str, char* value_str) {
  size_t bytes = strlen(addr_str) + 3 + strlen(value_str) + 3;
  char* buf = malloc(bytes);
  assert(buf);
  snprintf(buf, bytes, "%s = %s;", addr_str, value_str);

  cn_replica_lines_append(buf);
}

char* cn_replicate_owned_cn_pointer_aux(cn_pointer* q) {
  void* p = convert_from_cn_pointer(q);

  if (p == NULL) {
    char* buf = malloc(5);
    assert(buf);
    snprintf(buf, 5, "NULL");
    return buf;
  }

  size_t sz = cn_replica_alloc_get(p);
  if (sz == -1) {
    char* buf = malloc(24);
    assert(buf);
    snprintf(buf, 24, "%" PRIxPTR, (uintptr_t)p);
    return buf;
  }

  void* parent = cn_replica_alloc_get_parent(p);
  uintptr_t dist = (uintptr_t)p - (uintptr_t)parent;

  const char* name = cn_replicate_get(p);
  assert(name != NULL);

  if (dist == 0) {
    size_t bytes = 11 + strlen(name) + 1;
    char* buf = malloc(bytes);
    assert(buf);
    snprintf(buf, bytes, "%s", name);
    return buf;
  }

  size_t bytes = 12 + strlen(name) + 3 + 24 + 2;
  char* buf = malloc(bytes);
  assert(buf);
  snprintf(buf, bytes, "((uintptr_t)%s + %" PRIxPTR ")", name, dist);
  return buf;
}

static int decimal_places = 0;

static void init_decimal_places(void) {
  if (!decimal_places) {
    uintmax_t count = UINTMAX_MAX;

    int log = 1;
    while (count > 0) {
      count /= 10;
      log++;
    }
    decimal_places = log;
  }
}

#define CN_REPLICATE_BITS(ty)                                                            \
  char* cn_replicate_owned_cn_bits_##ty##_aux(cn_pointer* p) {                           \
    init_decimal_places();                                                               \
                                                                                         \
    size_t bytes = decimal_places + 1;                                                   \
    char* buf = malloc(bytes);                                                           \
    assert(buf);                                                                         \
    snprintf(buf,                                                                        \
        bytes,                                                                           \
        "%" PRI##ty,                                                                     \
        convert_from_cn_bits_##ty((cn_bits_##ty*)convert_from_cn_pointer(p)));           \
    return buf;                                                                          \
  }

CN_REPLICATE_BITS(i8);
CN_REPLICATE_BITS(i16);
CN_REPLICATE_BITS(i32);
CN_REPLICATE_BITS(i64);

CN_REPLICATE_BITS(u8);
CN_REPLICATE_BITS(u16);
CN_REPLICATE_BITS(u32);
CN_REPLICATE_BITS(u64);

// Other //

void cn_replica_alloc_reset(void) {
  // First pass
  alloc_sizes = ht_create(&fulm_default_alloc);

  // Second pass
  allocated = ht_create(&fulm_default_alloc);
}
