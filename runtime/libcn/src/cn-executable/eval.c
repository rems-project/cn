#include <stdlib.h>

#include <cn-executable/eval.h>

cn_bool* convert_to_cn_bool(_Bool b) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  if (!res)
    exit(1);
  res->val = b;
  return res;
}

_Bool convert_from_cn_bool(cn_bool* b) {
  return b->val;
}

cn_bool* cn_bool_and(cn_bool* b1, cn_bool* b2) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = b1->val && b2->val;
  return res;
}

cn_bool* cn_bool_or(cn_bool* b1, cn_bool* b2) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = b1->val || b2->val;
  return res;
}

cn_bool* cn_bool_implies(cn_bool* b1, cn_bool* b2) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = !b1->val || b2->val;
  return res;
}

cn_bool* cn_bool_not(cn_bool* b) {
  cn_bool* res = cn_bump_malloc(sizeof(cn_bool));
  res->val = !(b->val);
  return res;
}

cn_bool* cn_bool_equality(cn_bool* b1, cn_bool* b2) {
  return convert_to_cn_bool(b1->val == b2->val);
}

void* cn_ite(cn_bool* b, void* e1, void* e2) {
  return b->val ? e1 : e2;
}

cn_map* map_create(void) {
  static allocator bump_alloc = (allocator){
      .malloc = &cn_bump_malloc, .calloc = &cn_bump_calloc, .free = &cn_bump_free};

  return ht_create(&bump_alloc);
}

cn_map* cn_map_set(cn_map* m, cn_integer* key, void* value) {
  ht_set(m, &key->val, value);
  return m;
}

cn_map* cn_map_deep_copy(cn_map* m1) {
  cn_map* m2 = map_create();

  hash_table_iterator hti = ht_iterator(m1);

  while (ht_next(&hti)) {
    int64_t* curr_key = hti.key;
    void* val = ht_get(m1, curr_key);
    ht_set(m2, curr_key, val);
  }

  return m2;
}

cn_map* default_cn_map(void) {
  return map_create();
}

cn_bool* default_cn_bool(void) {
  return convert_to_cn_bool(false);
}

cn_bool* cn_pointer_equality(void* i1, void* i2) {
  return convert_to_cn_bool((((cn_pointer*)i1)->ptr) == (((cn_pointer*)i2)->ptr));
}

cn_bool* cn_pointer_is_null(cn_pointer* p) {
  return convert_to_cn_bool(p->ptr == NULL);
}

cn_bool* cn_pointer_lt(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr < p2->ptr);
}

cn_bool* cn_pointer_le(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr <= p2->ptr);
}

cn_bool* cn_pointer_gt(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr > p2->ptr);
}

cn_bool* cn_pointer_ge(cn_pointer* p1, cn_pointer* p2) {
  return convert_to_cn_bool(p1->ptr >= p2->ptr);
}

cn_pointer* cast_cn_pointer_to_cn_pointer(cn_pointer* p) {
  return p;
}

// Check if m2 is a subset of m1
cn_bool* cn_map_subset(
    cn_map* m1, cn_map* m2, cn_bool*(value_equality_fun)(void*, void*)) {
  if (ht_size(m1) != ht_size(m2))
    return convert_to_cn_bool(0);

  hash_table_iterator hti1 = ht_iterator(m1);

  while (ht_next(&hti1)) {
    int64_t* curr_key = hti1.key;
    void* val1 = ht_get(m1, curr_key);
    void* val2 = ht_get(m2, curr_key);

    /* Check if other map has this key at all */
    if (!val2)
      return convert_to_cn_bool(0);

    if (convert_from_cn_bool(cn_bool_not(value_equality_fun(val1, val2)))) {
      // cn_printf(CN_LOGGING_INFO, "Values not equal!\n");
      return convert_to_cn_bool(0);
    }
  }

  return convert_to_cn_bool(1);
}

cn_bool* cn_map_equality(
    cn_map* m1, cn_map* m2, cn_bool*(value_equality_fun)(void*, void*)) {
  return cn_bool_and(cn_map_subset(m1, m2, value_equality_fun),
      cn_map_subset(m2, m1, value_equality_fun));
}

cn_pointer* convert_to_cn_pointer(const void* ptr) {
  cn_pointer* res = (cn_pointer*)cn_bump_malloc(sizeof(cn_pointer));
  res->ptr = (void*)ptr;  // Carries around an address
  return res;
}

void* convert_from_cn_pointer(cn_pointer* cn_ptr) {
  return cn_ptr->ptr;
}

cn_pointer* cn_pointer_min(cn_pointer* p, cn_pointer* q) {
  uintptr_t p_raw = (uintptr_t)convert_from_cn_pointer(p);
  uintptr_t q_raw = (uintptr_t)convert_from_cn_pointer(q);
  return convert_to_cn_pointer((void*)(p_raw < q_raw ? p_raw : q_raw));
}

cn_pointer* cn_pointer_max(cn_pointer* p, cn_pointer* q) {
  uintptr_t p_raw = (uintptr_t)convert_from_cn_pointer(p);
  uintptr_t q_raw = (uintptr_t)convert_from_cn_pointer(q);
  return convert_to_cn_pointer((void*)(p_raw > q_raw ? p_raw : q_raw));
}

cn_pointer* cn_pointer_mod(cn_pointer* ptr, cn_pointer* n) {
  uintptr_t ptr_raw = (uintptr_t)convert_from_cn_pointer(ptr);
  uintptr_t n_raw = (uintptr_t)convert_from_cn_pointer(n);
  return convert_to_cn_pointer((void*)(ptr_raw % n_raw));
}

static uint32_t cn_fls(uint32_t x) {
  return x ? sizeof(x) * 8 - __builtin_clz(x) : 0;
}

static uint64_t cn_flsl(uint64_t x) {
  return x ? sizeof(x) * 8 - __builtin_clzl(x) : 0;
}

cn_bits_u32* cn_bits_u32_fls(cn_bits_u32* i1) {
  cn_bits_u32* res = (cn_bits_u32*)cn_bump_malloc(sizeof(cn_bits_u32));
  res->val = cn_fls(i1->val);
  return res;
}

cn_bits_u64* cn_bits_u64_flsl(cn_bits_u64* i1) {
  cn_bits_u64* res = (cn_bits_u64*)cn_bump_malloc(sizeof(cn_bits_u64));
  res->val = cn_flsl(i1->val);
  return res;
}
