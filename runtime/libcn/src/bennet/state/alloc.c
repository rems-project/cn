#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include <bennet/prelude.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils/vector.h>
#include <cn-executable/rmap.h>
#include <cn-executable/utils.h>

// Define the pointer_data structure
typedef struct {
  void* ptr;
  size_t sz;
} pointer_data;

// Declare vector types for pointer_data
BENNET_VECTOR_DECL(pointer_data)
BENNET_VECTOR_IMPL(pointer_data)

// Global vectors for tracking allocations and ownership
static bennet_vector(pointer_data) alloc_vector;
static bennet_vector(pointer_data) ownership_vector;

// Acceleration index for ownership overlap checks. Mirrors the ranges held in
// `ownership_vector` exactly (every owned region is also added here, and removed
// when the vector pops it). Lets `bennet_ownership_check` answer overlap queries
// in O(64/radix) instead of an O(n) scan of `ownership_vector`. The vector is
// retained as the undo-log: rmap has no enumeration, so `restore` reads the
// popped entries from the vector to know which ranges to remove.
static rmap ownership_map;

// Stored as the rmap value for every owned byte. Must differ from the rmap
// empty sentinel (INT_MIN) so `rmap_find_range` can report "nothing owned".
#define BENNET_OWNERSHIP_OWNED 1

void bennet_alloc_destroy(void) {
  bennet_vector_free(pointer_data)(&alloc_vector);
}

void bennet_alloc_init(void) {
  bennet_vector_init(pointer_data)(&alloc_vector);
}

size_t bennet_alloc_save(void) {
  return bennet_vector_size(pointer_data)(&alloc_vector);
}

void bennet_alloc_restore(size_t size) {
  if (size <= bennet_vector_size(pointer_data)(&alloc_vector)) {
    // Truncate the vector to the saved size
    while (bennet_vector_size(pointer_data)(&alloc_vector) > size) {
      pointer_data removed = bennet_vector_pop(pointer_data)(&alloc_vector);
      bennet_rand_alloc_free(removed.ptr);
    }
    return;
  }

  fprintf(stderr, "Error: Tried to grow allocation data\n");
  exit(1);
}

void bennet_ownership_destroy(void) {
  bennet_vector_free(pointer_data)(&ownership_vector);
  rmap_free(ownership_map);
  ownership_map = NULL;
}

void bennet_ownership_init(void) {
  bennet_vector_init(pointer_data)(&ownership_vector);
  ownership_map = rmap_create(2, malloc, free);
}

size_t bennet_ownership_save(void) {
  return bennet_vector_size(pointer_data)(&ownership_vector);
}

void bennet_ownership_restore(size_t size) {
  if (size <= bennet_vector_size(pointer_data)(&ownership_vector)) {
    // Truncate the vector to the saved size, removing each popped range from the
    // mirror index. Owned regions are disjoint, so removing a popped range is the
    // exact inverse of the `rmap_add` done when it was pushed.
    while (bennet_vector_size(pointer_data)(&ownership_vector) > size) {
      pointer_data removed = bennet_vector_pop(pointer_data)(&ownership_vector);
      if (removed.sz != 0) {
        rmap_remove((uintptr_t)removed.ptr,
            (uintptr_t)removed.ptr + removed.sz - 1,
            ownership_map);
      }
    }
    return;
  }

  fprintf(stderr, "Error: Tried to grow ownership data\n");
  exit(1);
}

void* bennet_alloc(size_t bytes) {
  void* p = bennet_rand_alloc(bytes);
  if (!p) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }
  bennet_alloc_record(p, bytes);
  return p;
}

void* bennet_alloc_bounded(size_t bytes, uintptr_t lower_bound, uintptr_t upper_bound) {
  void* p = bennet_rand_alloc_bounded(bytes, lower_bound, upper_bound);
  if (!p) {
    cn_failure(CN_FAILURE_FULM_ALLOC, NON_SPEC);
  }
  bennet_alloc_record(p, bytes);
  return p;
}

void bennet_alloc_record(void* p, size_t sz) {
  if (!bennet_get_old_style_alloc()) {
    return;  // New style: no allocation tracking
  }
  pointer_data data = {.ptr = p, .sz = sz};
  bennet_vector_push(pointer_data)(&alloc_vector, data);
}

int bennet_alloc_check(void* p, size_t sz) {
  if (!bennet_get_old_style_alloc()) {
    // New style: just check [p, p+sz) fits within the rand_alloc buffer
    if (sz == 0) {
      return 1;
    }
    uintptr_t lo = (uintptr_t)bennet_rand_alloc_min_ptr();
    uintptr_t hi = (uintptr_t)bennet_rand_alloc_max_ptr();  // inclusive
    uintptr_t a = (uintptr_t)p;
    uintptr_t b = a + sz - 1;  // inclusive end
    return (a >= lo && b <= hi && a <= b);
  }

  if (bennet_vector_size(pointer_data)(&alloc_vector) == 0) {
    return 0;
  }

  int bytes = sz;

  // Iterate through all allocation records
  for (size_t i = 0; i < bennet_vector_size(pointer_data)(&alloc_vector); i++) {
    pointer_data* q = bennet_vector_get(pointer_data)(&alloc_vector, i);
    uintptr_t lb = (uintptr_t)q->ptr;
    uintptr_t ub = (uintptr_t)q->ptr + q->sz;
    if (lb < (uintptr_t)p) {
      lb = (uintptr_t)p;
    }
    if (ub > (uintptr_t)p + sz) {
      ub = (uintptr_t)p + sz;
    }
    if (ub > lb) {
      bytes -= (ub - lb);
    }

    if (bytes == 0) {
      return 1;
    }
  }
  assert(bytes >= 0);
  return (bytes == 0);
}

void bennet_ownership_update(void* ptr, size_t sz) {
  pointer_data data = {.ptr = ptr, .sz = sz};
  bennet_vector_push(pointer_data)(&ownership_vector, data);
  if (sz != 0) {
    rmap_add(
        (uintptr_t)ptr, (uintptr_t)ptr + sz - 1, BENNET_OWNERSHIP_OWNED, ownership_map);
  }
}

int bennet_ownership_check(void* p, size_t sz) {
  if (sz == 0) {
    return 1;
  }

  // Query the mirror index: if any byte in [p, p+sz) is already owned, the range
  // query returns a non-empty result (max != INT_MIN), i.e. an overlap. Owned
  // regions are disjoint, so the only value ever stored is BENNET_OWNERSHIP_OWNED.
  rmap_range_res_t res =
      rmap_find_range((uintptr_t)p, (uintptr_t)p + sz - 1, ownership_map);
  return res.max == INT_MIN;
}

size_t bennet_ownership_size(void) {
  size_t total = 0;

  for (size_t i = 0; i < bennet_vector_size(pointer_data)(&ownership_vector); i++) {
    pointer_data* pd = bennet_vector_get(pointer_data)(&ownership_vector, i);

    total += pd->sz;
  }

  return total;
}
