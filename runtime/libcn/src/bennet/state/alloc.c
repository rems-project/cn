#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

#include <bennet/prelude.h>
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

// Acceleration indices mirroring the two vectors exactly (every range pushed to a
// vector is also added here, and removed when the vector pops it). They let
// `bennet_alloc_check` / `bennet_ownership_check` answer their per-assignment
// queries in O(64/radix) instead of an O(n) scan. The vectors are retained as the
// undo-log: rmap has no enumeration, so `restore` reads the popped entries from the
// vector to know which ranges to remove (and alloc still needs the pointer for
// `bennet_rand_alloc_free`, ownership still sums sizes for `bennet_ownership_size`).
static rmap alloc_map;
static rmap ownership_map;

// Stored as the rmap value for every tracked byte. Must differ from the rmap empty
// sentinel (INT_MIN) so `rmap_find_range` can report "nothing here".
#define BENNET_PRESENT 1

void bennet_alloc_destroy(void) {
  bennet_vector_free(pointer_data)(&alloc_vector);
  rmap_free(alloc_map);
  alloc_map = NULL;
}

void bennet_alloc_init(void) {
  bennet_vector_init(pointer_data)(&alloc_vector);
  alloc_map = rmap_create(2, malloc, free);
}

size_t bennet_alloc_save(void) {
  return bennet_vector_size(pointer_data)(&alloc_vector);
}

void bennet_alloc_restore(size_t size) {
  if (size <= bennet_vector_size(pointer_data)(&alloc_vector)) {
    // Truncate the vector to the saved size, removing each popped range from the
    // mirror index. Allocation records are disjoint (rand_alloc hands out
    // non-overlapping slices), so removing a popped range is the exact inverse of
    // the `rmap_add` done when it was pushed.
    while (bennet_vector_size(pointer_data)(&alloc_vector) > size) {
      pointer_data removed = bennet_vector_pop(pointer_data)(&alloc_vector);
      bennet_rand_alloc_free(removed.ptr);
      if (removed.sz != 0) {
        rmap_remove(
            (uintptr_t)removed.ptr, (uintptr_t)removed.ptr + removed.sz - 1, alloc_map);
      }
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

void bennet_alloc_record(void* p, size_t sz) {
  pointer_data data = {.ptr = p, .sz = sz};
  bennet_vector_push(pointer_data)(&alloc_vector, data);
  if (sz != 0) {
    rmap_add((uintptr_t)p, (uintptr_t)p + sz - 1, BENNET_PRESENT, alloc_map);
  }
}

int bennet_alloc_check(void* p, size_t sz) {
  if (sz == 0) {
    return 1;
  }

  // `p` is an arbitrary generated (possibly wild) pointer here, so guard the
  // inclusive end against wrapping the address space: a range that wraps cannot be
  // a valid allocation, so report "not covered" (as the old linear scan did).
  uintptr_t lo = (uintptr_t)p, hi = lo + sz - 1;
  if (hi < lo) {
    return 0;
  }

  // Query the mirror index: the range is fully covered iff no byte in [p, p+sz) is
  // unmapped. An unmapped byte injects the empty sentinel, dragging `.min` down to
  // INT_MIN, while a gap-free range keeps `.min == BENNET_PRESENT`. Allocation
  // records are disjoint, so the range is covered exactly when every byte is present.
  rmap_range_res_t res = rmap_find_range(lo, hi, alloc_map);
  return res.min != INT_MIN;
}

void bennet_ownership_update(void* ptr, size_t sz) {
  pointer_data data = {.ptr = ptr, .sz = sz};
  bennet_vector_push(pointer_data)(&ownership_vector, data);
  if (sz != 0) {
    rmap_add((uintptr_t)ptr, (uintptr_t)ptr + sz - 1, BENNET_PRESENT, ownership_map);
  }
}

int bennet_ownership_check(void* p, size_t sz) {
  if (sz == 0) {
    return 1;
  }

  // `p` may be an arbitrary generated (possibly wild) pointer, so guard the
  // inclusive end against wrapping the address space: a wrapping range cannot
  // overlap any (non-wrapping) owned region, so report "no overlap".
  uintptr_t lo = (uintptr_t)p, hi = lo + sz - 1;
  if (hi < lo) {
    return 1;
  }

  // Query the mirror index: if any byte in [p, p+sz) is already owned, the range
  // query returns a non-empty result (`.max != INT_MIN`), i.e. an overlap. Owned
  // regions are disjoint, so the only value ever stored is BENNET_PRESENT.
  rmap_range_res_t res = rmap_find_range(lo, hi, ownership_map);
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
