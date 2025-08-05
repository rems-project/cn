#include <stdio.h>
#include <stdlib.h>

#include <bennet/prelude.h>
#include <bennet/utils/vector.h>
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

void bennet_alloc_reset(void) {
  bennet_vector_free(pointer_data)(&alloc_vector);
  bennet_vector_init(pointer_data)(&alloc_vector);
}

size_t bennet_alloc_save(void) {
  return bennet_vector_size(pointer_data)(&alloc_vector);
}

void bennet_alloc_restore(size_t size) {
  if (size <= bennet_vector_size(pointer_data)(&alloc_vector)) {
    // Truncate the vector to the saved size
    while (bennet_vector_size(pointer_data)(&alloc_vector) > size) {
      bennet_vector_pop(pointer_data)(&alloc_vector);
    }
    return;
  }

  fprintf(stderr, "Error: Tried to grow allocation data\n");
  exit(1);
}

void bennet_ownership_reset(void) {
  bennet_vector_free(pointer_data)(&ownership_vector);
  bennet_vector_init(pointer_data)(&ownership_vector);
}

size_t bennet_ownership_save(void) {
  return bennet_vector_size(pointer_data)(&ownership_vector);
}

void bennet_ownership_restore(size_t size) {
  if (size <= bennet_vector_size(pointer_data)(&ownership_vector)) {
    // Truncate the vector to the saved size
    while (bennet_vector_size(pointer_data)(&ownership_vector) > size) {
      bennet_vector_pop(pointer_data)(&ownership_vector);
    }
    return;
  }

  fprintf(stderr, "Error: Tried to grow ownership data\n");
  exit(1);
}

void bennet_alloc_record(void* p, size_t sz) {
  pointer_data data = {.ptr = p, .sz = sz};
  bennet_vector_push(pointer_data)(&alloc_vector, data);
}

cn_pointer* bennet_alloc(bennet_domain(uintptr_t) * cs) {
  size_t bytes = cs->lower_offset_bound + cs->upper_offset_bound;

  // TODO: Perform static analysis to use the following when possible:
  // cn_bump_aligned_alloc(
  //    bennet_optional_unwrap_or(uintptr_t)
  //      (&cs->multiple, alignof(max_align_t)), bytes);
  // Much faster than `bennet_rand_alloc_bounded`

  void* p = bennet_rand_alloc_bounded(cs);

  bennet_alloc_record(p, bytes);

  return convert_to_cn_pointer(p + cs->lower_offset_bound);
}

int bennet_alloc_check(void* p, size_t sz) {
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
}

int bennet_ownership_check(void* p, size_t sz) {
  if (bennet_vector_size(pointer_data)(&ownership_vector) == 0) {
    return 1;
  }

  // Iterate through all ownership records
  for (size_t i = 0; i < bennet_vector_size(pointer_data)(&ownership_vector); i++) {
    pointer_data* q = bennet_vector_get(pointer_data)(&ownership_vector, i);
    uintptr_t lb = (uintptr_t)q->ptr;
    uintptr_t ub = (uintptr_t)q->ptr + q->sz;
    if (lb < (uintptr_t)p) {
      lb = (uintptr_t)p;
    }
    if (ub > (uintptr_t)p + sz) {
      ub = (uintptr_t)p + sz;
    }
    if (ub > lb) {
      return 0;
    }
  }

  return 1;
}

size_t bennet_ownership_size(void) {
  size_t total = 0;

  for (size_t i = 0; i < bennet_vector_size(pointer_data)(&ownership_vector); i++) {
    pointer_data* pd = bennet_vector_get(pointer_data)(&ownership_vector, i);

    total += pd->sz;
  }

  return total;
}
