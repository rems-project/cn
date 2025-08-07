#ifndef BENNET_RAND_ALLOC_H
#define BENNET_RAND_ALLOC_H

#include <stddef.h>
#include <stdint.h>

#include <bennet/internals/domain.h>

// Opaque struct for the allocator
struct rand_alloc;

// Allocate a random, non-overlapping region of the given length
void *bennet_rand_alloc(size_t bytes);

// Allocate a random, non-overlapping region of the given domain
void *bennet_rand_alloc_bounded(
    size_t length, uintptr_t lower_bound, uintptr_t upper_bound);

// Free the allocator and its resources
void bennet_rand_alloc_free_all(void);

// Free a specific region allocated
void bennet_rand_alloc_free(void *ptr);

// Returns the minimum pointer that can be generated
void *bennet_rand_alloc_min_ptr(void);

// Returns the maximum (inclusive) pointer that can be generated
void *bennet_rand_alloc_max_ptr(void);

#endif  // BENNET_RAND_ALLOC_H
