#ifndef BENNET_RAND_ALLOC_H
#define BENNET_RAND_ALLOC_H

#include <stdbool.h>
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

// Set the memory size for the random allocator (must be called before first allocation)
void bennet_rand_alloc_set_mem_size(size_t size);

// Toggle old-style allocation: overlap checking in rand_alloc + allocation
// tracking in alloc.c. When disabled (default), allocations are untracked and
// may overlap; allocation checks only verify the region lies within the buffer.
void bennet_set_old_style_alloc(bool enabled);
bool bennet_get_old_style_alloc(void);

#endif  // BENNET_RAND_ALLOC_H
