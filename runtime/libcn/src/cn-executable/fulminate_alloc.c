
#include <stdlib.h>

struct alloc_fns {
  void *(*malloc)(size_t size);
  void *(*calloc)(size_t count, size_t size);
  void (*free)(void *p);
};

struct alloc_fns fulminate_internal_alloc =
    (struct alloc_fns){.malloc = &malloc, .calloc = &calloc, .free = &free};
