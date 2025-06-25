
#include <stdlib.h>

#include <cn-executable/fulminate_alloc.h>

struct alloc_fns fulminate_internal_alloc =
    (struct alloc_fns){.malloc = &malloc, .calloc = &calloc, .free = &free};
