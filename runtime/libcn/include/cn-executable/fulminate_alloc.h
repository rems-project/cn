#ifndef FULMINATE_ALLOC
#define FULMINATE_ALLOC

//////////////////////////////////
// Fulminate Allocator //
//////////////////////////////////

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

struct alloc_fns {
  void *(*malloc)(size_t size);
  void *(*calloc)(size_t count, size_t size);
  void (*free)(void *p);
};

static struct alloc_fns fulminate_internal_alloc =
    (struct alloc_fns){.malloc = &malloc, .calloc = &calloc, .free = &free};

#define CN_GEN_MAP_GET(CNTYPE)                                                           \
  static inline void *cn_map_get_##CNTYPE(cn_map *m, cn_integer *key) {                  \
    int64_t *key_ptr = (*fulminate_internal_alloc.malloc)(sizeof(int64_t));              \
    *key_ptr = key->val;                                                                 \
    void *res = ht_get(m, key_ptr);                                                      \
    (*fulminate_internal_alloc.free)(key_ptr);                                           \
    if (!res) {                                                                          \
      return (void *)default_##CNTYPE();                                                 \
    }                                                                                    \
    return res;                                                                          \
  }

#ifdef __cplusplus
}
#endif

#endif  // FULMINATE_ALLOC
