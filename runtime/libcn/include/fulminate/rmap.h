#ifndef _RMAP_H
#define _RMAP_H

#include <cn-executable/rts_deps.h>

#ifdef _RMAP_DEBUG
  #include <stdio.h>
#endif /* _RMAP_DEBUG */

typedef void *(*malloc_f)(size_t);
typedef void (*free_f)(void *);

typedef uint64_t rmap_key_t;
typedef int rmap_value_t;

typedef struct {
  bool defined;
  rmap_value_t v;
} rmap_value_opt_t;

typedef struct {
  bool defined;
  rmap_value_t min;
  rmap_value_t max;
} rmap_range_res_t;

typedef struct rmap *rmap;

rmap rmap_create(unsigned int radix, malloc_f malloc, free_f free);
void rmap_free(rmap);
rmap_value_opt_t rmap_find(rmap_key_t k, rmap map);
rmap_range_res_t rmap_find_range(rmap_key_t k0, rmap_key_t k1, rmap map);
void rmap_add(rmap_key_t k0, rmap_key_t k1, rmap_value_t v, rmap map);
void rmap_remove(rmap_key_t k0, rmap_key_t k1, rmap map);

#ifdef _RMAP_DEBUG
bool rmap_is_wf(rmap map);
void dump(FILE *stream, bool, rmap);
#endif /* _RMAP_DEBUG */

#endif /* _RMAP_H */
