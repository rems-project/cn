#ifndef FOCUS_CTX_H
#define FOCUS_CTX_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const char *type_sig;

// Wrapper for cn values
#define CN_INSERT_ITER_RES(base, start, end, size, sig) \
    insert_iter_res((uint64_t)base->ptr, start->val, end->val, size, sig)

#define CN_INSERT_FOCUS(index, sig) insert_focus(index->val, sig)


// Limitation: we only consider contiguous iter ress.
struct iter_res {
  uint64_t ptr;
  // contiguous indices (closed interval)
  uint64_t start;
  uint64_t end;
  // step
  uint64_t size;
  // type signature
  type_sig sig;
};

typedef struct iter_res_set {
  struct iter_res res;
  struct iter_res_set *next;
} iter_res_set;

struct focus_info {
  int64_t index;
  type_sig sig;
};

typedef struct focus_set {
  struct focus_info info;
  struct focus_set *next;
} focus_set;

struct focus_context {
  focus_set *indices;
  iter_res_set *iter_ress;
  struct focus_context *prev;
};

void initialise_focus_context(void);
void push_focus_context(void);
void pop_focus_context(void);
void insert_focus(int64_t index, type_sig sig);
void insert_iter_res(
    uint64_t ptr, uint64_t start, uint64_t end, uint64_t size, type_sig sig);
  int needs_focus(uint64_t address, uint64_t size);

#ifdef __cplusplus
}
#endif

#endif  // FOCUS_CTX_H
