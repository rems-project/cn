#ifndef FOCUS_CTX_H
#define FOCUS_CTX_H

#include <stdio.h>
#include <stdlib.h>
#include <cn-executable/cerb_types.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const char *type_sig;

// Defined in cn-autoannot/auto_annot.c
extern FILE *auto_annot_log_file;

#define cn_auto_annot_printf(...) fprintf(auto_annot_log_file, __VA_ARGS__)

// Wrapper for cn values
#define CN_INSERT_ITER_RES(base, start, end, size, sig)                                  \
  insert_iter_res((uint64_t)base->ptr, start->val, end->val, size, sig)

#define CN_INSERT_FOCUS(index, sig) insert_focus(index->val, sig)

#define CN_LOAD_ANNOT(LV, FMT, ...)                                                      \
  ({                                                                                     \
    typeof(LV) *__tmp = &(LV);                                                           \
    int64_t index;                                                                       \
    type_sig sig;                                                                        \
    if (needs_focus((uint64_t)__tmp, sizeof(typeof(LV)), &index, &sig))                  \
      cn_auto_annot_printf(FMT, "RW", sig, index, ##__VA_ARGS__);                        \
    CN_LOAD(LV);                                                                         \
  })

#define CN_STORE_OP_ANNOT(LV, op, X, FMT, ...)                                           \
  ({                                                                                     \
    typeof(LV) *__tmp;                                                                   \
    __tmp = &(LV);                                                                       \
    int64_t index;                                                                       \
    type_sig sig;                                                                        \
    if (needs_focus((uint64_t)__tmp, sizeof(typeof(LV)), &index, &sig))                  \
      cn_auto_annot_printf(FMT, "RW", sig, index, ##__VA_ARGS__);                        \
    CN_STORE_OP(LV, op, X);                                                              \
  })

#define CN_STORE_ANNOT(LV, X, FMT, ...) CN_STORE_OP_ANNOT(LV, , X, FMT, ##__VA_ARGS__)

void initialize_auto_annot(const char *log_file);
void finalize_auto_annot(void);

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

enum cn_res_type {
  CN_RES_ITER,
  OTHER,
};

struct cn_res {
  enum cn_res_type type;
  union {
    struct iter_res iter_res;
  };
};

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
  struct focus_context *prev;
};

void initialise_focus_context();
void push_focus_context(void);
void pop_focus_context(void);
void clear_focus(void);
void insert_focus(int64_t index, type_sig sig);
int needs_focus(uint64_t address, uint64_t size, int64_t *index_out, type_sig *sig_out);

#ifdef __cplusplus
}
#endif

#endif  // FOCUS_CTX_H
