#include "fulminate_alloc.h"

// typedef struct cn_source_location_node {
//     char *cn_source_location;
//     cn_source_location_node *next;
// }cn_source_location_node;

// typedef struct cn_source_location_stack {
//     cn_source_location_node *top;
//     int size;
// } cn_source_location_stack;

#define NODE_DEF(NAME, CTYPE)                                                            \
  typedef struct NAME##_node {                                                           \
    CTYPE NAME;                                                                          \
    NAME##_node *next;                                                                   \
  } NAME##_node;

#define STACK_DEF(NAME)                                                                  \
  typedef struct NAME##_stack {                                                          \
    NAME##_node *top;                                                                    \
  } NAME##_stack;

#define STACK_INIT(NAME)                                                                 \
  static inline NAME##_stack *NAME##_stack_init(allocator *allocator) {                  \
    NAME##_stack *s = (NAME##_stack *)allocator->malloc(sizeof(NAME##_stack));           \
    s->size = 0;                                                                         \
    return s;                                                                            \
  }

#define STACK_PUSH(NAME, CTYPE)                                                          \
  static inline void push(NAME##_stack *s, CTYPE elem, allocator *allocator) {           \
    NAME##_node *node = (NAME##_node *)allocator->malloc(sizeof(NAME##_node));           \
    node->NAME = elem;                                                                   \
    if (!s->top) {                                                                       \
      s->top = node;                                                                     \
    } else {                                                                             \
      node->next = s->top;                                                               \
      s->top = node;                                                                     \
    }                                                                                    \
    s->size++;                                                                           \
  }

#define STACK_POP(NAME, CTYPE)                                                           \
  static inline CTYPE pop(NAME##_stack s, allocator *allocator) {                        \
    if (s->top) {                                                                        \
      CTYPE elem = s->top->NAME;                                                         \
      NAME##_node *old_top = s->top;                                                     \
      s->top = s->top->next;                                                             \
      allocator->free(old_top);                                                          \
      s->size--;                                                                         \
      return elem;                                                                       \
    }                                                                                    \
    return 0;                                                                            \
  }

#define STACK_SIZE(NAME)                                                                 \
  static inline int size(NAME##_stack *s) {                                              \
    return s->size;                                                                      \
  }

#define GEN_ALL_STACK(NAME, CTYPE)                                                       \
  NODE_DEF(NAME, CTYPE)                                                                  \
  STACK_DEF(NAME)                                                                        \
  STACK_INIT(NAME)                                                                       \
  STACK_PUSH(NAME, CTYPE)                                                                \
  STACK_POP(NAME, CTYPE)                                                                 \
  STACK_SIZE(NAME)
\
