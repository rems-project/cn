#ifndef FULMINATE_STACK_H
#define FULMINATE_STACK_H

#include <cn-executable/fulminate_alloc.h>

#define NODE_DEF(NAME, CTYPE)                                                            \
  typedef struct NAME##_node {                                                           \
    CTYPE NAME;                                                                          \
    struct NAME##_node *next;                                                            \
  } NAME##_node;

#define STACK_DEF(NAME)                                                                  \
  typedef struct NAME##_stack {                                                          \
    NAME##_node *top;                                                                    \
    int size;                                                                            \
  } NAME##_stack;

#define STACK_INIT(NAME)                                                                 \
  static inline NAME##_stack *NAME##_stack_init(allocator *alloc) {                      \
    NAME##_stack *s = (NAME##_stack *)alloc->malloc(sizeof(NAME##_stack));               \
    s->size = 0;                                                                         \
    return s;                                                                            \
  }

#define STACK_PUSH(NAME, CTYPE)                                                          \
  static inline void NAME##_stack_push(NAME##_stack *s, CTYPE elem, allocator *alloc) {  \
    NAME##_node *node = (NAME##_node *)alloc->malloc(sizeof(NAME##_node));               \
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
  static inline CTYPE NAME##_stack_pop(NAME##_stack *s, allocator *alloc) {              \
    if (s->size > 0 && s->top) {                                                         \
      NAME##_node *old_top = s->top;                                                     \
      CTYPE elem = old_top->NAME;                                                        \
      if (s->size == 1) {                                                                \
        s->top = 0;                                                                      \
      } else {                                                                           \
        s->top = s->top->next;                                                           \
      }                                                                                  \
      alloc->free(old_top);                                                              \
      s->size--;                                                                         \
      return elem;                                                                       \
    }                                                                                    \
    return 0;                                                                            \
  }

#define STACK_SIZE(NAME)                                                                 \
  static inline int NAME##_stack_size(NAME##_stack *s) {                                 \
    return s->size;                                                                      \
  }

#define GEN_ALL_STACK(NAME, CTYPE)                                                       \
  NODE_DEF(NAME, CTYPE)                                                                  \
  STACK_DEF(NAME)                                                                        \
  STACK_INIT(NAME)                                                                       \
  STACK_PUSH(NAME, CTYPE)                                                                \
  STACK_POP(NAME, CTYPE)                                                                 \
  STACK_SIZE(NAME)

#endif  // FULMINATE_STACK_H
