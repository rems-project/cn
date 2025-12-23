#ifndef BENNET_VECTOR_H
#define BENNET_VECTOR_H

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

// Macro to declare a vector type for a given type 'ty'
#define bennet_vector(ty) struct bennet_vector_##ty

#define BENNET_VECTOR_DECL(ty)                                                           \
  typedef bennet_vector(ty) {                                                            \
    ty *data;                                                                            \
    size_t size;                                                                         \
    size_t capacity;                                                                     \
  }                                                                                      \
  bennet_vector_##ty;

// Macro to generate function names for a given type 'ty'
#define bennet_vector_init(ty)     bennet_vector_init_##ty
#define bennet_vector_free(ty)     bennet_vector_free_##ty
#define bennet_vector_reserve(ty)  bennet_vector_reserve_##ty
#define bennet_vector_push(ty)     bennet_vector_push_##ty
#define bennet_vector_pop(ty)      bennet_vector_pop_##ty
#define bennet_vector_get(ty)      bennet_vector_get_##ty
#define bennet_vector_set(ty)      bennet_vector_set_##ty
#define bennet_vector_size(ty)     bennet_vector_size_##ty
#define bennet_vector_capacity(ty) bennet_vector_capacity_##ty
#define bennet_vector_delete(ty)   bennet_vector_delete_##ty

// Macro to define vector functions for a given type 'ty'
#define BENNET_VECTOR_IMPL(ty)                                                           \
  __attribute__((unused)) static inline void bennet_vector_init(ty)(                     \
      bennet_vector(ty) * vec) {                                                         \
    vec->data = NULL;                                                                    \
    vec->size = 0;                                                                       \
    vec->capacity = 0;                                                                   \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_vector_free(ty)(                     \
      bennet_vector(ty) * vec) {                                                         \
    if (vec->data) {                                                                     \
      free(vec->data);                                                                   \
      vec->data = NULL;                                                                  \
    }                                                                                    \
    vec->size = 0;                                                                       \
    vec->capacity = 0;                                                                   \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_vector_reserve(ty)(                  \
      bennet_vector(ty) * vec, size_t new_cap) {                                         \
    if (new_cap > vec->capacity) {                                                       \
      size_t cap = vec->capacity ? vec->capacity : 1;                                    \
      while (cap < new_cap) {                                                            \
        cap *= 2;                                                                        \
      }                                                                                  \
      ty *new_data = (ty *)realloc(vec->data, cap * sizeof(ty));                         \
      assert(new_data != NULL);                                                          \
      vec->data = new_data;                                                              \
      vec->capacity = cap;                                                               \
    }                                                                                    \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_vector_push(ty)(                     \
      bennet_vector(ty) * vec, ty value) {                                               \
    if (vec->size == vec->capacity) {                                                    \
      bennet_vector_reserve(ty)(vec, vec->capacity ? vec->capacity * 2 : 1);             \
    }                                                                                    \
    vec->data[vec->size++] = value;                                                      \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline ty bennet_vector_pop(ty)(                        \
      bennet_vector(ty) * vec) {                                                         \
    assert(vec->size > 0);                                                               \
    return vec->data[--vec->size];                                                       \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline ty *bennet_vector_get(ty)(                       \
      bennet_vector(ty) * vec, size_t idx) {                                             \
    assert(idx < vec->size);                                                             \
    return &vec->data[idx];                                                              \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_vector_set(ty)(                      \
      bennet_vector(ty) * vec, size_t idx, ty value) {                                   \
    assert(idx < vec->size);                                                             \
    vec->data[idx] = value;                                                              \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_vector_delete(ty)(                   \
      bennet_vector(ty) * vec, size_t idx) {                                             \
    assert(idx < vec->size);                                                             \
                                                                                         \
    for (size_t i = idx; i + 1 < vec->size; ++i) {                                       \
      vec->data[i] = vec->data[i + 1];                                                   \
    }                                                                                    \
                                                                                         \
    vec->size -= 1;                                                                      \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline size_t bennet_vector_size(ty)(                   \
      bennet_vector(ty) * vec) {                                                         \
    return vec->size;                                                                    \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline size_t bennet_vector_capacity(ty)(               \
      bennet_vector(ty) * vec) {                                                         \
    return vec->capacity;                                                                \
  }

#ifdef __cplusplus
}
#endif

#endif  // BENNET_VECTOR_H
