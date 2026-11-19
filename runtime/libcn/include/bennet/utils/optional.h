#ifndef BENNET_OPTIONAL_H
#define BENNET_OPTIONAL_H

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

/////////////////
// Definitions //
/////////////////

#define bennet_optional(ty) struct bennet_optional_##ty

#define bennet_optional_is_none(...) (!(__VA_ARGS__).is_some)
#define bennet_optional_is_some(...) ((__VA_ARGS__).is_some)

#define bennet_optional_unwrap(...)                                                      \
  ({                                                                                     \
    assert((__VA_ARGS__).is_some);                                                       \
    (__VA_ARGS__).body;                                                                  \
  })

#define bennet_optional_none(ty)                                                         \
  ({                                                                                     \
    bennet_optional(ty) _temp = {.is_some = false};                                      \
    memset(&_temp.body, 0, sizeof(ty));                                                  \
    _temp;                                                                               \
  })
#define bennet_optional_some(ty, ...)                                                    \
  ((bennet_optional(ty)){.is_some = true, .body = __VA_ARGS__})

#define bennet_optional_cast(ty, ...)                                                    \
  ((bennet_optional(ty)){                                                                \
      .is_some = (__VA_ARGS__).is_some, .body = (ty)(__VA_ARGS__).body})

/////////////////

#define BENNET_OPTIONAL_DECL(ty)                                                         \
  bennet_optional(ty) {                                                                  \
    bool is_some;                                                                        \
    ty body;                                                                             \
  }

BENNET_OPTIONAL_DECL(int8_t);
BENNET_OPTIONAL_DECL(uint8_t);
BENNET_OPTIONAL_DECL(int16_t);
BENNET_OPTIONAL_DECL(uint16_t);
BENNET_OPTIONAL_DECL(int32_t);
BENNET_OPTIONAL_DECL(uint32_t);
BENNET_OPTIONAL_DECL(int64_t);
BENNET_OPTIONAL_DECL(uint64_t);
BENNET_OPTIONAL_DECL(intptr_t);
BENNET_OPTIONAL_DECL(uintptr_t);
BENNET_OPTIONAL_DECL(intmax_t);
BENNET_OPTIONAL_DECL(uintmax_t);
BENNET_OPTIONAL_DECL(size_t);

#define bennet_optional_equal(ty) bennet_optional_equal_##ty

#define BENNET_OPTIONAL_EQUAL_IMPL(ty)                                                   \
  __attribute__((unused)) static inline bool bennet_optional_equal(ty)(                  \
      bennet_optional(ty) * o1, bennet_optional(ty) * o2) {                              \
    if (o1->is_some) {                                                                   \
      return o2->is_some && o1->body == o2->body;                                        \
    }                                                                                    \
                                                                                         \
    return !o2->is_some;                                                                 \
  }

BENNET_OPTIONAL_EQUAL_IMPL(int8_t);
BENNET_OPTIONAL_EQUAL_IMPL(uint8_t);
BENNET_OPTIONAL_EQUAL_IMPL(int16_t);
BENNET_OPTIONAL_EQUAL_IMPL(uint16_t);
BENNET_OPTIONAL_EQUAL_IMPL(int32_t);
BENNET_OPTIONAL_EQUAL_IMPL(uint32_t);
BENNET_OPTIONAL_EQUAL_IMPL(int64_t);
BENNET_OPTIONAL_EQUAL_IMPL(uint64_t);
BENNET_OPTIONAL_EQUAL_IMPL(intptr_t);
BENNET_OPTIONAL_EQUAL_IMPL(uintptr_t);
BENNET_OPTIONAL_EQUAL_IMPL(intmax_t);
BENNET_OPTIONAL_EQUAL_IMPL(uintmax_t);

#define bennet_optional_unwrap_or(ty) bennet_optional_unwrap_or_##ty

#define BENNET_OPTIONAL_UNWRAP_OR_IMPL(ty)                                               \
  __attribute__((unused)) static inline ty bennet_optional_unwrap_or(ty)(                \
      bennet_optional(ty) * o, ty alt) {                                                 \
    if (o->is_some) {                                                                    \
      return o->body;                                                                    \
    }                                                                                    \
                                                                                         \
    return alt;                                                                          \
  }

BENNET_OPTIONAL_UNWRAP_OR_IMPL(int8_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(uint8_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(int16_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(uint16_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(int32_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(uint32_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(int64_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(uint64_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(intptr_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(uintptr_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(intmax_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(uintmax_t);
BENNET_OPTIONAL_UNWRAP_OR_IMPL(size_t);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_OPTIONAL_H
