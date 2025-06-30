#ifndef BENNET_EXP_OPTIONAL_H
#define BENNET_EXP_OPTIONAL_H

#include <assert.h>
#include <stdbool.h>

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
  ((bennet_optional(ty)){.is_some = false, .body = (ty){0}})
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

#endif  // BENNET_EXP_OPTIONAL_H
