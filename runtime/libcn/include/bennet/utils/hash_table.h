#ifndef BENNET_HASH_TABLE_H
#define BENNET_HASH_TABLE_H

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <bennet/utils/optional.h>

#ifdef __cplusplus
extern "C" {
#endif

#define bennet_hash_table(kty, vty) bennet_hash_table_##kty##_##vty
#define bennet_hash_entry(kty, vty) bennet_hash_entry_##kty##_##vty

#define BENNET_HASH_TABLE_DECL(kty, vty)                                                 \
  typedef struct {                                                                       \
    kty key;                                                                             \
    vty value;                                                                           \
    bool occupied;                                                                       \
  } bennet_hash_entry(kty, vty);                                                         \
                                                                                         \
  typedef struct {                                                                       \
    bennet_hash_entry(kty, vty) * entries;                                               \
    size_t size;                                                                         \
    size_t capacity;                                                                     \
    size_t (*hash_fn)(kty);                                                              \
    bool (*eq_fn)(kty, kty);                                                             \
  } bennet_hash_table(kty, vty);

// Macro to generate function names for given key and value types
#define bennet_hash_table_init(kty, vty)     bennet_hash_table_init_##kty##_##vty
#define bennet_hash_table_free(kty, vty)     bennet_hash_table_free_##kty##_##vty
#define bennet_hash_table_reserve(kty, vty)  bennet_hash_table_reserve_##kty##_##vty
#define bennet_hash_table_get(kty, vty)      bennet_hash_table_get_##kty##_##vty
#define bennet_hash_table_set(kty, vty)      bennet_hash_table_set_##kty##_##vty
#define bennet_hash_table_delete(kty, vty)   bennet_hash_table_delete_##kty##_##vty
#define bennet_hash_table_contains(kty, vty) bennet_hash_table_contains_##kty##_##vty
#define bennet_hash_table_size(kty, vty)     bennet_hash_table_size_##kty##_##vty
#define bennet_hash_table_capacity(kty, vty) bennet_hash_table_capacity_##kty##_##vty
#define bennet_hash_table_clear(kty, vty)    bennet_hash_table_clear_##kty##_##vty

// Default hash functions for common types
#define BENNET_DEFAULT_HASH_IMPL(kty)                                                    \
  static inline size_t bennet_hash_##kty(kty key) {                                      \
    return (size_t)key;                                                                  \
  }

BENNET_DEFAULT_HASH_IMPL(int8_t)
BENNET_DEFAULT_HASH_IMPL(uint8_t)
BENNET_DEFAULT_HASH_IMPL(int16_t)
BENNET_DEFAULT_HASH_IMPL(uint16_t)
BENNET_DEFAULT_HASH_IMPL(int32_t)
BENNET_DEFAULT_HASH_IMPL(uint32_t)
BENNET_DEFAULT_HASH_IMPL(int64_t)
BENNET_DEFAULT_HASH_IMPL(uint64_t)
BENNET_DEFAULT_HASH_IMPL(size_t)

// Default equality functions for common types
#define BENNET_DEFAULT_EQUALITY_IMPL(kty)                                                \
  static inline bool bennet_eq_##kty(kty a, kty b) {                                     \
    return a == b;                                                                       \
  }

BENNET_DEFAULT_EQUALITY_IMPL(int8_t)
BENNET_DEFAULT_EQUALITY_IMPL(uint8_t)
BENNET_DEFAULT_EQUALITY_IMPL(int16_t)
BENNET_DEFAULT_EQUALITY_IMPL(uint16_t)
BENNET_DEFAULT_EQUALITY_IMPL(int32_t)
BENNET_DEFAULT_EQUALITY_IMPL(uint32_t)
BENNET_DEFAULT_EQUALITY_IMPL(int64_t)
BENNET_DEFAULT_EQUALITY_IMPL(uint64_t)
BENNET_DEFAULT_EQUALITY_IMPL(size_t)

// Macro to define hash table functions for given key and value types
#define BENNET_HASH_TABLE_IMPL(kty, vty)                                                 \
  __attribute__((unused)) static inline void bennet_hash_table_init(kty, vty)(           \
      bennet_hash_table(kty, vty) * table,                                               \
      size_t (*hash_fn)(kty),                                                            \
      bool (*eq_fn)(kty, kty)) {                                                         \
    table->entries = NULL;                                                               \
    table->size = 0;                                                                     \
    table->capacity = 0;                                                                 \
    table->hash_fn = hash_fn;                                                            \
    table->eq_fn = eq_fn;                                                                \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_hash_table_free(kty, vty)(           \
      bennet_hash_table(kty, vty) * table) {                                             \
    if (table->entries) {                                                                \
      free(table->entries);                                                              \
      table->entries = NULL;                                                             \
    }                                                                                    \
    table->size = 0;                                                                     \
    table->capacity = 0;                                                                 \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_hash_table_reserve(kty, vty)(        \
      bennet_hash_table(kty, vty) * table, size_t new_cap) {                             \
    if (new_cap > table->capacity) {                                                     \
      size_t cap = table->capacity ? table->capacity : 8;                                \
      while (cap < new_cap) {                                                            \
        cap *= 2;                                                                        \
      }                                                                                  \
                                                                                         \
      bennet_hash_entry(kty, vty) *old_entries = table->entries;                         \
      size_t old_capacity = table->capacity;                                             \
      bennet_hash_entry(kty, vty) *new_entries =                                         \
          (bennet_hash_entry(kty, vty) *)calloc(cap, sizeof(*new_entries));              \
      assert(new_entries != NULL);                                                       \
                                                                                         \
      /* Rehash existing entries into new array */                                       \
      if (old_entries) {                                                                 \
        for (size_t i = 0; i < old_capacity; ++i) {                                      \
          if (old_entries[i].occupied) {                                                 \
            kty key = old_entries[i].key;                                                \
            vty value = old_entries[i].value;                                            \
            size_t hash = table->hash_fn(key);                                           \
            size_t index = hash % cap;                                                   \
            while (new_entries[index].occupied) {                                        \
              index = (index + 1) % cap;                                                 \
            }                                                                            \
            new_entries[index].key = key;                                                \
            new_entries[index].value = value;                                            \
            new_entries[index].occupied = true;                                          \
          }                                                                              \
        }                                                                                \
        free(old_entries);                                                               \
      }                                                                                  \
                                                                                         \
      table->entries = new_entries;                                                      \
      table->capacity = cap;                                                             \
    }                                                                                    \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline bennet_optional(vty)                             \
      bennet_hash_table_get(kty, vty)(bennet_hash_table(kty, vty) * table, kty key) {    \
    if (table->capacity == 0) {                                                          \
      return bennet_optional_none(vty);                                                  \
    }                                                                                    \
                                                                                         \
    size_t hash = table->hash_fn(key);                                                   \
    size_t index = hash % table->capacity;                                               \
    size_t start_index = index;                                                          \
                                                                                         \
    do {                                                                                 \
      if (!table->entries[index].occupied) {                                             \
        return bennet_optional_none(vty);                                                \
      }                                                                                  \
                                                                                         \
      if (table->eq_fn(table->entries[index].key, key)) {                                \
        return bennet_optional_some(vty, table->entries[index].value);                   \
      }                                                                                  \
                                                                                         \
      index = (index + 1) % table->capacity;                                             \
    } while (index != start_index);                                                      \
                                                                                         \
    return bennet_optional_none(vty);                                                    \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_hash_table_set(kty, vty)(            \
      bennet_hash_table(kty, vty) * table, kty key, vty value) {                         \
    /* Resize if load factor exceeds 0.75 */                                             \
    if (table->size >= table->capacity * 3 / 4) {                                        \
      bennet_hash_table_reserve(kty, vty)(                                               \
          table, table->capacity ? table->capacity * 2 : 8);                             \
    }                                                                                    \
                                                                                         \
    size_t hash = table->hash_fn(key);                                                   \
    size_t index = hash % table->capacity;                                               \
    size_t start_index = index;                                                          \
                                                                                         \
    do {                                                                                 \
      if (!table->entries[index].occupied) {                                             \
        table->entries[index].key = key;                                                 \
        table->entries[index].value = value;                                             \
        table->entries[index].occupied = true;                                           \
        table->size++;                                                                   \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      if (table->eq_fn(table->entries[index].key, key)) {                                \
        table->entries[index].value = value;                                             \
        return;                                                                          \
      }                                                                                  \
                                                                                         \
      index = (index + 1) % table->capacity;                                             \
    } while (index != start_index);                                                      \
                                                                                         \
    /* Should never reach here if resize worked properly */                              \
    assert(false);                                                                       \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline bool bennet_hash_table_delete(kty, vty)(         \
      bennet_hash_table(kty, vty) * table, kty key) {                                    \
    if (table->capacity == 0) {                                                          \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    size_t hash = table->hash_fn(key);                                                   \
    size_t index = hash % table->capacity;                                               \
    size_t start_index = index;                                                          \
                                                                                         \
    do {                                                                                 \
      if (!table->entries[index].occupied) {                                             \
        return false;                                                                    \
      }                                                                                  \
                                                                                         \
      if (table->eq_fn(table->entries[index].key, key)) {                                \
        table->entries[index].occupied = false;                                          \
        table->size--;                                                                   \
                                                                                         \
        /* Rehash following entries to handle clustering */                              \
        index = (index + 1) % table->capacity;                                           \
        while (table->entries[index].occupied) {                                         \
          kty rehash_key = table->entries[index].key;                                    \
          vty rehash_value = table->entries[index].value;                                \
          table->entries[index].occupied = false;                                        \
          table->size--;                                                                 \
          bennet_hash_table_set(kty, vty)(table, rehash_key, rehash_value);              \
          index = (index + 1) % table->capacity;                                         \
        }                                                                                \
                                                                                         \
        return true;                                                                     \
      }                                                                                  \
                                                                                         \
      index = (index + 1) % table->capacity;                                             \
    } while (index != start_index);                                                      \
                                                                                         \
    return false;                                                                        \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline bool bennet_hash_table_contains(kty, vty)(       \
      bennet_hash_table(kty, vty) * table, kty key) {                                    \
    return bennet_optional_is_some(bennet_hash_table_get(kty, vty)(table, key));         \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline size_t bennet_hash_table_size(kty, vty)(         \
      bennet_hash_table(kty, vty) * table) {                                             \
    return table->size;                                                                  \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline size_t bennet_hash_table_capacity(kty, vty)(     \
      bennet_hash_table(kty, vty) * table) {                                             \
    return table->capacity;                                                              \
  }                                                                                      \
                                                                                         \
  __attribute__((unused)) static inline void bennet_hash_table_clear(kty, vty)(          \
      bennet_hash_table(kty, vty) * table) {                                             \
    for (size_t i = 0; i < table->capacity; ++i) {                                       \
      table->entries[i].occupied = false;                                                \
    }                                                                                    \
    table->size = 0;                                                                     \
  }

#ifdef __cplusplus
}
#endif

#endif  // BENNET_HASH_TABLE_H
