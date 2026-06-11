#ifndef BENNET_ABSINT_H
#define BENNET_ABSINT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations for cn_term types (defined in cn-smt/terms.h) */
struct cn_term;
typedef struct cn_term cn_term;
struct cn_base_type;
typedef struct cn_base_type cn_base_type;

/*-----------------------------------------------------------------------------
 * Shared Types
 *---------------------------------------------------------------------------*/

#ifndef BENNET_ABSINT_SYM_DEFINED
  #define BENNET_ABSINT_SYM_DEFINED
typedef struct {
  const char* name;
  uint64_t id;
} bennet_absint_sym;
#endif

/**
 * A domain value tagged with its type.
 * Allows heterogeneous storage of domains for different integer types.
 */
typedef struct bennet_tagged_domain {
  cn_base_type* type; /* Type info (CN_BASE_BITS with signedness/width) */
  void* domain;       /* Pointer to the actual domain (e.g., bennet_domain(uint64_t)*) */
} bennet_tagged_domain;

typedef struct {
  uint64_t id;
  bennet_tagged_domain domain;
} bennet_absint_state_entry;

struct bennet_absint_state {
  bennet_absint_state_entry* entries;
  size_t count;
  size_t capacity;
};

typedef struct bennet_absint_state bennet_absint_state;

/*-----------------------------------------------------------------------------
 * Shared (Domain-Independent) Functions
 *---------------------------------------------------------------------------*/

/** Create a tagged domain from type and domain pointer */
bennet_tagged_domain bennet_tagged_domain_create(cn_base_type* type, void* domain);

/** Create an empty abstract state */
bennet_absint_state* bennet_absint_state_create(void);

/** Free an abstract state */
void bennet_absint_state_free(bennet_absint_state* state);

/** Find entry index for a symbol, or return -1 if not found */
int bennet_absint_state_find(bennet_absint_state* state, uint64_t id);

/** Check if a term contains a specific symbol */
bool term_contains_sym(cn_term* term, uint64_t sym_id);

/*-----------------------------------------------------------------------------
 * Domain-Specific Tagged Domain Declaration Macro
 *---------------------------------------------------------------------------*/

#define BENNET_TAGGED_DOMAIN_DECL(dom)                                                   \
  bool bennet_tagged_domain_is_bottom_##dom(bennet_tagged_domain* d);                    \
  bool bennet_tagged_domain_is_top_##dom(bennet_tagged_domain* d);                       \
  bennet_tagged_domain bennet_tagged_domain_copy_##dom(bennet_tagged_domain* d);         \
  bennet_tagged_domain bennet_tagged_domain_top_##dom(cn_base_type* type);               \
  bennet_tagged_domain bennet_tagged_domain_bottom_##dom(cn_base_type* type);            \
  bennet_tagged_domain bennet_tagged_domain_meet_##dom(                                  \
      bennet_tagged_domain* d1, bennet_tagged_domain* d2);                               \
  bennet_tagged_domain bennet_tagged_domain_join_##dom(                                  \
      bennet_tagged_domain* d1, bennet_tagged_domain* d2);

/*-----------------------------------------------------------------------------
 * Domain-Specific Absint State Declaration Macro
 *---------------------------------------------------------------------------*/

#define BENNET_ABSINT_STATE_DECL(dom)                                                    \
  bennet_absint_state* bennet_absint_state_copy_##dom(bennet_absint_state* state);       \
  bennet_tagged_domain bennet_absint_state_get_##dom(                                    \
      bennet_absint_state* state, bennet_absint_sym sym, cn_base_type* type);            \
  bennet_absint_state* bennet_absint_state_set_##dom(                                    \
      bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain);   \
  bennet_absint_state* bennet_absint_state_meet_##dom(                                   \
      bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain);   \
  bool bennet_absint_state_is_bottom_##dom(bennet_absint_state* state);

/*-----------------------------------------------------------------------------
 * Dispatcher Macros
 *---------------------------------------------------------------------------*/

#define bennet_tagged_domain_is_bottom(dom, d) bennet_tagged_domain_is_bottom_##dom(d)
#define bennet_tagged_domain_is_top(dom, d)    bennet_tagged_domain_is_top_##dom(d)
#define bennet_tagged_domain_copy(dom, d)      bennet_tagged_domain_copy_##dom(d)
#define bennet_tagged_domain_top(dom, type)    bennet_tagged_domain_top_##dom(type)
#define bennet_tagged_domain_bottom(dom, type) bennet_tagged_domain_bottom_##dom(type)
#define bennet_tagged_domain_meet(dom, d1, d2) bennet_tagged_domain_meet_##dom(d1, d2)
#define bennet_tagged_domain_join(dom, d1, d2) bennet_tagged_domain_join_##dom(d1, d2)

#define bennet_absint_state_copy(dom, state) bennet_absint_state_copy_##dom(state)
#define bennet_absint_state_get(dom, state, sym, type)                                   \
  bennet_absint_state_get_##dom(state, sym, type)
#define bennet_absint_state_set(dom, state, sym, domain)                                 \
  bennet_absint_state_set_##dom(state, sym, domain)
#define bennet_absint_state_meet(dom, state, sym, domain)                                \
  bennet_absint_state_meet_##dom(state, sym, domain)
#define bennet_absint_state_is_bottom(dom, state)                                        \
  bennet_absint_state_is_bottom_##dom(state)

/*-----------------------------------------------------------------------------
 * Absint State Implementation Macro
 *
 * Each domain calls this once in its .c file to generate the boilerplate
 * absint_state functions that delegate to domain-specific tagged_domain ops.
 *---------------------------------------------------------------------------*/

#define BENNET_ABSINT_STATE_IMPL(dom)                                                    \
  bennet_absint_state* bennet_absint_state_copy_##dom(bennet_absint_state* state) {      \
    if (!state)                                                                          \
      return bennet_absint_state_create();                                               \
                                                                                         \
    bennet_absint_state* copy = std_malloc(sizeof(bennet_absint_state));                 \
    assert(copy);                                                                        \
    copy->count = state->count;                                                          \
    copy->capacity = state->capacity;                                                    \
    copy->entries = std_malloc(copy->capacity * sizeof(bennet_absint_state_entry));      \
    assert(copy->entries);                                                               \
                                                                                         \
    for (size_t i = 0; i < state->count; i++) {                                          \
      copy->entries[i].id = state->entries[i].id;                                        \
      copy->entries[i].domain =                                                          \
          bennet_tagged_domain_copy_##dom(&state->entries[i].domain);                    \
    }                                                                                    \
                                                                                         \
    return copy;                                                                         \
  }                                                                                      \
                                                                                         \
  bennet_tagged_domain bennet_absint_state_get_##dom(                                    \
      bennet_absint_state* state, bennet_absint_sym sym, cn_base_type* type) {           \
    if (!state) {                                                                        \
      return bennet_tagged_domain_top_##dom(type);                                       \
    }                                                                                    \
                                                                                         \
    int idx = bennet_absint_state_find(state, sym.id);                                   \
    if (idx < 0) {                                                                       \
      return bennet_tagged_domain_top_##dom(type);                                       \
    }                                                                                    \
                                                                                         \
    return bennet_tagged_domain_copy_##dom(&state->entries[idx].domain);                 \
  }                                                                                      \
                                                                                         \
  bennet_absint_state* bennet_absint_state_set_##dom(                                    \
      bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain) {  \
    bennet_absint_state* result = bennet_absint_state_copy_##dom(state);                 \
                                                                                         \
    int idx = bennet_absint_state_find(result, sym.id);                                  \
    if (idx >= 0) {                                                                      \
      result->entries[idx].domain = domain;                                              \
    } else {                                                                             \
      if (result->count >= result->capacity) {                                           \
        size_t new_capacity = result->capacity * 2;                                      \
        bennet_absint_state_entry* new_entries =                                         \
            std_malloc(new_capacity * sizeof(bennet_absint_state_entry));                \
        assert(new_entries);                                                             \
        memcpy(new_entries,                                                              \
            result->entries,                                                             \
            result->count * sizeof(bennet_absint_state_entry));                          \
        result->entries = new_entries;                                                   \
        result->capacity = new_capacity;                                                 \
      }                                                                                  \
      result->entries[result->count].id = sym.id;                                        \
      result->entries[result->count].domain = domain;                                    \
      result->count++;                                                                   \
    }                                                                                    \
                                                                                         \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bennet_absint_state* bennet_absint_state_meet_##dom(                                   \
      bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain) {  \
    bennet_absint_state* result = bennet_absint_state_copy_##dom(state);                 \
                                                                                         \
    int idx = bennet_absint_state_find(result, sym.id);                                  \
    if (idx >= 0) {                                                                      \
      result->entries[idx].domain =                                                      \
          bennet_tagged_domain_meet_##dom(&result->entries[idx].domain, &domain);        \
    } else {                                                                             \
      if (result->count >= result->capacity) {                                           \
        size_t new_capacity = result->capacity * 2;                                      \
        bennet_absint_state_entry* new_entries =                                         \
            std_malloc(new_capacity * sizeof(bennet_absint_state_entry));                \
        assert(new_entries);                                                             \
        memcpy(new_entries,                                                              \
            result->entries,                                                             \
            result->count * sizeof(bennet_absint_state_entry));                          \
        result->entries = new_entries;                                                   \
        result->capacity = new_capacity;                                                 \
      }                                                                                  \
      result->entries[result->count].id = sym.id;                                        \
      result->entries[result->count].domain = domain;                                    \
      result->count++;                                                                   \
    }                                                                                    \
                                                                                         \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  bool bennet_absint_state_is_bottom_##dom(bennet_absint_state* state) {                 \
    if (!state)                                                                          \
      return false;                                                                      \
    for (size_t i = 0; i < state->count; i++) {                                          \
      if (bennet_tagged_domain_is_bottom_##dom(&state->entries[i].domain)) {             \
        return true;                                                                     \
      }                                                                                  \
    }                                                                                    \
    return false;                                                                        \
  }

/*-----------------------------------------------------------------------------
 * Instantiate Declarations for All Domains
 *---------------------------------------------------------------------------*/

BENNET_TAGGED_DOMAIN_DECL(wint)
BENNET_ABSINT_STATE_DECL(wint)

BENNET_TAGGED_DOMAIN_DECL(tnum)
BENNET_ABSINT_STATE_DECL(tnum)

BENNET_TAGGED_DOMAIN_DECL(ownership)
BENNET_ABSINT_STATE_DECL(ownership)

BENNET_TAGGED_DOMAIN_DECL(congr)
BENNET_ABSINT_STATE_DECL(congr)

#ifdef __cplusplus
}
#endif

#endif /* BENNET_ABSINT_H */
