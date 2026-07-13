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

/**
 * Persistent association list. Cells are immutable after creation, so states
 * share tails: set/meet cons a fresh head whose binding shadows older ones
 * for the same symbol, and copy is identity. A cell with has_entry == false
 * is the empty state.
 */
struct bennet_absint_state {
  bool has_entry;
  bennet_absint_state_entry entry;
  struct bennet_absint_state* next;
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

/** Prepend a binding, shadowing any older binding for the same symbol */
bennet_absint_state* bennet_absint_state_cons(
    bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain);

/** Newest binding for a symbol id, or NULL if unbound */
bennet_tagged_domain* bennet_absint_state_lookup(bennet_absint_state* state, uint64_t id);

/**
 * Iterator over the newest binding per symbol (skips shadowed cells and the
 * empty sentinel).
 */
typedef struct {
  bennet_absint_state* head;
  bennet_absint_state* cell;
} bennet_absint_state_iter;

bennet_absint_state_iter bennet_absint_state_iter_begin(bennet_absint_state* state);
bool bennet_absint_state_iter_done(const bennet_absint_state_iter* it);
void bennet_absint_state_iter_next(bennet_absint_state_iter* it);
bennet_tagged_domain* bennet_absint_state_iter_domain(const bennet_absint_state_iter* it);

/** Check if a term contains a specific symbol */
bool term_contains_sym(cn_term* term, uint64_t sym_id);

/** Collect the syms of a term into a fixed-size buffer (left-first,
 *  per-occurrence, truncated at max_syms; ITE conditions are not visited).
 *  Returns the number of syms written. */
int bennet_absint_term_collect_syms(cn_term* term, bennet_absint_sym* syms, int max_syms);

/** Lower a cn_base_type to (width, signedness). CN_BASE_BITS uses its own
 *  size/signedness; CN_BASE_LOC and anything else lower to unsigned 64. */
void bennet_absint_type_info(cn_base_type* type, int* width, bool* is_signed);

/** Fuel for the assume-side local-iteration loop (re-run refinement while
 *  the state changes; pointer equality on the persistent cons-list is the
 *  "unchanged" test). The default of 1 is single-pass, deliberately
 *  diverging from the OCaml engine's get_local_iterations() = 10; set via
 *  the `--dynamic-local-iterations` runtime flag. */
void bennet_set_dynamic_local_iterations(int fuel);
int bennet_get_dynamic_local_iterations(void);

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
    /* Cells are immutable, so sharing the list is a valid copy. */                      \
    return state;                                                                        \
  }                                                                                      \
                                                                                         \
  bennet_tagged_domain bennet_absint_state_get_##dom(                                    \
      bennet_absint_state* state, bennet_absint_sym sym, cn_base_type* type) {           \
    bennet_tagged_domain* found = bennet_absint_state_lookup(state, sym.id);             \
    if (!found) {                                                                        \
      return bennet_tagged_domain_top_##dom(type);                                       \
    }                                                                                    \
                                                                                         \
    return bennet_tagged_domain_copy_##dom(found);                                       \
  }                                                                                      \
                                                                                         \
  bennet_absint_state* bennet_absint_state_set_##dom(                                    \
      bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain) {  \
    return bennet_absint_state_cons(state, sym, domain);                                 \
  }                                                                                      \
                                                                                         \
  bennet_absint_state* bennet_absint_state_meet_##dom(                                   \
      bennet_absint_state* state, bennet_absint_sym sym, bennet_tagged_domain domain) {  \
    bennet_tagged_domain* found = bennet_absint_state_lookup(state, sym.id);             \
    if (!found) {                                                                        \
      return bennet_absint_state_cons(state, sym, domain);                               \
    }                                                                                    \
                                                                                         \
    return bennet_absint_state_cons(                                                     \
        state, sym, bennet_tagged_domain_meet_##dom(found, &domain));                    \
  }                                                                                      \
                                                                                         \
  bool bennet_absint_state_is_bottom_##dom(bennet_absint_state* state) {                 \
    for (bennet_absint_state_iter it = bennet_absint_state_iter_begin(state);            \
        !bennet_absint_state_iter_done(&it);                                             \
        bennet_absint_state_iter_next(&it)) {                                            \
      if (bennet_tagged_domain_is_bottom_##dom(bennet_absint_state_iter_domain(&it))) {  \
        return true;                                                                     \
      }                                                                                  \
    }                                                                                    \
    return false;                                                                        \
  }

/*-----------------------------------------------------------------------------
 * Tagged-Domain Conversion Generator
 *
 * Generates the static <dom>_from_tagged / <dom>_to_tagged converters between
 * the void*-erased per-C-type domain structs and the domain's 64-bit generic
 * form. Dispatch is on (signedness, width) with the 64-bit arm as the
 * default. The per-domain payload mapping is supplied via two hook macros:
 *
 *   LOAD(cty, ucty)  - runs with `d` (tagged in) and `result` (generic out)
 *                      in scope; cast d->domain to the per-type struct and
 *                      fill result's flags and payload words
 *   STORE(cty, ucty) - runs with `g` (generic in), `width`, and `result`
 *                      (tagged out) in scope; std_malloc the per-type struct,
 *                      fill it from g, and set result.domain
 *---------------------------------------------------------------------------*/

#define BENNET_ABSINT_TAGGED_CONVERT_IMPL(dom, generic_t, LOAD, STORE)                   \
  static generic_t dom##_from_tagged(bennet_tagged_domain* d) {                          \
    generic_t result = {0};                                                              \
    if (!d || !d->type || !d->domain) {                                                  \
      result.is_top = true;                                                              \
      result.width = 64;                                                                 \
      return result;                                                                     \
    }                                                                                    \
                                                                                         \
    bennet_absint_type_info(d->type, &result.width, &result.is_signed);                  \
                                                                                         \
    if (result.is_signed) {                                                              \
      switch (result.width) {                                                            \
        case 8:                                                                          \
          LOAD(int8_t, uint8_t);                                                         \
          break;                                                                         \
        case 16:                                                                         \
          LOAD(int16_t, uint16_t);                                                       \
          break;                                                                         \
        case 32:                                                                         \
          LOAD(int32_t, uint32_t);                                                       \
          break;                                                                         \
        case 64:                                                                         \
        default:                                                                         \
          LOAD(int64_t, uint64_t);                                                       \
          break;                                                                         \
      }                                                                                  \
    } else {                                                                             \
      switch (result.width) {                                                            \
        case 8:                                                                          \
          LOAD(uint8_t, uint8_t);                                                        \
          break;                                                                         \
        case 16:                                                                         \
          LOAD(uint16_t, uint16_t);                                                      \
          break;                                                                         \
        case 32:                                                                         \
          LOAD(uint32_t, uint32_t);                                                      \
          break;                                                                         \
        case 64:                                                                         \
        default:                                                                         \
          LOAD(uint64_t, uint64_t);                                                      \
          break;                                                                         \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    return result;                                                                       \
  }                                                                                      \
                                                                                         \
  static bennet_tagged_domain dom##_to_tagged(generic_t* g, cn_base_type* type) {        \
    bennet_tagged_domain result;                                                         \
    result.type = type;                                                                  \
                                                                                         \
    int width;                                                                           \
    bool is_signed;                                                                      \
    bennet_absint_type_info(type, &width, &is_signed);                                   \
                                                                                         \
    if (is_signed) {                                                                     \
      switch (width) {                                                                   \
        case 8:                                                                          \
          STORE(int8_t, uint8_t);                                                        \
          break;                                                                         \
        case 16:                                                                         \
          STORE(int16_t, uint16_t);                                                      \
          break;                                                                         \
        case 32:                                                                         \
          STORE(int32_t, uint32_t);                                                      \
          break;                                                                         \
        case 64:                                                                         \
        default:                                                                         \
          STORE(int64_t, uint64_t);                                                      \
          break;                                                                         \
      }                                                                                  \
    } else {                                                                             \
      switch (width) {                                                                   \
        case 8:                                                                          \
          STORE(uint8_t, uint8_t);                                                       \
          break;                                                                         \
        case 16:                                                                         \
          STORE(uint16_t, uint16_t);                                                     \
          break;                                                                         \
        case 32:                                                                         \
          STORE(uint32_t, uint32_t);                                                     \
          break;                                                                         \
        case 64:                                                                         \
        default:                                                                         \
          STORE(uint64_t, uint64_t);                                                     \
          break;                                                                         \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    return result;                                                                       \
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
