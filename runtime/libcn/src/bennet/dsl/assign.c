#include <string.h>

#include <bennet/dsl/assign.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/state/failure.h>
#include <cn-smt/memory/std_alloc.h>

static bennet_dynamic_absint_assign_mode dynamic_absint_assign =
    BENNET_DYNAMIC_ABSINT_ASSIGN_DISABLED;

void bennet_set_dynamic_absint_assign(bennet_dynamic_absint_assign_mode mode) {
  dynamic_absint_assign = mode;
}

bennet_dynamic_absint_assign_mode bennet_get_dynamic_absint_assign(void) {
  return dynamic_absint_assign;
}

#define BENNET_ASSIGN_IMPL(pointer_ty)                                                   \
  bool bennet_assign_##pointer_ty(void* id,                                              \
      cn_pointer* base_ptr,                                                              \
      cn_pointer* addr,                                                                  \
      void* value,                                                                       \
      size_t bytes,                                                                      \
      const void* vars[],                                                                \
      cn_term* addr_term,                                                                \
      size_t num_other_vars,                                                             \
      const void* other_var_ids[],                                                       \
      const bennet_absint_sym other_var_syms[]) {                                        \
    bennet_domain(pointer_ty) * domain;                                                  \
                                                                                         \
    void* raw_base_ptr = convert_from_cn_pointer(base_ptr);                              \
    void* raw_addr = convert_from_cn_pointer(addr);                                      \
    if (raw_base_ptr == NULL || !bennet_alloc_check(raw_addr, bytes)) {                  \
      domain =                                                                           \
          bennet_domain_from_assignment_##pointer_ty(raw_base_ptr, raw_addr, bytes);     \
                                                                                         \
      if (bennet_domain_is_bottom(pointer_ty, domain)) {                                 \
        /* Assignment is impossible regardless of pointer. also = plain     */           \
        /* blame plus backward-absint domains (the blame merge upgrades     */           \
        /* duplicate ids); only = backward blame alone.                     */           \
        bennet_dynamic_absint_assign_mode _mode = bennet_get_dynamic_absint_assign();    \
        if (_mode != BENNET_DYNAMIC_ABSINT_ASSIGN_ONLY) {                                \
          /* Blame all vars that determine bytes (e.g., size) */                         \
          bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                        \
          bennet_failure_blame_many(vars);                                               \
        }                                                                                \
        if (_mode != BENNET_DYNAMIC_ABSINT_ASSIGN_DISABLED) {                            \
          /* Backward absint: blame non-pointer vars with domains */                     \
          bennet_assign_backward_blame(                                                  \
              addr_term, num_other_vars, other_var_ids, other_var_syms, bytes);          \
        }                                                                                \
      } else {                                                                           \
        /* Pointer is wrong but a valid pointer exists — blame the pointer */            \
        bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);                          \
        bennet_failure_blame_domain(pointer_ty, id, domain);                             \
      }                                                                                  \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    if (!bennet_ownership_check(raw_addr, bytes)) {                                      \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                            \
                                                                                         \
      bennet_failure_blame_many(vars);                                                   \
                                                                                         \
      return true;                                                                       \
    }                                                                                    \
                                                                                         \
    memcpy(raw_addr, value, bytes);                                                      \
    bennet_ownership_update(raw_addr, bytes);                                            \
                                                                                         \
    return false;                                                                        \
  }

BENNET_ASSIGN_IMPL(int8_t)
BENNET_ASSIGN_IMPL(uint8_t)
BENNET_ASSIGN_IMPL(int16_t)
BENNET_ASSIGN_IMPL(uint16_t)
BENNET_ASSIGN_IMPL(int32_t)
BENNET_ASSIGN_IMPL(uint32_t)
BENNET_ASSIGN_IMPL(int64_t)
BENNET_ASSIGN_IMPL(uint64_t)
BENNET_ASSIGN_IMPL(uintptr_t)

void bennet_assign_backward_blame(cn_term* addr_term,
    size_t num_other_vars,
    const void* other_var_ids[],
    const bennet_absint_sym other_var_syms[],
    size_t bytes) {
  /* Build output ownership domain: {before=0, after=bytes} */
  bennet_domain_ownership(uintptr_t)* out =
      bennet_domain_ownership_of(uintptr_t, 0, bytes);

  /* Initialize absint state with all vars mapped to top ownership */
  bennet_absint_state* state = bennet_absint_state_create();

  /* Run backward propagation through address term */
  bennet_absint_state* result =
      bennet_ownership_backward_propagate_to_syms(addr_term, out, state);
  std_free(out);

  /* Set failure type and blame each non-pointer variable */
  cn_base_type loc_bt = {.tag = CN_BASE_LOC};
  bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);

  for (size_t i = 0; i < num_other_vars; i++) {
    bennet_tagged_domain dom =
        bennet_absint_state_get_ownership(result, other_var_syms[i], &loc_bt);
    bennet_domain_ownership(uintptr_t)* own =
        (bennet_domain_ownership(uintptr_t)*)dom.domain;

    /* If we got a non-trivial domain from backward propagation, lift it into
     * the product and blame with it; otherwise fall back to a plain blame
     * (no domain constraint). */
    if (own && !bennet_domain_ownership_is_top_uintptr_t(own)) {
      bennet_domain(uintptr_t)* blamed = bennet_domain_from_ownership_uintptr_t(own);
      bennet_failure_blame_domain_uintptr_t(other_var_ids[i], blamed);
      std_free(blamed); /* blame copies the product */
    } else {
      bennet_failure_blame(other_var_ids[i]);
    }
  }

  bennet_absint_state_free(state);
  bennet_absint_state_free(result);
}
