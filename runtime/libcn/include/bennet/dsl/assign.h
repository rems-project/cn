#ifndef BENNET_ASSIGN_H
#define BENNET_ASSIGN_H

#include <bennet/internals/absint.h>
#include <bennet/state/alloc.h>
#include <cn-executable/utils.h>
#include <cn-smt/terms.h>

#define bennet_assign(pointer_ty,                                                        \
    id,                                                                                  \
    base_ptr,                                                                            \
    addr,                                                                                \
    value,                                                                               \
    bytes,                                                                               \
    vars,                                                                                \
    addr_term,                                                                           \
    num_other_vars,                                                                      \
    other_var_ids,                                                                       \
    other_var_syms)                                                                      \
  (bennet_assign_##pointer_ty(id,                                                        \
      base_ptr,                                                                          \
      addr,                                                                              \
      value,                                                                             \
      bytes,                                                                             \
      vars,                                                                              \
      addr_term,                                                                         \
      num_other_vars,                                                                    \
      other_var_ids,                                                                     \
      other_var_syms))

#define BENNET_ASSIGN_DECL(pointer_ty)                                                   \
  bool bennet_assign_##pointer_ty(void* id,                                              \
      cn_pointer* base_ptr,                                                              \
      cn_pointer* addr,                                                                  \
      void* value,                                                                       \
      size_t bytes,                                                                      \
      const void* vars[],                                                                \
      cn_term* addr_term,                                                                \
      size_t num_other_vars,                                                             \
      const void* other_var_ids[],                                                       \
      const bennet_absint_sym other_var_syms[]);

void bennet_assign_backward_blame(cn_term* addr_term,
    size_t num_other_vars,
    const void* other_var_ids[],
    const bennet_absint_sym other_var_syms[],
    size_t bytes);

BENNET_ASSIGN_DECL(int8_t)
BENNET_ASSIGN_DECL(uint8_t)
BENNET_ASSIGN_DECL(int16_t)
BENNET_ASSIGN_DECL(uint16_t)
BENNET_ASSIGN_DECL(int32_t)
BENNET_ASSIGN_DECL(uint32_t)
BENNET_ASSIGN_DECL(int64_t)
BENNET_ASSIGN_DECL(uint64_t)
BENNET_ASSIGN_DECL(uintptr_t)

typedef enum {
  BENNET_DYNAMIC_ABSINT_ASSIGN_DISABLED = 0,
  BENNET_DYNAMIC_ABSINT_ASSIGN_ALSO = 1,
  BENNET_DYNAMIC_ABSINT_ASSIGN_ONLY = 2
} bennet_dynamic_absint_assign_mode;

void bennet_set_dynamic_absint_assign(bennet_dynamic_absint_assign_mode mode);
bennet_dynamic_absint_assign_mode bennet_get_dynamic_absint_assign(void);

#endif  // BENNET_ASSIGN_H
