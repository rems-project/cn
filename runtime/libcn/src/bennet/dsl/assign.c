#include <string.h>

#include <bennet/dsl/assign.h>
#include <bennet/state/failure.h>

#define BENNET_ASSIGN_IMPL(pointer_ty)                                                   \
  bool bennet_assign_##pointer_ty(void* id,                                              \
      cn_pointer* base_ptr,                                                              \
      cn_pointer* addr,                                                                  \
      void* value,                                                                       \
      size_t bytes,                                                                      \
      const void* vars[]) {                                                              \
    bennet_domain(pointer_ty) * domain;                                                  \
                                                                                         \
    void* raw_base_ptr = convert_from_cn_pointer(base_ptr);                              \
    void* raw_addr = convert_from_cn_pointer(addr);                                      \
    if (raw_base_ptr == NULL || !bennet_alloc_check(raw_addr, bytes)) {                  \
      bennet_failure_set_failure_type(BENNET_FAILURE_ASSIGN);                            \
                                                                                         \
      domain =                                                                           \
          bennet_domain_from_assignment_##pointer_ty(raw_base_ptr, raw_addr, bytes);     \
      bennet_failure_blame_domain(pointer_ty, id, domain);                               \
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
