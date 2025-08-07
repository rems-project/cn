#ifndef BENNET_ASSIGN_H
#define BENNET_ASSIGN_H

#include <stdbool.h>

#include <bennet/state/alloc.h>
#include <cn-executable/utils.h>

#define bennet_assign(pointer_ty, id, base_ptr, addr, value, bytes, vars)                \
  (bennet_assign_##pointer_ty(id, base_ptr, addr, value, bytes, vars))

#define BENNET_ASSIGN_DECL(pointer_ty)                                                   \
  bool bennet_assign_##pointer_ty(void* id,                                              \
      cn_pointer* base_ptr,                                                              \
      cn_pointer* addr,                                                                  \
      void* value,                                                                       \
      size_t bytes,                                                                      \
      const void* vars[]);

BENNET_ASSIGN_DECL(int8_t)
BENNET_ASSIGN_DECL(uint8_t)
BENNET_ASSIGN_DECL(int16_t)
BENNET_ASSIGN_DECL(uint16_t)
BENNET_ASSIGN_DECL(int32_t)
BENNET_ASSIGN_DECL(uint32_t)
BENNET_ASSIGN_DECL(int64_t)
BENNET_ASSIGN_DECL(uint64_t)
BENNET_ASSIGN_DECL(uintptr_t)

#endif  // BENNET_ASSIGN_H
