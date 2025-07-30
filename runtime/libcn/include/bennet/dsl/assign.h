#ifndef BENNET_ASSIGN_H
#define BENNET_ASSIGN_H

#include <stdbool.h>

#include <bennet/state/alloc.h>
#include <cn-executable/utils.h>

bool bennet_assign(void* id,
    cn_pointer* base_ptr,
    cn_pointer* addr,
    void* value,
    size_t bytes,
    const void* vars[]);

#endif  // BENNET_ASSIGN_H
