#ifndef BENNET_EXP_ASSIGN_H
#define BENNET_EXP_ASSIGN_H

#include <stdbool.h>

#include <bennet-exp/state/alloc.h>
#include <cn-executable/utils.h>

bool bennet_assign(void* id,
    cn_pointer* base_ptr,
    cn_pointer* addr,
    void* value,
    size_t bytes,
    const void* vars[]);

#endif  // BENNET_EXP_ASSIGN_H
