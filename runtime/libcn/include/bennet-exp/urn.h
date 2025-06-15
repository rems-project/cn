#ifndef BENNET_EXP_URN_H
#define BENNET_EXP_URN_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct bennet_int_tree {
  uint64_t weight;
  uint64_t value;

  struct bennet_int_tree* left;
  struct bennet_int_tree* right;
};

struct bennet_int_urn {
  uint8_t size;
  struct bennet_int_tree* tree;
};

struct bennet_int_urn* urn_from_array(uint64_t elems[], uint8_t len);

void urn_insert(struct bennet_int_urn* urn, uint64_t weight, uint64_t value);

uint64_t urn_remove(struct bennet_int_urn* urn);

void urn_free(struct bennet_int_urn* urn);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_EXP_URN_H
