#ifndef BENNET_FAILURE_H
#define BENNET_FAILURE_H

#include <stdint.h>
#include <stdlib.h>

#include <bennet/internals/domain.h>

enum bennet_failure_type {
  BENNET_FAILURE_NONE,
  BENNET_FAILURE_ASSERT,
  BENNET_FAILURE_ASSIGN,
  BENNET_FAILURE_DEPTH,
  BENNET_FAILURE_TIMEOUT
};

bool bennet_failure_is_young(void);
void bennet_failure_mark_young(void);
void bennet_failure_mark_old(void);

enum bennet_failure_type bennet_failure_get_failure_type(void);
void bennet_failure_set_failure_type(enum bennet_failure_type type);

void bennet_failure_reset(void);

void bennet_failure_blame(const void* id);

#define bennet_failure_blame_domain(ty, id, cs) (bennet_failure_blame_domain_##ty(id, cs))
#define bennet_failure_get_domain(ty, id)       (bennet_failure_get_domain_##ty(id))

#define BENNET_DOMAIN_FAILURE_DECL(ty)                                                   \
  void bennet_failure_blame_domain_##ty(const void* id, bennet_domain(ty) * domain);     \
  bennet_domain(ty) * bennet_failure_get_domain_##ty(const void* id);

BENNET_DOMAIN_FAILURE_DECL(int8_t)
BENNET_DOMAIN_FAILURE_DECL(uint8_t)
BENNET_DOMAIN_FAILURE_DECL(int16_t)
BENNET_DOMAIN_FAILURE_DECL(uint16_t)
BENNET_DOMAIN_FAILURE_DECL(int32_t)
BENNET_DOMAIN_FAILURE_DECL(uint32_t)
BENNET_DOMAIN_FAILURE_DECL(int64_t)
BENNET_DOMAIN_FAILURE_DECL(uint64_t)
BENNET_DOMAIN_FAILURE_DECL(uintptr_t)

int bennet_failure_remove_blame(const void* id);

void bennet_failure_blame_many(const void* toAdd[]);

bool bennet_failure_is_blamed(const void* id);

/**
 * @brief Remaps a relevant variable
 *
 * @param from A NULL-terminated variable name
 * @param to A NULL-terminated variable name to replace `from` with
 * @return int Was the remapping successful?
 */
int bennet_failure_remap_blamed(const void* from, const void* to);

/**
 * @brief Remaps multiple relevant variables
 *
 * @param from A NULL-terminated list of `char*`
 * @param to A NULL-terminated list of `char*` of the same length as `from`
 * @return int How many remappings were successful?
 */
int bennet_failure_remap_blamed_many(const char* from[], const char* to[]);

#endif  // BENNET_FAILURE_H
