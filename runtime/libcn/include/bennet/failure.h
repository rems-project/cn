#ifndef BENNET_FAILURE_H
#define BENNET_FAILURE_H

#include <stdint.h>
#include <stdlib.h>

enum bennet_failure_type {
  BENNET_BACKTRACK_NONE,
  BENNET_BACKTRACK_ASSERT,
  BENNET_BACKTRACK_ALLOC,
  BENNET_BACKTRACK_DEPTH
};

enum bennet_failure_type bennet_failure_get_failure_type(void);
void bennet_failure_set_failure_type(enum bennet_failure_type type);

void bennet_failure_reset(void);

void bennet_failure_blame(char* varname);

void bennet_failure_blame_many(char* toAdd[]);

int bennet_failure_is_blamed(char* varname);

/**
 * @brief Remaps a relevant variable
 *
 * @param from A NULL-terminated variable name
 * @param to A NULL-terminated variable name to replace `from` with
 * @return int Was the remapping successful?
 */
int bennet_failure_remap_blamed(char* from, char* to);

/**
 * @brief Remaps multiple relevant variables
 *
 * @param from A NULL-terminated list of `char*`
 * @param to A NULL-terminated list of `char*` of the same length as `from`
 * @return int How many remappings were successful?
 */
int bennet_failure_remap_blamed_many(char* from[], char* to[]);

void bennet_failure_set_allocation_needed(size_t sz);

size_t bennet_failure_get_allocation_needed();

#endif  // BENNET_FAILURE_H
