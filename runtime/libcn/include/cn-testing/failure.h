#ifndef CN_GEN_BACKTRACK_H
#define CN_GEN_BACKTRACK_H

#include <stdint.h>
#include <stdlib.h>

enum cn_gen_failure_type {
  CN_GEN_BACKTRACK_NONE,
  CN_GEN_BACKTRACK_ASSERT,
  CN_GEN_BACKTRACK_ALLOC,
  CN_GEN_BACKTRACK_DEPTH
};

enum cn_gen_failure_type cn_gen_failure_get_failure_type(void);
void cn_gen_failure_set_failure_type(enum cn_gen_failure_type type);

void cn_gen_failure_reset(void);

void cn_gen_failure_blame(char* varname);

void cn_gen_failure_blame_many(char* toAdd[]);

int cn_gen_failure_is_blamed(char* varname);

/**
 * @brief Remaps a relevant variable
 *
 * @param from A NULL-terminated variable name
 * @param to A NULL-terminated variable name to replace `from` with
 * @return int Was the remapping successful?
 */
int cn_gen_failure_remap_blamed(char* from, char* to);

/**
 * @brief Remaps multiple relevant variables
 *
 * @param from A NULL-terminated list of `char*`
 * @param to A NULL-terminated list of `char*` of the same length as `from`
 * @return int How many remappings were successful?
 */
int cn_gen_failure_remap_blamed_many(char* from[], char* to[]);

void cn_gen_failure_set_allocation_needed(size_t sz);

size_t cn_gen_failure_get_allocation_needed();

#endif  // CN_GEN_BACKTRACK_H
