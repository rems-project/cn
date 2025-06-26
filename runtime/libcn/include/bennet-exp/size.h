#ifndef BENNET_EXP_SIZE_H
#define BENNET_EXP_SIZE_H

#include <stdint.h>
#include <stdlib.h>

size_t bennet_get_size(void);
void bennet_set_size(size_t sz);

size_t bennet_get_max_size(void);
void bennet_set_max_size(size_t sz);

uint16_t bennet_depth(void);
uint16_t bennet_max_depth(void);
void bennet_set_max_depth(uint16_t msd);
void bennet_increment_depth(void);
void bennet_decrement_depth(void);

void bennet_set_size_split_backtracks_allowed(uint16_t allowed);
uint16_t bennet_get_size_split_backtracks_allowed(void);

void bennet_set_input_timeout(uint64_t seconds);
uint64_t bennet_get_input_timeout(void);

void bennet_set_input_timer(uint64_t time);
uint64_t bennet_get_input_timer(void);

uint64_t bennet_get_milliseconds(void);
uint64_t bennet_get_microseconds(void);

int64_t timediff_timeval(struct timeval *early, struct timeval *late);

#endif  // BENNET_EXP_SIZE_H
