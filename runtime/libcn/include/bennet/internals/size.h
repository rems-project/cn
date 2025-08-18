#ifndef BENNET_SIZE_H
#define BENNET_SIZE_H

#include <sys/time.h>

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

enum bennet_sizing_strategy {
  BENNET_SIZE_UNIFORM = 0,
  BENNET_SIZE_QUARTILE = 1,
  BENNET_SIZE_QUICKCHECK = 2
};

size_t bennet_get_size(void);
void bennet_set_size(size_t sz);

size_t bennet_get_max_size(void);
void bennet_set_max_size(size_t sz);

uint16_t bennet_get_depth();
void bennet_set_depth(uint16_t);
uint16_t bennet_max_depth();
void bennet_set_max_depth(uint16_t msd);
void bennet_increment_depth();
void bennet_decrement_depth();

void bennet_set_size_split_backtracks_allowed(uint16_t allowed);
uint16_t bennet_get_size_split_backtracks_allowed();

void bennet_set_input_timeout(uint64_t seconds);
uint64_t bennet_get_input_timeout(void);

void bennet_set_input_timer(uint64_t time);
uint64_t bennet_get_input_timer(void);

uint64_t bennet_get_milliseconds(void);
uint64_t bennet_get_microseconds(void);

int64_t timediff_timeval(struct timeval *early, struct timeval *late);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_SIZE_H
