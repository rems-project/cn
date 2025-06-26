#include <assert.h>
#include <stdbool.h>

#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>

static size_t global_max_size = 25;

size_t bennet_get_max_size(void) {
  return global_max_size;
}

void bennet_set_max_size(size_t sz) {
  assert(sz != 0);

  global_max_size = sz;
}

static size_t global_size = 20;

size_t bennet_get_size(void) {
  return global_size;
}

void bennet_set_size(size_t sz) {
  assert(sz != 0);
  assert(sz <= global_max_size);

  global_size = sz;
}

static uint16_t stack_depth = 0;
static uint16_t max_stack_depth = UINT8_MAX;

uint16_t bennet_get_depth(void) {
  return stack_depth;
}

void bennet_set_depth(uint16_t depth) {
  stack_depth = depth;
}

uint16_t bennet_max_depth(void) {
  return max_stack_depth;
}

void bennet_set_max_depth(uint16_t msd) {
  max_stack_depth = msd;
}

void bennet_increment_depth(void) {
  stack_depth++;
}

void bennet_decrement_depth(void) {
  stack_depth--;
}

static uint16_t size_split_backtracks_allowed = 0;

void bennet_set_size_split_backtracks_allowed(uint16_t allowed) {
  size_split_backtracks_allowed = allowed;
}

uint16_t bennet_get_size_split_backtracks_allowed(void) {
  return size_split_backtracks_allowed;
}

static uint64_t timeout = 0;

void bennet_set_input_timeout(uint64_t ms) {
  timeout = ms;
}

uint64_t bennet_get_input_timeout(void) {
  return timeout;
}

static uint64_t timer = 0;

void bennet_set_input_timer(uint64_t time) {
  timer = time;
}

uint64_t bennet_get_input_timer(void) {
  return timer;
}

#if defined(__unix__) || (defined(__APPLE__) && defined(__MACH__))
  #include <sys/time.h>
#elif defined(_WIN32) || defined(_WIN64)
  #include <Windows.h>

/// Taken from https://stackoverflow.com/questions/10905892/equivalent-of-gettimeofday-for-windows
int gettimeofday(struct timeval *tp, struct timezone *tzp) {
  // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
  // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
  // until 00:00:00 January 1, 1970
  static const uint64_t EPOCH = ((uint64_t)116444736000000000ULL);

  SYSTEMTIME system_time;
  FILETIME file_time;
  uint64_t time;

  GetSystemTime(&system_time);
  SystemTimeToFileTime(&system_time, &file_time);
  time = ((uint64_t)file_time.dwLowDateTime);
  time += ((uint64_t)file_time.dwHighDateTime) << 32;

  tp->tv_sec = (long)((time - EPOCH) / 10000000L);
  tp->tv_usec = (long)(system_time.wMilliseconds * 1000);
  return 0;
}
#endif

uint64_t bennet_get_milliseconds(void) {
  struct timeval tv;

  gettimeofday(&tv, NULL);
  return (((uint64_t)tv.tv_sec) * 1000) + (tv.tv_usec / 1000);
}

uint64_t bennet_get_microseconds(void) {
  struct timeval tv;

  gettimeofday(&tv, NULL);
  return (((uint64_t)tv.tv_sec) * 1000000) + tv.tv_usec;
}

int64_t timediff_timeval(struct timeval *early, struct timeval *late) {
  return (late->tv_sec - early->tv_sec) * 1000000 + (late->tv_usec - early->tv_usec);
}

size_t bennet_compute_size(enum bennet_sizing_strategy strategy,
    int max_tests,
    int max_discard_ratio,
    int successes,
    int recent_discards) {
  size_t max_size = bennet_get_max_size();

  switch (strategy) {
    case BENNET_SIZE_UNIFORM:;
      size_t sz = bennet_uniform_uint16_t(max_size) + 1;
      return sz;

    case BENNET_SIZE_CONSTANT:
      return max_size;

    case BENNET_SIZE_QUICKCHECK:;
      size_t discard_divisor;
      if (max_discard_ratio > 0) {
        discard_divisor = (successes * max_discard_ratio / 3);
        if (discard_divisor < 1) {
          discard_divisor = 1;
        } else if (discard_divisor > 10) {
          discard_divisor = 10;
        }
      } else {
        discard_divisor = 1;
      }

      size_t potential_size;
      if ((successes / max_size) * max_size + max_size <= max_tests ||
          successes >= max_tests || max_tests % max_size == 0) {
        potential_size = (successes % max_tests + recent_discards / discard_divisor);
      } else {
        potential_size = (successes % max_size) * max_size / (successes % max_size) +
                         recent_discards / discard_divisor;
      }

      if (potential_size < max_size) {
        return potential_size + 1;
      }

      if (max_size > global_max_size) {
        return global_max_size;
      }

      return max_size;

    default:
      assert(false);
  }
}
