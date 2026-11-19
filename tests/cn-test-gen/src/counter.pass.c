#include <stdint.h>

static uint64_t __count = 0;

// External linkage: exercises `accesses` handling in the test wrapper
// generated for externally-linked functions.
uint64_t count()
/*@ accesses __count;
    requires
        __count < 100u64;
  @*/
{
  return __count++;
}

// `static` so that this exercises the test wrapper generated for internal
// linkage as well as `accesses`.
static uint64_t count_alt()
/*@ accesses __count;
    requires
        __count < 100u64;
  @*/
{
  return __count++;
}
