#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/failure.h>
#include <bennet/state/rand_alloc.h>

void bennet_reset(void) {
  bennet_failure_reset();
  bennet_alloc_reset();
  bennet_ownership_reset();
  bennet_rand_alloc_free_all();
  bennet_set_depth(0);
}
