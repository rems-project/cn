#include <bennet-exp/state/alloc.h>
#include <bennet-exp/state/failure.h>
#include <bennet-exp/state/rand_alloc.h>

void bennet_reset(void) {
  bennet_failure_reset();
  bennet_alloc_reset();
  bennet_ownership_reset();
  bennet_rand_alloc_free_all();
}
