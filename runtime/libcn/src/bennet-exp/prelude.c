#include <bennet-exp/alloc.h>
#include <bennet-exp/failure.h>
#include <bennet-exp/rand_alloc.h>

void bennet_reset(void) {
  bennet_failure_reset();
  bennet_alloc_reset();
  bennet_ownership_reset();
  bennet_rand_alloc_free_all();
}
