#include <bennet/alloc.h>
#include <bennet/failure.h>

void bennet_reset(void) {
  bennet_failure_reset();
  bennet_alloc_reset();
  bennet_ownership_reset();
}
