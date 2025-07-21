#include <bennet/alloc.h>
#include <bennet/failure.h>
#include <bennet/size.h>

void bennet_reset(void) {
  bennet_failure_reset();
  bennet_alloc_reset();
  bennet_ownership_reset();
  bennet_set_depth(0);
}
