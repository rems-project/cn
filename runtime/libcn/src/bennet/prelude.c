#include <assert.h>

#include <bennet/internals/lazy.h>
#include <bennet/internals/size.h>
#include <bennet/state/alloc.h>
#include <bennet/state/failure.h>
#include <bennet/state/rand_alloc.h>

static _Bool bennet_initialized = false;

void bennet_destroy(void) {
  if (!bennet_initialized) {
    return;  // Already destroyed or never initialized
  }

  bennet_failure_reset();
  bennet_alloc_destroy();
  bennet_ownership_destroy();
  bennet_rand_alloc_free_all();
  bennet_lazy_reset();

  bennet_initialized = false;
}

void bennet_init(void) {
  assert(!bennet_initialized && "Bennet already initialized - destroy first");

  bennet_alloc_init();
  bennet_ownership_init();
  bennet_set_depth(0);

  bennet_initialized = true;
}
