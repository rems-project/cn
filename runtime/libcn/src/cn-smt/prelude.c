#include <assert.h>
#include <stdbool.h>

#include <cn-smt/prelude.h>

static bool cn_smt_initialized = false;

void cn_smt_destroy(void) {
  if (!cn_smt_initialized) {
    return;  // Already destroyed or never initialized
  }

  cn_smt_datatype_registry_reset();
  cn_smt_func_registry_reset();
  cn_smt_record_registry_reset();
  cn_smt_struct_registry_reset();
  cn_intern_destroy();

  cn_smt_initialized = false;
}

void cn_smt_init(void) {
  assert(!cn_smt_initialized && "CN-SMT already initialized - destroy first");

  // Registries are lazily initialized on first use, so no explicit initialization needed

  cn_smt_initialized = true;
}
