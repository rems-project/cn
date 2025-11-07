#include <cn-smt/prelude.h>

void cn_smt_reset(void) {
  cn_smt_datatype_registry_reset();
  cn_smt_func_registry_reset();
  cn_smt_record_registry_reset();
  cn_smt_struct_registry_reset();
  cn_intern_destroy();
}
