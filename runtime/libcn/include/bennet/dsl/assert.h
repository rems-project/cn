#ifndef BENNET_ASSERT_H
#define BENNET_ASSERT_H

#include <stdint.h>

#include <bennet/info/backtracks.h>
#include <bennet/state/failure.h>
#include <bennet/utils/optional.h>
#include <cn-executable/utils.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BENNET_ASSERT(cond, last_var, ...)                                               \
  if (!convert_from_cn_bool(cond)) {                                                     \
    bennet_info_backtracks_log(__FUNCTION__, __FILE__, __LINE__);                        \
    bennet_info_unsatisfied_log(__FILE__, __LINE__, true);                               \
                                                                                         \
    bennet_failure_set_failure_type(BENNET_FAILURE_ASSERT);                              \
    const void* vars[] = {__VA_ARGS__};                                                  \
    bennet_failure_blame_many(vars);                                                     \
    goto bennet_label_##last_var##_backtrack;                                            \
  }                                                                                      \
                                                                                         \
  bennet_info_unsatisfied_log(__FILE__, __LINE__, false);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_ASSERT_H
