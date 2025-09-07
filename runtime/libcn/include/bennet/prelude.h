#ifndef BENNET_PRELUDE_H
#define BENNET_PRELUDE_H

#include <bennet/dsl.h>
#include <bennet/info/backtracks.h>
#include <bennet/info/sizes.h>
#include <bennet/info/tyche.h>
#include <bennet/info/unsatisfied.h>
#include <bennet/internals/domain.h>
#include <bennet/internals/domains/ownership.h>
#include <bennet/internals/domains/products.h>
#include <bennet/internals/domains/sized.h>
#include <bennet/internals/domains/tnum.h>
#include <bennet/internals/domains/wint.h>
#include <bennet/internals/rand.h>
#include <bennet/internals/size.h>
#include <bennet/internals/urn.h>
#include <bennet/state/alloc.h>
#include <bennet/state/checkpoint.h>
#include <bennet/state/failure.h>
#include <bennet/state/rand_alloc.h>
#include <bennet/utils/optional.h>
#include <bennet/utils/vector.h>

#ifdef __cplusplus
extern "C" {
#endif

void bennet_reset(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_PRELUDE_H
