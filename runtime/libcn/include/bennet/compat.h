#ifndef BENNET_COMPAT_H
#define BENNET_COMPAT_H

#include <stdint.h>

#include <bennet/internals/rand.h>
#include <bennet/state/failure.h>

#define BENNET_BACKTRACK_NONE BENNET_FAILURE_NONE

#define BENNET_BACKTRACK_TIMEOUT BENNET_FAILURE_TIMEOUT

#endif  // BENNET_COMPAT_H
