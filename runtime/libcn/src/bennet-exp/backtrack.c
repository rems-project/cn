#include <stdbool.h>

#include <bennet-exp/backtrack.h>
#include <bennet-exp/failure.h>
#include <bennet-exp/rand.h>

#define BENNET_BACKTRACK_ARBITRARY(cn_ty, c_ty)                                          \
  /** Returns whether this `let` should catch the failure */                             \
  bool bennet_backtrack_arbitrary_##cn_ty(int *backtracks,                               \
      bennet_domain(c_ty) * cs,                                                          \
      const bennet_checkpoint *cp,                                                       \
      const void *var) {                                                                 \
    assert(bennet_failure_get_failure_type() != BENNET_FAILURE_NONE);                    \
                                                                                         \
    if (!bennet_failure_is_blamed(var)) {                                                \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    switch (bennet_failure_get_failure_type()) {                                         \
      case BENNET_FAILURE_ASSERT:                                                        \
      case BENNET_FAILURE_ASSIGN:                                                        \
      case BENNET_FAILURE_DEPTH:                                                         \
        if (*backtracks <= 0) {                                                          \
          bennet_failure_remove_blame(var);                                              \
          return false;                                                                  \
        }                                                                                \
                                                                                         \
        (*backtracks)--;                                                                 \
        bennet_domain_update(c_ty, var, cs);                                             \
        bennet_checkpoint_restore(cp);                                                   \
        bennet_failure_reset();                                                          \
                                                                                         \
        return true;                                                                     \
                                                                                         \
      case BENNET_FAILURE_NONE:                                                          \
        assert(false); /* unreachable */                                                 \
    }                                                                                    \
                                                                                         \
    assert(false); /* unreachable */                                                     \
    return false;                                                                        \
  }

#define BENNET_BACKTRACK_ARBITRARY_BV(bits)                                              \
  BENNET_BACKTRACK_ARBITRARY(cn_bits_i##bits, int##bits##_t)                             \
  BENNET_BACKTRACK_ARBITRARY(cn_bits_u##bits, uint##bits##_t)

BENNET_BACKTRACK_ARBITRARY(cn_pointer, uintptr_t)
BENNET_BACKTRACK_ARBITRARY_BV(8)
BENNET_BACKTRACK_ARBITRARY_BV(16)
BENNET_BACKTRACK_ARBITRARY_BV(32)
BENNET_BACKTRACK_ARBITRARY_BV(64)

/** Returns whether this `let` should catch the failure */
bool bennet_backtrack(int *backtracks, const bennet_checkpoint *cp, const void *var) {
  assert(bennet_failure_get_failure_type() != BENNET_FAILURE_NONE);

  if (!bennet_failure_is_blamed(var)) {
    return false;
  }

  switch (bennet_failure_get_failure_type()) {
    case BENNET_FAILURE_ASSERT:
    case BENNET_FAILURE_ASSIGN:
    case BENNET_FAILURE_DEPTH:
      if (*backtracks <= 0) {
        bennet_failure_remove_blame(var);
        return false;
      }

      (*backtracks)--;
      bennet_checkpoint_restore(cp);
      bennet_failure_reset();

      return true;

    case BENNET_FAILURE_NONE:
      assert(false); /* unreachable */
  }

  assert(false); /* unreachable */
  return false;
}
