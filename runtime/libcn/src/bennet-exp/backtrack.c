#include <stdbool.h>

#include <bennet-exp/backtrack.h>
#include <bennet-exp/failure.h>
#include <bennet-exp/rand.h>
#include <bennet-exp/rand_alloc.h>

#define BENNET_BACKTRACK_ARBITRARY(cn_ty, c_ty)                                          \
  /** Returns whether this `let` should catch the failure */                             \
  bool bennet_backtrack_arbitrary_##cn_ty(int *backtracks,                               \
      bennet_domain(c_ty) * cs,                                                          \
      bennet_domain(c_ty) * cs_tmp,                                                      \
      const bennet_checkpoint *cp,                                                       \
      const void *var) {                                                                 \
    assert(bennet_failure_get_failure_type() != BENNET_FAILURE_NONE);                    \
                                                                                         \
    if (!bennet_failure_is_blamed(var)) {                                                \
      return false;                                                                      \
    }                                                                                    \
                                                                                         \
    if (cs->is_owned) {                                                                  \
      uintptr_t ptr = (uintptr_t)convert_from_##cn_ty((cn_ty *)var);                     \
      if ((uintptr_t)bennet_rand_alloc_min_ptr() <= ptr &&                               \
          ptr <= (uintptr_t)bennet_rand_alloc_max_ptr()) {                               \
        bennet_rand_alloc_free((void *)ptr);                                             \
      }                                                                                  \
    }                                                                                    \
                                                                                         \
    enum bennet_failure_type ty = bennet_failure_get_failure_type();                     \
    switch (ty) {                                                                        \
      case BENNET_FAILURE_ASSERT:                                                        \
      case BENNET_FAILURE_ASSIGN:                                                        \
      case BENNET_FAILURE_DEPTH:                                                         \
        if (*backtracks <= 0) {                                                          \
          bennet_failure_remove_blame(var);                                              \
          return false;                                                                  \
        }                                                                                \
                                                                                         \
        bennet_domain_failure_info *new_cs = bennet_failure_get_domain(var);             \
        bennet_domain_update(c_ty, cs_tmp, new_cs);                                      \
        if (bennet_domain_is_empty(c_ty)(cs_tmp)) {                                      \
          return false;                                                                  \
        }                                                                                \
        if (ty == BENNET_FAILURE_ASSIGN || bennet_failure_is_young()) {                  \
          *cs = *cs_tmp;                                                                 \
        }                                                                                \
                                                                                         \
        (*backtracks)--;                                                                 \
                                                                                         \
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
