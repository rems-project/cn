#ifndef BENNET_INTERNALS_LAZY_H
#define BENNET_INTERNALS_LAZY_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Register a new pointer for lazy tracking.
 * The pointer is initially marked as unused (false).
 *
 * @param ptr The pointer to track
 */
void bennet_lazy_new(void* ptr);

/**
 * Mark a tracked pointer as used.
 * Asserts that the pointer was previously registered with bennet_lazy_new.
 *
 * @param ptr The pointer to mark as used
 * @return true if the pointer was previously unused (value was updated),
 *         false if already marked as used
 */
bool bennet_lazy_mark(void* ptr);

/**
 * Unmark a tracked pointer, setting it back to unused.
 * Asserts that the pointer was previously registered and is currently marked.
 *
 * @param ptr The pointer to unmark
 */
void bennet_lazy_unmark(void* ptr);

/**
 * Check if a tracked pointer has been instantiated (marked as used).
 * Asserts that the pointer was previously registered with bennet_lazy_new.
 *
 * @param ptr The pointer to check
 * @return true if the pointer has been instantiated, false otherwise
 */
bool bennet_lazy_is_instantiated(void* ptr);

/**
 * Reset the lazy tracking system.
 * Clears all tracked pointers. Called during bennet_destroy().
 */
void bennet_lazy_reset(void);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_INTERNALS_LAZY_H
