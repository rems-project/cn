#ifndef CN_SMT_MEMORY_INTERN_H
#define CN_SMT_MEMORY_INTERN_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Intern a string, returning a canonical pointer.
 *
 * This function ensures that all strings with identical content share the same
 * memory location, enabling fast pointer equality comparisons instead of strcmp.
 * The returned pointer remains valid until cn_intern_destroy() is called.
 *
 * The implementation uses lazy initialization - the first call to this function
 * (or any other intern function) will automatically initialize the global
 * interning table and arena.
 *
 * @param str The string to intern. Must not be NULL.
 * @return A pointer to the canonical copy of the string, or NULL on allocation failure.
 */
const char* cn_intern_string(const char* str);

/**
 * Look up a string in the intern table without adding it.
 *
 * This function checks if a string has already been interned without
 * adding it to the table if it's not present.
 *
 * @param str The string to look up. Must not be NULL.
 * @return A pointer to the canonical copy if the string is interned,
 *         or NULL if it's not in the table.
 */
const char* cn_intern_lookup(const char* str);

/**
 * Get the number of unique strings currently interned.
 *
 * @return The count of interned strings, or 0 if not initialized.
 */
size_t cn_intern_count(void);

/**
 * Destroy the intern table and free all associated memory.
 *
 * After calling this function, all pointers returned by cn_intern_string()
 * become invalid. This function is primarily useful for testing and cleanup.
 */
void cn_intern_destroy(void);

#ifdef __cplusplus
}
#endif

#endif /* CN_SMT_MEMORY_INTERN_H */
