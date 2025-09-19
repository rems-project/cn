#ifndef BENNET_URN_H
#define BENNET_URN_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Weighted random sampling urn data structure.
 * 
 * Maintains a balanced binary tree for efficient O(log n) insertion and removal
 * of weighted elements. The size tracks the number of elements for path generation.
 */
struct bennet_int_urn {
  uint8_t size;
  struct bennet_int_tree* tree;
};

/**
 * @brief Constructs an urn from an array of (weight, value) pairs.
 * 
 * The array should contain alternating weight and value elements. Pairs with
 * zero weight are skipped during construction. This provides a convenient
 * way to initialize an urn from a list of weighted elements.
 * 
 * Time complexity: O(n log n), where n is the number of non-zero weight pairs.
 * 
 * @param elems Array of alternating weights and values
 * @param len Number of (weight, value) pairs (array length / 2)
 * @return Pointer to newly allocated urn, or NULL on failure
 */
struct bennet_int_urn* urn_from_array(uint64_t elems[], uint8_t len);

/**
 * @brief Inserts a new weighted element into the urn.
 * 
 * Maintains tree balance by using the urn's current size as a binary path
 * to determine placement. The path is traversed bit-by-bit from the least
 * significant bit, ensuring a balanced tree structure. Weights of ancestor
 * nodes are incremented accordingly.
 * 
 * Time complexity: O(log n).
 * 
 * @param urn Pointer to the urn
 * @param weight Weight of the new element
 * @param value Value of the new element
 */
void urn_insert(struct bennet_int_urn* urn, uint64_t weight, uint64_t value);

/**
 * @brief Removes a randomly sampled element from the urn.
 * 
 * Generates a random index proportional to element weights, then removes
 * the corresponding element. Uses the "remove at index" strategy: uninserts
 * the last element, then replaces the target element with the uninserted one
 * if they're different.
 * 
 * Time complexity: O(log n).
 * 
 * @param urn Pointer to the urn
 * @return Value of the removed element
 */
uint64_t urn_remove(struct bennet_int_urn* urn);

/**
 * @brief Frees all memory associated with an urn.
 * 
 * Recursively frees the tree structure and the urn itself.
 * The urn pointer becomes invalid after this call.
 * 
 * @param urn Pointer to the urn to free
 */
void urn_free(struct bennet_int_urn* urn);

#ifdef __cplusplus
}
#endif

#endif  // BENNET_URN_H
