#ifndef CN_SMT_TRIE_H
#define CN_SMT_TRIE_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <bennet/utils/hash_table.h>
#include <bennet/utils/optional.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Forward declaration of trie node structure
 */
typedef struct cn_trie_node cn_trie_node;

/**
 * @brief Typedef for trie node pointer to work with hash table macros
 */
typedef cn_trie_node* trie_node_ptr;

/**
 * @brief Declare optional type for trie_node_ptr
 */
BENNET_OPTIONAL_DECL(trie_node_ptr);

/**
 * @brief Declare hash table type for uint64_t -> trie_node_ptr
 */
BENNET_HASH_TABLE_DECL(uint64_t, trie_node_ptr);
BENNET_HASH_TABLE_IMPL(uint64_t, trie_node_ptr);

/**
 * @brief Trie structure (wrapper around the root node)
 */
typedef struct {
  cn_trie_node* root; /**< Root node of the trie */
} cn_trie;

/**
 * @brief Create a new empty trie
 * @return Pointer to newly created trie, or NULL on allocation failure
 */
cn_trie* cn_trie_create(void);

/**
 * @brief Destroy a trie and free all its memory
 * @param trie Trie to destroy (can be NULL)
 */
void cn_trie_destroy(cn_trie* trie);

/**
 * @brief Add a sequence of uint64_t values to the trie
 * @param trie The trie to add to
 * @param sequence Array of uint64_t values to add (can be NULL if length is 0)
 * @param length Number of elements in the sequence
 * @return true on success, false on allocation failure
 */
bool cn_trie_add_sequence(cn_trie* trie, const uint64_t* sequence, size_t length);

/**
 * @brief Get the subtrie starting from the given key
 * @param node The node to search from
 * @param key The key to look for
 * @return Pointer to the subtrie node, or NULL if the key doesn't exist
 */
cn_trie_node* cn_trie_get_subtrie(cn_trie_node* node, uint64_t key);

/**
 * @brief Check if the given key from this node leads to a terminal node (leaf)
 * @param node The node to search from
 * @param key The key to check
 * @return true if following the key leads to a terminal node, false otherwise
 */
bool cn_trie_is_leaf(cn_trie_node* node, uint64_t key);

/**
 * @brief Create a new trie node
 * @return Pointer to newly created node, or NULL on allocation failure
 */
cn_trie_node* cn_trie_node_create(void);

/**
 * @brief Destroy a trie node and all its children recursively
 * @param node Node to destroy (can be NULL)
 */
void cn_trie_node_destroy(cn_trie_node* node);

/**
 * @brief Set a trie node as terminal
 * @param node Node to mark as terminal
 */
void cn_trie_node_set_terminal(cn_trie_node* node);

/**
 * @brief Clear all children of a trie node
 * @param node Node whose children to clear
 */
void cn_trie_node_clear_children(cn_trie_node* node);

#ifdef __cplusplus
}
#endif

#endif  // CN_SMT_TRIE_H
