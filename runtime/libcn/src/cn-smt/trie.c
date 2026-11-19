#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/trie.h>

/**
 * @brief Trie node structure containing children and terminal flag
 * 
 * Each node stores its children in a hash table for efficient lookup.
 * Uses uint64_t keys to map to child node pointers.
 */
struct cn_trie_node {
  bool is_terminal; /**< True if this node represents the end of a valid sequence */
  bennet_hash_table(
      uint64_t, trie_node_ptr) children; /**< Hash table mapping keys to child nodes */
};

/**
 * @brief Create a new trie node with default initialization
 * 
 * Allocates memory for a new trie node and initializes all fields to their
 * default values (empty hash table, not terminal).
 * 
 * @return Pointer to newly created node, assertion failure on allocation error
 */
cn_trie_node* cn_trie_node_create(void) {
  cn_trie_node* node = malloc(sizeof(cn_trie_node));
  assert(node);

  node->is_terminal = false;

  // Initialize the hash table for children
  bennet_hash_table_init(uint64_t, trie_node_ptr)(
      &node->children, bennet_hash_uint64_t, bennet_eq_uint64_t);

  return node;
}

/**
 * @brief Destroy a trie node and all its children recursively
 * 
 * Performs a depth-first traversal to destroy all child nodes before
 * freeing the current node's memory. Safe to call with NULL pointer.
 * 
 * @param node Node to destroy (can be NULL)
 */
void cn_trie_node_destroy(cn_trie_node* node) {
  if (!node) {
    return;
  }

  // Recursively destroy all children
  for (size_t i = 0; i < node->children.capacity; i++) {
    if (node->children.entries[i].occupied) {
      cn_trie_node_destroy((cn_trie_node*)node->children.entries[i].value);
    }
  }

  // Free the hash table entries
  bennet_hash_table_free(uint64_t, trie_node_ptr)(&node->children);

  // Free the node itself
  free(node);
}

/**
 * @brief Create a new empty trie
 * 
 * Allocates memory for a new trie structure and creates its root node.
 * The trie starts empty with no stored sequences.
 * 
 * @return Pointer to newly created trie, assertion failure on allocation error
 */
cn_trie* cn_trie_create(void) {
  cn_trie* trie = malloc(sizeof(cn_trie));
  assert(trie);

  trie->root = cn_trie_node_create();

  return trie;
}

/**
 * @brief Destroy a trie and free all its memory
 * 
 * Destroys the root node (which recursively destroys all children) and
 * frees the trie structure itself. Safe to call with NULL pointer.
 * 
 * @param trie Trie to destroy (can be NULL)
 */
void cn_trie_destroy(cn_trie* trie) {
  if (!trie) {
    return;
  }

  cn_trie_node_destroy(trie->root);
  free(trie);
}

/**
 * @brief Find an existing child or create a new one for the given key
 * 
 * Searches the node's children hash table for one with the matching key.
 * If found, returns the existing child. If not found, creates a new child
 * and adds it to the hash table.
 * 
 * @param node Parent node to search/add to
 * @param key Key to find or create child for
 * @return Pointer to child node, or NULL on allocation failure
 */
static cn_trie_node* find_or_create_child(cn_trie_node* node, uint64_t key) {
  // First, look for existing child with this key
  bennet_optional(trie_node_ptr) child_opt =
      bennet_hash_table_get(uint64_t, trie_node_ptr)(&node->children, key);

  if (bennet_optional_is_some(child_opt)) {
    // Child exists, use it
    return (cn_trie_node*)bennet_optional_unwrap(child_opt);
  }

  // Child doesn't exist, create it
  cn_trie_node* child = cn_trie_node_create();
  if (!child) {
    return NULL;
  }

  // Add the new child to the hash table
  bennet_hash_table_set(uint64_t, trie_node_ptr)(
      &node->children, key, (trie_node_ptr)child);

  return child;
}

/**
 * @brief Add a sequence of uint64_t values to the trie
 * 
 * Traverses the trie following the sequence, creating new nodes as necessary.
 * Marks the final node as terminal to indicate a complete sequence.
 * 
 * @param trie The trie to add to
 * @param sequence Array of uint64_t values to add (can be NULL if length is 0)
 * @param length Number of elements in the sequence
 * @return true on success, false on allocation failure
 */
bool cn_trie_add_sequence(cn_trie* trie, const uint64_t* sequence, size_t length) {
  assert(trie);
  assert(sequence || length == 0);

  cn_trie_node* current = trie->root;

  // Traverse/create path for each element in the sequence
  for (size_t i = 0; i < length; i++) {
    uint64_t key = sequence[i];

    current = find_or_create_child(current, key);
    if (!current) {
      return false;  // Allocation failed
    }
  }

  // Mark the final node as terminal
  current->is_terminal = true;

  return true;
}

/**
 * @brief Get the subtrie starting from the given key
 * 
 * Searches the node's children hash table for one with the matching key and returns it.
 * This allows traversing down the trie one level at a time.
 * 
 * @param node The node to search from
 * @param key The key to look for
 * @return Pointer to the subtrie node, or NULL if the key doesn't exist
 */
cn_trie_node* cn_trie_get_subtrie(cn_trie_node* node, uint64_t key) {
  if (!node) {
    return NULL;
  }

  bennet_optional(trie_node_ptr) child_opt =
      bennet_hash_table_get(uint64_t, trie_node_ptr)(&node->children, key);

  if (bennet_optional_is_some(child_opt)) {
    return (cn_trie_node*)bennet_optional_unwrap(child_opt);
  }

  return NULL;
}

/**
 * @brief Check if the given key from this node leads to a terminal node (leaf)
 * 
 * Finds the child node for the given key and checks if it's marked as terminal.
 * A terminal node indicates the end of a valid sequence in the trie.
 * 
 * @param node The node to search from
 * @param key The key to check
 * @return true if following the key leads to a terminal node, false otherwise
 */
bool cn_trie_is_leaf(cn_trie_node* node, uint64_t key) {
  cn_trie_node* child = cn_trie_get_subtrie(node, key);

  if (!child) {
    return false;  // Key doesn't exist
  }

  return child->is_terminal;
}

/**
 * @brief Set a trie node as terminal
 * 
 * Marks the node as representing the end of a valid sequence.
 * 
 * @param node Node to mark as terminal
 */
void cn_trie_node_set_terminal(cn_trie_node* node) {
  if (node) {
    node->is_terminal = true;
  }
}

/**
 * @brief Clear all children of a trie node
 * 
 * Destroys all child nodes and clears the children hash table.
 * The node itself remains valid but becomes childless.
 * 
 * @param node Node whose children to clear
 */
void cn_trie_node_clear_children(cn_trie_node* node) {
  if (!node) {
    return;
  }

  // Destroy all children
  for (size_t i = 0; i < node->children.capacity; i++) {
    if (node->children.entries[i].occupied) {
      cn_trie_node_destroy((cn_trie_node*)node->children.entries[i].value);
    }
  }

  // Clear the hash table
  bennet_hash_table_free(uint64_t, trie_node_ptr)(&node->children);

  // Reinitialize the hash table
  bennet_hash_table_init(uint64_t, trie_node_ptr)(
      &node->children, bennet_hash_uint64_t, bennet_eq_uint64_t);
}
