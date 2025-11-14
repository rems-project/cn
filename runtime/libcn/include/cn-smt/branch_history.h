#ifndef CN_SMT_BRANCH_HISTORY_H
#define CN_SMT_BRANCH_HISTORY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <cn-smt/trie.h>

/**
 * Global variable to control SMT pruning at runtime.
 * When true, enables SMT solver-based branch pruning during execution.
 */
extern bool cn_smt_pruning_at_runtime;

/**
 * Node structure for the branch history queue.
 * Forms a doubly-linked list for efficient traversal and manipulation.
 */
struct branch_history_node {
  uint64_t data;                    /**< The stored uint64_t value */
  struct branch_history_node* next; /**< Pointer to the next node */
  struct branch_history_node* prev; /**< Pointer to the previous node */
};

/**
 * Checkpoint type - represents a position in the branch history queue.
 * This is essentially a pointer to a specific node that can be used
 * for restoration operations.
 */
typedef struct branch_history_node* branch_history_checkpoint;

/**
 * Queue structure that maintains pointers to head and tail for efficient operations.
 */
struct branch_history_queue {
  struct branch_history_node* head; /**< First node in the queue */
  struct branch_history_node* tail; /**< Last node in the queue (for fast append) */
  size_t length;                    /**< Number of nodes in the queue */
};

/**
 * Initialize a new branch history queue.
 * 
 * @param queue Pointer to the queue structure to initialize
 */
void branch_history_init(struct branch_history_queue* queue);

/**
 * Record a uint64_t value to the end of the queue.
 * 
 * @param queue Pointer to the queue
 * @param data The uint64_t value to record
 * @return Pointer to the newly created node, or NULL on allocation failure
 */
struct branch_history_node* branch_history_record(
    struct branch_history_queue* queue, uint64_t data);

/**
 * Get a checkpoint representing the current tail of the queue.
 * This checkpoint can be used later to restore the queue to this state.
 * 
 * @param queue Pointer to the queue
 * @return Checkpoint representing the current tail, or NULL if queue is empty
 */
branch_history_checkpoint branch_history_checkpoint_current(
    const struct branch_history_queue* queue);

/**
 * Get a checkpoint for a specific node.
 * 
 * @param node Pointer to the node to create a checkpoint for
 * @return Checkpoint for the given node
 */
branch_history_checkpoint branch_history_checkpoint_at(struct branch_history_node* node);

/**
 * Restore the queue to a specific checkpoint by removing all nodes after that point.
 * The checkpoint node itself is preserved.
 * 
 * @param queue Pointer to the queue
 * @param checkpoint The checkpoint to restore to
 */
void branch_history_restore(
    struct branch_history_queue* queue, branch_history_checkpoint checkpoint);

/**
 * Get the length of the queue.
 * 
 * @param queue Pointer to the queue
 * @return Number of nodes in the queue
 */
size_t branch_history_length(const struct branch_history_queue* queue);

/**
 * Check if the queue is empty.
 * 
 * @param queue Pointer to the queue
 * @return true if the queue is empty, false otherwise
 */
bool branch_history_is_empty(const struct branch_history_queue* queue);

/**
 * Free all nodes in the queue and reset it to empty state.
 * 
 * @param queue Pointer to the queue to clear
 */
void branch_history_clear(struct branch_history_queue* queue);

/**
 * Update the head of the queue to the next node and return the old head's value.
 * 
 * @param queue Pointer to the queue
 * @return The value from the old head node, or UINT64_MAX if queue was empty
 */
uint64_t branch_history_next(struct branch_history_queue* queue);

/**
 * Get the data from a specific node.
 * 
 * @param node Pointer to the node
 * @return The uint64_t value stored in the node, or UINT64_MAX if node is NULL
 */
uint64_t branch_history_node_data(const struct branch_history_node* node);

/**
 * Rewind the head of the queue to the first element.
 * Updates the head pointer to point to the original head via prev pointer.
 * 
 * @param queue Pointer to the queue
 */
void branch_history_rewind(struct branch_history_queue* queue);

/**
 * Update the trie with the sequence of values from the branch history queue.
 * Loops through the queue from head to NULL, collecting all data values
 * and adding that sequence to the provided trie.
 * 
 * @param queue Pointer to the branch history queue to read from
 * @param trie Pointer to the trie to update with the sequence
 */
void branch_history_update_trie(const struct branch_history_queue* queue, cn_trie* trie);

#endif /* CN_SMT_BRANCH_HISTORY_H */
