#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-smt/branch_history.h>
#include <cn-smt/trie.h>

// Debug flag - set to 1 to enable debug prints (optimized out at compile time if 0)
#define DEBUG_BRANCH_HISTORY 0

#if DEBUG_BRANCH_HISTORY
// Helper function to print the entire branch history
static void print_branch_history(
    const char* prefix, const struct branch_history_queue* queue) {
  fprintf(stderr, "[BH_DEBUG] %s history (len=%zu): [", prefix, queue->length);
  struct branch_history_node* curr = queue->head;
  bool first = true;
  while (curr != NULL) {
    if (!first)
      fprintf(stderr, ", ");
    fprintf(stderr, "%" PRIu64, curr->data);
    first = false;
    curr = curr->next;
  }
  fprintf(stderr, "]\n");
  fflush(stderr);
}
#endif

/**
 * Global variable to control SMT pruning at runtime.
 * When true, enables SMT solver-based branch pruning during execution.
 */
bool cn_smt_pruning_at_runtime = false;

/**
 * Initialize a new branch history queue.
 * Sets all pointers to NULL and length to 0.
 * 
 * @param queue Pointer to the queue structure to initialize
 */
void branch_history_init(struct branch_history_queue* queue) {
  assert(queue != NULL);

  queue->head = NULL;
  queue->tail = NULL;
  queue->length = 0;
}

/**
 * Record a uint64_t value to the end of the queue.
 * 
 * @param queue Pointer to the queue
 * @param data The uint64_t value to record
 * @return Pointer to the newly created node, or NULL on allocation failure
 */
struct branch_history_node* branch_history_record(
    struct branch_history_queue* queue, uint64_t data) {
  assert(queue != NULL);

#if DEBUG_BRANCH_HISTORY
  fprintf(stderr, "[BH_DEBUG] RECORD choice %" PRIu64 "\n", data);
  fflush(stderr);
#endif

  // Allocate the node
  struct branch_history_node* new_node = malloc(sizeof(struct branch_history_node));
  if (new_node == NULL) {
    return NULL;
  }

  // Initialize the node
  new_node->data = data;
  new_node->next = NULL;
  new_node->prev = queue->tail;

  // Update queue pointers
  if (queue->tail != NULL) {
    queue->tail->next = new_node;
  } else {
    // First node in the queue
    queue->head = new_node;
  }

  queue->tail = new_node;
  queue->length++;

#if DEBUG_BRANCH_HISTORY
  print_branch_history("  After RECORD", queue);
#endif

  return new_node;
}

/**
 * Get a checkpoint representing the current tail of the queue.
 * 
 * @param queue Pointer to the queue
 * @return Checkpoint representing the current tail, or NULL if queue is empty
 */
branch_history_checkpoint branch_history_checkpoint_current(
    const struct branch_history_queue* queue) {
  assert(queue != NULL);

#if DEBUG_BRANCH_HISTORY
  if (queue->tail == NULL) {
    fprintf(stderr, "[BH_DEBUG] CHECKPOINT_CURRENT: returning NULL (queue empty)\n");
  } else {
    fprintf(stderr,
        "[BH_DEBUG] CHECKPOINT_CURRENT: returning node=%p (data=%" PRIu64 ")\n",
        (void*)queue->tail,
        queue->tail->data);
  }
  print_branch_history("  Current state", queue);
#endif

  return queue->tail;
}

/**
 * Get a checkpoint for a specific node.
 * 
 * @param node Pointer to the node to create a checkpoint for
 * @return Checkpoint for the given node
 */
branch_history_checkpoint branch_history_checkpoint_at(struct branch_history_node* node) {
  return node;
}

/**
 * Free all nodes after (but not including) the given node.
 * Helper function for restoration operations.
 * 
 * @param start_node Node after which to start freeing (this node is preserved)
 */
static void free_nodes_after(struct branch_history_node* start_node) {
  if (start_node == NULL) {
    return;
  }

  struct branch_history_node* curr = start_node->next;
  while (curr != NULL) {
    struct branch_history_node* next = curr->next;

    // Free the node itself
    free(curr);

    curr = next;
  }

  // Update the start node to have no successors
  start_node->next = NULL;
}

/**
 * Restore the queue to a specific checkpoint by removing all nodes after that point.
 * The checkpoint node itself is preserved. If checkpoint is NULL, the queue is cleared
 * and the queue pointer may be set to NULL if the queue becomes empty.
 * 
 * @param queue Pointer to the queue
 * @param checkpoint The checkpoint to restore to
 */
void branch_history_restore(
    struct branch_history_queue* queue, branch_history_checkpoint checkpoint) {
  assert(queue != NULL);

#if DEBUG_BRANCH_HISTORY
  if (checkpoint == NULL) {
    fprintf(stderr, "[BH_DEBUG] RESTORE to checkpoint (NULL)\n");
  } else {
    fprintf(stderr,
        "[BH_DEBUG] RESTORE to checkpoint (node=%p, data=%" PRIu64 ")\n",
        (void*)checkpoint,
        checkpoint->data);
  }
  print_branch_history("  Before RESTORE", queue);
#endif

  if (checkpoint == NULL) {
    // Restore to empty queue
    branch_history_clear(queue);
#if DEBUG_BRANCH_HISTORY
    print_branch_history("  After RESTORE to NULL (cleared)", queue);
#endif
    return;
  }

  // Free all nodes after the checkpoint
  free_nodes_after(checkpoint);

  // Update queue tail pointer
  queue->tail = checkpoint;

  // Recalculate length by traversing from head to tail
  queue->length = 0;
  struct branch_history_node* curr = queue->head;
  while (curr != NULL) {
    (queue)->length++;
    if (curr == (queue)->tail) {
      break;
    }
    curr = curr->next;
  }

#if DEBUG_BRANCH_HISTORY
  print_branch_history("  After RESTORE", queue);
#endif
}

/**
 * Get the length of the queue.
 * 
 * @param queue Pointer to the queue
 * @return Number of nodes in the queue
 */
size_t branch_history_length(const struct branch_history_queue* queue) {
  assert(queue != NULL);

  return queue->length;
}

/**
 * Check if the queue is empty.
 * 
 * @param queue Pointer to the queue
 * @return true if the queue is empty, false otherwise
 */
bool branch_history_is_empty(const struct branch_history_queue* queue) {
  assert(queue != NULL);

  return queue->length == 0;
}

/**
 * Free all nodes in the queue and reset it to empty state.
 * 
 * @param queue Pointer to the queue to clear
 */
void branch_history_clear(struct branch_history_queue* queue) {
  assert(queue != NULL);

#if DEBUG_BRANCH_HISTORY
  fprintf(stderr, "[BH_DEBUG] CLEAR called\n");
  print_branch_history("  Before CLEAR", queue);
#endif

  struct branch_history_node* curr = queue->head;
  while (curr != NULL) {
    struct branch_history_node* next = curr->next;

    // Free the node itself
    free(curr);

    curr = next;
  }

  // Reset queue to empty state
  queue->head = NULL;
  queue->tail = NULL;
  queue->length = 0;

#if DEBUG_BRANCH_HISTORY
  fprintf(stderr, "[BH_DEBUG]   After CLEAR history (len=0): []\n");
#endif
}

/**
 * Update the head of the queue to the next node and return the old head's value.
 * 
 * @param queue Pointer to the queue
 * @return The value from the old head node, or UINT64_MAX if queue was empty
 */
uint64_t branch_history_next(struct branch_history_queue* queue) {
  assert(queue != NULL);

  struct branch_history_node* old_head = queue->head;

  if (old_head == NULL) {
#if DEBUG_BRANCH_HISTORY
    fprintf(stderr, "[BH_DEBUG] NEXT (GATHER) - RAN OUT! Returning UINT64_MAX\n");
    fflush(stderr);
#endif
    return UINT64_MAX;
  }

  uint64_t data = old_head->data;

#if DEBUG_BRANCH_HISTORY
  fprintf(stderr, "[BH_DEBUG] NEXT (GATHER) consuming %" PRIu64 "\n", data);
  print_branch_history("  Before NEXT", queue);
#endif

  queue->head = old_head->next;

  // Note: We do NOT set tail to NULL when head becomes NULL
  // because rewind() needs tail to find the last node and traverse backwards

  queue->length--;

#if DEBUG_BRANCH_HISTORY
  print_branch_history("  After NEXT", queue);
#endif

  return data;
}

/**
 * Get the data from a specific node.
 * 
 * @param node Pointer to the node
 * @return The uint64_t value stored in the node, or UINT64_MAX if node is NULL
 */
uint64_t branch_history_node_data(const struct branch_history_node* node) {
  if (node == NULL) {
    return UINT64_MAX;
  }

  return node->data;
}

/**
 * Rewind the head of the queue to the first element.
 * Updates the head pointer to point to the original head via prev pointer.
 * Also recalculates and restores the correct queue length.
 * Works even when head is NULL after consuming all nodes by using tail.
 * 
 * @param queue Pointer to the queue
 */
void branch_history_rewind(struct branch_history_queue* queue) {
  assert(queue != NULL);

#if DEBUG_BRANCH_HISTORY
  fprintf(
      stderr, "\n[BH_DEBUG] ========== REWIND (PATH SELECTION -> GATHER) ==========\n");
  print_branch_history("  Before REWIND", queue);
#endif

  // If both head and tail are NULL, queue is empty
  if (queue->head == NULL && queue->tail == NULL) {
#if DEBUG_BRANCH_HISTORY
    fprintf(stderr, "[BH_DEBUG] REWIND: Queue is empty\n");
#endif
    return;
  }

  // If head is NULL but tail is not, we consumed all nodes
  // Start from tail and traverse backwards to find the first node
  if (queue->head == NULL && queue->tail != NULL) {
    queue->head = queue->tail;
    while (queue->head->prev != NULL) {
      queue->head = queue->head->prev;
    }
  } else {
    // Normal case: head is not NULL, traverse backwards to first node
    while (queue->head->prev != NULL) {
      queue->head = queue->head->prev;
    }
  }

  // Recalculate length by traversing from head to tail
  queue->length = 0;
  struct branch_history_node* curr = queue->head;
  while (curr != NULL) {
    queue->length++;
    if (curr == queue->tail) {
      break;
    }
    curr = curr->next;
  }

#if DEBUG_BRANCH_HISTORY
  print_branch_history("  After REWIND (FINAL history for gather)", queue);
  fprintf(
      stderr, "[BH_DEBUG] ======================================================\n\n");
  fflush(stderr);
#endif
}

/**
 * Update the trie with the sequence of values from the branch history queue.
 * Loops through the queue from head to NULL, collecting all data values
 * and adding that sequence to the provided trie.
 * 
 * @param queue Pointer to the branch history queue to read from
 * @param trie Pointer to the trie to update with the sequence
 */
void branch_history_update_trie(const struct branch_history_queue* queue, cn_trie* trie) {
  assert(queue != NULL);
  assert(trie != NULL);

  // If queue is empty, add empty sequence (mark root as terminal)
  if (queue->length == 0) {
    cn_trie_add_sequence(trie, NULL, 0);
    return;
  }

  // Allocate array to hold the sequence using queue length
  uint64_t* sequence = malloc(queue->length * sizeof(uint64_t));
  assert(sequence != NULL);

  // Collect all data values from head to NULL
  struct branch_history_node* curr = queue->head;
  for (size_t i = 0; i < queue->length && curr != NULL; i++) {
    sequence[i] = curr->data;
    curr = curr->next;
  }

  // Add the sequence to the trie
  cn_trie_add_sequence(trie, sequence, queue->length);

  // Free the temporary array
  free(sequence);
}
