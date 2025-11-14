#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <bennet/internals/rand.h>
#include <bennet/internals/urn.h>

/**
 * @brief Binary tree node for weighted random sampling.
 * 
 * Each node contains a weight (cumulative for internal nodes, actual for leaves)
 * and a value. Leaf nodes have NULL left and right pointers.
 */
struct bennet_int_tree {
  uint64_t weight;
  uint64_t value;

  struct bennet_int_tree* left;
  struct bennet_int_tree* right;
};

/**
 * @brief Checks if a tree node is a leaf (has no children).
 * @param tree Pointer to the tree node
 * @return 1 if leaf, 0 if internal node
 */
static inline int is_leaf(struct bennet_int_tree* tree) {
  return tree->left == NULL && tree->right == NULL;
}

/**
 * @brief Deterministically samples an element from the tree at a given index.
 * 
 * Navigates the tree based on the index and weights of subtrees to find the
 * corresponding value. Compares the index with the left subtree's weight:
 * if less, recurses left; otherwise recurses right with adjusted index.
 * 
 * Time complexity: O(log n).
 * 
 * @param tree Pointer to the tree root
 * @param index Index to sample (0 <= index < total_weight)
 * @return Value at the given index, or -1 if tree is NULL
 */
uint64_t sample_tree_det(struct bennet_int_tree* tree, uint64_t index) {
  if (tree == NULL) {
    return -1;
  }

  if (is_leaf(tree)) {
    return tree->value;
  }

  if (index < tree->left->weight) {
    return sample_tree_det(tree->left, index);
  }

  return sample_tree_det(tree->right, index - tree->left->weight);
}

/**
 * @brief Samples a random element from the urn based on weights.
 * 
 * Generates a random index proportional to element weights, then uses
 * deterministic sampling to find the corresponding element.
 * 
 * @param urn Pointer to the urn
 * @return Randomly sampled value
 */
uint64_t sample_urn(struct bennet_int_urn* urn) {
  uint64_t index = bennet_uniform_uint64_t(urn->tree->weight);
  return sample_tree_det(urn->tree, index);
}

/**
 * @brief Inserts a leaf node into the tree following a binary path.
 * 
 * Uses the path parameter as a sequence of bits to determine placement.
 * Each bit (LSB first) indicates left (0) or right (1) traversal.
 * Creates internal nodes as needed and updates weights along the path.
 * 
 * @param path Binary path for insertion (size-based for balance)
 * @param tree Current tree root
 * @param leaf Leaf node to insert
 * @return Updated tree root
 */
static struct bennet_int_tree* insert_tree(
    uint8_t path, struct bennet_int_tree* tree, struct bennet_int_tree* leaf) {
  if (tree == NULL) {
    return leaf;
  }

  if (is_leaf(tree)) {
    struct bennet_int_tree* res =
        (struct bennet_int_tree*)malloc(sizeof(struct bennet_int_tree));
    assert(res);
    res->weight = tree->weight + leaf->weight;
    res->left = tree;
    res->right = leaf;
    return res;
  }

  if (path & 1) {
    tree->weight += leaf->weight;
    tree->right = insert_tree(path >> 1, tree->right, leaf);
  } else {
    tree->weight += leaf->weight;
    tree->left = insert_tree(path >> 1, tree->left, leaf);
  }
  return tree;
}

void urn_insert(struct bennet_int_urn* urn, uint64_t weight, uint64_t value) {
  struct bennet_int_tree* leaf =
      (struct bennet_int_tree*)malloc(sizeof(struct bennet_int_tree));
  assert(leaf);
  leaf->weight = weight;
  leaf->value = value;
  leaf->left = NULL;
  leaf->right = NULL;

  urn->tree = insert_tree(urn->size, urn->tree, leaf);
  urn->size += 1;
}

struct bennet_int_urn* urn_from_array(uint64_t elems[], uint8_t len) {
  struct bennet_int_urn* urn =
      (struct bennet_int_urn*)malloc(sizeof(struct bennet_int_urn));
  assert(urn);
  urn->size = 0;
  urn->tree = NULL;
  for (uint16_t i = 0; i < 2 * (uint16_t)len; i += 2) {
    if (elems[i] != 0) {
      urn_insert(urn, elems[i], elems[i + 1]);
    }
  }
  return urn;
}

struct replace_res {
  uint64_t weightOld;
  uint64_t valueOld;

  uint64_t weightNew;
  uint64_t valueNew;
};

/**
 * @brief Updates the weight and value of a leaf at a given index.
 * 
 * Navigates to the target leaf using the index, similar to sampling.
 * Applies the new weight and value, then adjusts ancestor weights
 * to reflect the change.
 * 
 * @param tree Tree root
 * @param weight New weight for the leaf
 * @param value New value for the leaf
 * @param index Index of the leaf to update
 * @return Structure containing old and new (weight, value) pairs
 */
static struct replace_res replace_tree(
    struct bennet_int_tree* tree, uint64_t weight, uint64_t value, uint64_t index) {
  if (tree == NULL) {
    assert(false);
  }

  if (is_leaf(tree)) {
    struct replace_res res = (struct replace_res){.weightOld = tree->weight,
        .valueOld = tree->value,

        .weightNew = weight,
        .valueNew = value};

    tree->weight = weight;
    tree->value = value;

    return res;
  }

  if (index < tree->left->weight) {
    struct replace_res res = replace_tree(tree->left, weight, value, index);

    tree->weight = tree->weight - res.weightOld + res.weightNew;

    return res;
  } else {
    struct replace_res res =
        replace_tree(tree->right, weight, value, (index - tree->left->weight));

    tree->weight = tree->weight - res.weightOld + res.weightNew;

    return res;
  }
}

/**
 * @brief Replaces an element at the given index with new weight and value.
 * 
 * @param urn Pointer to the urn
 * @param weight New weight
 * @param value New value
 * @param index Index of element to replace
 * @return Old value that was replaced
 */
static uint64_t replace(
    struct bennet_int_urn* urn, uint64_t weight, uint64_t value, uint64_t index) {
  return replace_tree(urn->tree, weight, value, index).valueOld;
}

struct uninsert_res {
  uint64_t weight;
  uint64_t value;

  uint64_t lowerBound;

  struct bennet_int_tree* tree;
};

/**
 * @brief Removes the leaf at the end of the given path.
 * 
 * This is the inverse of insert_tree. Follows the path to locate and remove
 * the target leaf, adjusting ancestor weights and cleaning up empty nodes.
 * Also computes the lower bound index for the removed element.
 * 
 * @param path Binary path to the leaf (based on size-1)
 * @param tree Current tree root
 * @return Structure containing removed element data and updated tree
 */
struct uninsert_res uninsert_tree(uint8_t path, struct bennet_int_tree* tree) {
  if (tree == NULL) {
    assert(false);
  }

  if (is_leaf(tree)) {
    uint64_t weight = tree->weight;
    uint64_t value = tree->value;
    free(tree);
    return (struct uninsert_res){.weight = weight, .value = value, .tree = NULL};
  }

  if (path & 1) {
    struct uninsert_res res = uninsert_tree(path >> 1, tree->right);
    tree->right = res.tree;
    tree->weight -= res.weight;

    res.tree = tree;
    res.lowerBound += tree->left->weight;
    return res;
  } else {
    struct uninsert_res res = uninsert_tree(path >> 1, tree->left);
    tree->left = res.tree;
    tree->weight -= res.weight;

    res.tree = tree;
    return res;
  }
}

/**
 * @brief Removes the most recently inserted element from the urn.
 * 
 * Uses the urn's size-1 as a path to locate and remove the last inserted
 * leaf. This is the inverse of urn_insert.
 * 
 * @param urn Pointer to the urn
 * @return Structure containing removed element and updated tree
 */
struct uninsert_res uninsert_urn(struct bennet_int_urn* urn) {
  urn->size -= 1;
  return uninsert_tree(urn->size, urn->tree);
}

/**
 * @brief Removes an element at a specific index from the urn.
 * 
 * Implements the "remove at index" strategy: first uninserts the last element,
 * then replaces the target element with the uninserted one if they're different.
 * This maintains tree balance while removing arbitrary elements.
 * 
 * @param urn Pointer to the urn
 * @param index Index of element to remove
 * @return Value of the removed element
 */
uint64_t remove_urn_det(struct bennet_int_urn* urn, uint64_t index) {
  struct uninsert_res res = uninsert_urn(urn);

  urn->tree = res.tree;

  if (res.tree == NULL) {
    return res.value;
  }

  if (index < res.lowerBound) {
    return replace(urn, res.weight, res.value, index);
  } else if (index < res.lowerBound + res.weight) {
    return res.value;
  } else {
    return replace(urn, res.weight, res.value, index - res.weight);
  }
}

uint64_t urn_remove(struct bennet_int_urn* urn) {
  uint64_t index = bennet_uniform_uint64_t(urn->tree->weight);
  return remove_urn_det(urn, index);
}

/**
 * @brief Recursively frees a tree and all its subtrees.
 * 
 * @param tree Pointer to tree root (may be NULL)
 */
void tree_free(struct bennet_int_tree* tree) {
  if (tree == NULL) {
    return;
  }

  if (is_leaf(tree)) {
    return free(tree);
  }

  tree_free(tree->left);
  tree_free(tree->right);
  return free(tree);
}

void urn_free(struct bennet_int_urn* urn) {
  tree_free(urn->tree);
  free(urn);
}
