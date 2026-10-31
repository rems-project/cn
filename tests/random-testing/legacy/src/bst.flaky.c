#include <stddef.h>

#define KEY    int
#define VALUE  long

struct MapNode {
  KEY key;
  VALUE value; 
  struct MapNode *smaller;
  struct MapNode *larger;
};

extern void* cn_malloc(size_t size);


/*@

type_synonym KEY = i32
type_synonym VALUE = i64
type_synonym NodeData = { KEY key, VALUE value }

function (KEY) defaultKey() { 0i32 }

// -----------------------------------------------------------------------------
// Intervals

// Non-empty, closed intervals
type_synonym Interval = { KEY lower, KEY upper }

function (Interval) defaultInterval() {
  { lower: defaultKey(), upper: defaultKey() }
}

datatype IntervalOption {
  IntervalNone {},
  IntervalSome { Interval i }
}

function (boolean) isIntervalSome(IntervalOption i) {
  match i {
    IntervalNone {} => { false }
    IntervalSome { i:_ } => { true }
  }
}

function (Interval) fromIntervalOption(IntervalOption i) {
  match i {
    IntervalNone {}      => { defaultInterval() }
    IntervalSome { i:j } => { j }
  }
}


function (IntervalOption)
  joinInterval(IntervalOption optSmaller, KEY val, IntervalOption optLarger) {
  match optSmaller {
    IntervalNone {} => {
      match optLarger {
        IntervalNone {} => {
          IntervalSome { i: { lower: val, upper: val } }
        }
        IntervalSome { i: larger } => {
          if (val < larger.lower) {
            IntervalSome { i: { lower: val, upper: larger.upper } }
          } else {
            IntervalNone {}
          }
        }
      }
    }
    IntervalSome { i: smaller } => {
      if (val > smaller.upper) { 
        match optLarger {
          IntervalNone {} => {
            IntervalSome { i: { lower: smaller.lower, upper: val } }
          }
          IntervalSome { i: larger } => {
            if (val < larger.lower) {
              IntervalSome { i: { lower: smaller.lower, upper: larger.upper } }
            } else {
              IntervalNone {}
            }
          }
        }
      } else {
        IntervalNone {}
      }
    }
  }
}



// -----------------------------------------------------------------------------




// A binary dearch tree
datatype BST {
  Leaf {},
  Node { NodeData data, BST smaller, BST larger }
}

function [rec] (BST) insert(KEY key, VALUE value, BST tree) {
  match tree {
    Leaf {} => { Node { data: { key: key, value: value },
                        smaller: Leaf {}, larger: Leaf {} } }
    Node { data: data, smaller: smaller, larger: larger } => {
      if (data.key == key) {
        Node { data: { key: key, value: value },
               smaller: smaller, larger: larger }
      } else {
        if (data.key < key) {
          Node { data: data,
                 smaller: smaller, larger: insert(key,value,larger) }
        } else {
          Node { data: data,
                 smaller: insert(key,value,smaller), larger: larger }
        }
      }
    }
  }
}

// *****************************************************************************
// Consuming an entire tree
// *****************************************************************************


// Semantic data stored at a node
function (NodeData) getNodeData(struct MapNode node) {
  { key: node.key, value: node.value }
}

type_synonym RangedBST = { BST tree, IntervalOption range }
type_synonym RangedNode = {
  struct MapNode node,
  BST smaller,
  BST larger,
  Interval range
}

predicate [rec] RangedNode RangedNode(pointer root) {
   take node = Owned<struct MapNode>(root);
   take smaller = RangedBST(node.smaller);
   take larger  = RangedBST(node.larger);
   let rangeOpt = joinInterval(smaller.range, node.key, larger.range);
   assert (isIntervalSome(rangeOpt));
   return { node: node, smaller: smaller.tree, larger: larger.tree,
            range: fromIntervalOption(rangeOpt) };
}

// A binary search tree, and the interval for all its keys.
predicate [rec] RangedBST RangedBST(pointer root) {
  if (is_null(root)) {
    return { tree: Leaf {}, range: IntervalNone{} };
  } else {
    take node = RangedNode(root);
    let data = getNodeData(node.node);
    return { tree: Node { data: data, smaller: node.smaller, larger: node.larger },
             range: IntervalSome { i: node.range } };
  }
}

// An arbitrary binary search tree.
predicate BST BST(pointer root) {
  take result = RangedBST(root);
  return result.tree;
}
@*/


/* Allocate a new singleton node.
   Left unspecified: `map_insert` is the only function under test here, and
   `bst.pass.c` already tests this spec. */
struct MapNode *newNode(KEY key, VALUE value)
{
  struct MapNode *node = (struct MapNode*)cn_malloc(sizeof(struct MapNode));
  node->key = key;
  node->value = value;
  node->smaller = 0;
  node->larger = 0;
  return node;
}


/* Left unspecified: see `newNode` above. */
struct MapNode *findParent(struct MapNode **node, KEY key)
{
  struct MapNode *parent = 0;
  struct MapNode *cur = *node;
  while (cur)
  {
    KEY k = cur->key;
    if (k == key) {
      *node = cur;
      return parent;
    }
    parent = cur;
    cur = k < key? cur->larger : cur->smaller;
  }
  *node = cur;
  return parent;
}

/* Insert an element into a map. Overwrites previous if already present. */
void map_insert(struct MapNode **root, KEY key, VALUE value)
/*@
requires
  take root_ptr = Owned(root);
  take tree = BST(root_ptr);
ensures
  take new_root = Owned(root);
  take new_tree = BST(new_root);
  new_tree == insert(key, value, tree);
@*/
{
  struct MapNode *search = *root;
  struct MapNode *parent = findParent(&search, key);

  if (!parent) {
    *root = newNode(key,value);
    return;
  }

  struct MapNode *new_node = newNode(key,value);
  if (parent->key < key) {
    parent->larger = new_node;
  } else {
    parent->smaller = new_node;
  }
}
