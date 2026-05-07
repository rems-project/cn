#include <stdlib.h>
// #include <stdio.h>

struct AVLNode {
  struct AVLNode *left;
  struct AVLNode *right;
  int key;
  int height;
};

/*@

type_synonym KEY = i32

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




// // A binary search tree
datatype AVL {
  Leaf {},
  Node { KEY key, AVL smaller, AVL larger }
}

function (boolean) hasRoot(KEY k, datatype AVL tree) {
  match tree {
    Leaf {} => { false }
    Node { key: key, smaller: _, larger: _ } => { k == key }
  }
}

function (boolean) isLeaf(datatype AVL tree) {
  match tree {
    Leaf {} => { true }
    Node { key: _, smaller: _, larger: _ } => { false }
  }
}

function (datatype AVL) getLeft(datatype AVL tree) {
  match tree {
    Leaf {} => { Leaf {} }
    Node { key: _, smaller: l, larger: _ } => { l }
  }
}

function (datatype AVL) getRight(datatype AVL tree) {
  match tree {
    Leaf {} => { Leaf {} }
    Node { key: _, smaller: _, larger: r } => { r }
  }
}

function [rec] (boolean) member(KEY k, AVL tree) {
  match tree {
    Leaf {} => { false }
    Node { key: key, smaller: smaller, larger: larger } => {
      key == k ||
      k < key && member(k,smaller) ||
      k > key && member(k,larger)
    }
  }
}

function [rec] (i32) height(datatype AVL tree) {
  match tree {
    Leaf {} => { 0i32 }
    Node { key: _, smaller: smaller, larger: larger } => {
      let hl = height(smaller);
      let hr = height(larger);
      1i32 + ((hl > hr) ? hl : hr)
    }
  }
}

function (datatype AVL) node(datatype AVL left, KEY key, datatype AVL right) {
  // let hl = height(left);
  // let hr = height(right);
  // let height = 1i32 + ((hl > hr) ? hl : hr);
  Node { key: key, smaller: left, larger: right }
}

function [rec] (IntervalOption) range (datatype AVL tree) {
  match tree {
    Leaf {} => { IntervalNone {} }
    Node { key: key, smaller: smaller, larger: larger } => {
      joinInterval(range(smaller), key, range(larger))
    }
  }
}

// *****************************************************************************
// Consuming an entire tree
// *****************************************************************************

function (i32) balanceFactor(datatype AVL tree) {
  match tree {
    Node {
      key: _,
      smaller: l,
      larger: r
    } => {
      height(l) - height(r)
    }
    Leaf {} => {
      0i32
    }
  }
}

// An arbitrary AVL tree.
predicate [rec] (datatype AVL) AVLTree(pointer root) {
  if (is_null(root)) {
    return Leaf {};
  } else {
    take n = Owned<struct AVLNode>(root);
    take left = AVLTree(n.left);
    take right  = AVLTree(n.right);
    let rangeOpt = joinInterval(range(left), n.key, range(right));
    assert (isIntervalSome(rangeOpt));
    let hs = height(left);
    let hl = height(right);
    assert (n.height == ((hl > hs) ? hl : hs) + 1i32);
    assert (1i32 <= n.height);
    let T = node(left, n.key, right);
    let bf = balanceFactor(T);
    assert (-2i32 < bf && bf < 2i32);
    return T;
  }
}

predicate (datatype AVL) UnbalancedAVL(pointer root) {
  take n = Owned<struct AVLNode>(root);
  take left = AVLTree(n.left);
  take right  = AVLTree(n.right);
  let rangeOpt = joinInterval(range(left), n.key, range(right));
  assert (isIntervalSome(rangeOpt));
  let hs = height(left);
  let hl = height(right);
  assert (1i32 <= n.height);
  return node(left, n.key, right);
}

lemma avl_nonegative_height(pointer x)
requires
  take T = AVLTree(x);
ensures
  take T_ = AVLTree(x);
  T == T_;
  height(T) >= 0i32;

lemma unbalancedavl_nonegative_height(pointer x)
requires
  take T = UnbalancedAVL(x);
ensures
  take T_ = UnbalancedAVL(x);
  T == T_;
  height(T) >= 0i32;
@*/

int get_height(struct AVLNode *x)
/*@
requires
  take T = AVLTree(x);
ensures
  take T_ = AVLTree(x);
  T == T_;
  return == height(T);
  return >= 0i32;
@*/
{
  /*@ unfold height(T); @*/
  return x == 0 ? 0 : x->height;
}
