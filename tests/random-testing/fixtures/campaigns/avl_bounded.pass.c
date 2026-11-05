// Regression test: recursive predicate threading key bounds through call
// arguments (`AVLAux(n.left, min, n.key - 1i32)`). Failures inside the callee
// blame its `min`/`max` parameters, whose values are compound expressions in
// the caller; without lifting those arguments into `let*` bindings, the blame
// matches no binder and generation discards until it gives up
// (FAILED TO GENERATE VALID INPUT). Unlike avl.pass.c, which asserts BST
// ordering in-frame via `range`, this shape exercises cross-call blame
// translation.

struct AVLNode {
  struct AVLNode *left;
  struct AVLNode *right;
  int key;
  int height;
};

/*@

type_synonym KEY = integer

datatype AVL {
  Leaf {},
  Node { KEY key, AVL smaller, AVL larger }
}

// Preserve the old signed 32-bit specification arithmetic at its boundaries.
function (integer) signed_height(integer value) {
  mod(value - MINi32(), MAXi32() - MINi32() + 1) + MINi32()
}

function [rec] (integer) height(datatype AVL tree) {
  match tree {
    Leaf {} => { 0 }
    Node { key: _, smaller: smaller, larger: larger } => {
      let hl = height(smaller);
      let hr = height(larger);
      signed_height(1 + ((hl > hr) ? hl : hr))
    }
  }
}

function (datatype AVL) node(datatype AVL left, KEY key, datatype AVL right) {
  Node { key: key, smaller: left, larger: right }
}

function (integer) balanceFactor(datatype AVL tree) {
  match tree {
    Node { key: _, smaller: l, larger: r } => {
      signed_height(height(l) - height(r))
    }
    Leaf {} => {
      0
    }
  }
}

predicate [rec] AVL AVLAux(pointer root, integer min, integer max) {
  if (is_null(root)) {
    return Leaf {};
  } else {
    take n = Owned<struct AVLNode>(root);
    assert (min <= n.key && n.key <= max);
    take left = AVLAux(n.left, min, n.key - 1);
    take right = AVLAux(n.right, n.key + 1, max);
    let hs = height(left);
    let hl = height(right);
    assert (n.height == signed_height(((hl > hs) ? hl : hs) + 1));
    assert (1 <= n.height);
    let T = node(left, n.key, right);
    let bf = balanceFactor(T);
    assert (-2 < bf && bf < 2);
    return T;
  }
}

predicate AVL UnbalancedAVL(pointer root) {
  take n = Owned<struct AVLNode>(root);
  assert (MINi32() + 1 <= n.key && n.key <= MAXi32() - 1);
  take left = AVLAux(n.left, MINi32() + 1, n.key - 1);
  take right = AVLAux(n.right, n.key + 1, MAXi32() - 1);
  let hs = height(left);
  let hl = height(right);
  assert (1 <= n.height);
  return node(left, n.key, right);
}
@*/

struct AVLNode *avl_id(struct AVLNode *x)
/*@
requires
  take T = UnbalancedAVL(x);
ensures
  take T_ = UnbalancedAVL(return);
  return == x;
@*/
{
  return x;
}
