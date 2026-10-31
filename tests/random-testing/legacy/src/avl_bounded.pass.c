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

type_synonym KEY = i32

datatype AVL {
  Leaf {},
  Node { KEY key, AVL smaller, AVL larger }
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
  Node { key: key, smaller: left, larger: right }
}

function (i32) balanceFactor(datatype AVL tree) {
  match tree {
    Node { key: _, smaller: l, larger: r } => {
      height(l) - height(r)
    }
    Leaf {} => {
      0i32
    }
  }
}

predicate [rec] AVL AVLAux(pointer root, i32 min, i32 max) {
  if (is_null(root)) {
    return Leaf {};
  } else {
    take n = Owned<struct AVLNode>(root);
    assert (min <= n.key && n.key <= max);
    take left = AVLAux(n.left, min, n.key - 1i32);
    take right = AVLAux(n.right, n.key + 1i32, max);
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

predicate AVL UnbalancedAVL(pointer root) {
  take n = Owned<struct AVLNode>(root);
  assert (MINi32() + 1i32 <= n.key && n.key <= MAXi32() - 1i32);
  take left = AVLAux(n.left, MINi32() + 1i32, n.key - 1i32);
  take right = AVLAux(n.right, n.key + 1i32, MAXi32() - 1i32);
  let hs = height(left);
  let hl = height(right);
  assert (1i32 <= n.height);
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
