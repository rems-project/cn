#include <assert.h>
#include <string.h>
#ifdef _RMAP_DEBUG
  #include <stdio.h>
#endif /* _RMAP_DEBUG */

#include <fulminate/rmap.h>

/* The entire rmap structure is parameterised by the type of range-query
 * results, defined below. Changing these definitions changes what
 * rmap_find_range produces.
 *
 * This type contains:
 *  - the type definition, result_t
 *  - a function `res_inject` lifting rmap_value_t into result_t
 *  - a constant `res_empty`, and a function `res_append`, which give an
 *    idempotent monoid over result_t (∀m, mm = m).
 *  - debugging functions `res_eq` and `res_print`.
 *
 */

typedef struct {
  rmap_value_t min;
  rmap_value_t max;
} result_t;

static const result_t res_empty = (result_t){.min = 1, .max = 0};

static inline bool res_is_empty(const result_t *a) {
  return a->min > a->max;
}

static inline result_t res_append(const result_t *a, const result_t *b) {
  bool a_ok = !res_is_empty(a), b_ok = !res_is_empty(b);
  if (a_ok && b_ok)
    return (result_t){.min = (a->min < b->min) ? a->min : b->min,
        .max = (a->max < b->max) ? b->max : a->max};
  return a_ok ? *a : (b_ok ? *b : res_empty);
}

static inline result_t res_inject(rmap_value_t v) {
  return (result_t){.min = v, .max = v};
}

#ifdef _RMAP_DEBUG

static inline bool res_eq(const result_t *a, const result_t *b) {
  bool a_ok = !res_is_empty(a), b_ok = !res_is_empty(b);
  return (!(a_ok || b_ok) || (a_ok && b_ok && a->max == b->max && a->min == b->min));
}

static void res_print(FILE *stream, const result_t *a) {
  if (res_is_empty(a))
    fprintf(stream, "-");
  else
    fprintf(stream, "min = %d, max = %d", a->min, a->max);
}

#endif /* _RMAP_DEBUG */

/***/

#define KEY_BITS (sizeof(rmap_key_t) * 8)

#define CACHED_NODES 32

typedef unsigned int bits_t;

enum node_st {
  EMPTY,
  LEAF,
  INNER,
  SKIP
};

struct node {
  enum node_st state;
  union {
    rmap_value_t leaf;
    struct {
      struct node *children;
      result_t qres;
    } inner;
    struct {
      struct node *child;
      rmap_key_t path;
      bits_t radix;
    } skip;
  };
};

struct rmap {
  bits_t radix;
  struct node root;
  malloc_f malloc;
  free_f free;
  struct {
    unsigned short cap;
    void *chunks[CACHED_NODES];
  } cache;
};

static const struct node EMPTY_NODE = (struct node){.state = EMPTY};
#define LEAF_NODE(v) ((struct node){.state = LEAF, .leaf = v})
#define INNER_NODE(xs, q)                                                                \
  ((struct node){.state = INNER, .inner.children = xs, .inner.qres = q})
#define SKIP_NODE(x, i, rx)                                                              \
  ((struct node){.state = SKIP, .skip.child = x, .skip.path = i, .skip.radix = rx})

rmap rmap_create(bits_t radix, malloc_f malloc, free_f free) {
  assert(KEY_BITS % radix == 0);

  rmap map = malloc(sizeof(struct rmap));
  if (!map)
    return NULL;
  *map =
      (struct rmap){.radix = radix, .root.state = EMPTY, .malloc = malloc, .free = free};
  return map;
}

static inline size_t asize(rmap map) {
  return 1 << map->radix;
}

static inline struct node *mk_block(rmap map) {
  if (map->cache.cap)
    return map->cache.chunks[--map->cache.cap];
  return map->malloc(sizeof(struct node) * asize(map));
}

static inline void free_block(struct node *xs, rmap map) {
  if (map->cache.cap < CACHED_NODES)
    map->cache.chunks[map->cache.cap++] = xs;
  else
    map->free(xs);
}

static inline struct node *mk_children(const struct node *node, rmap map) {
  struct node *res = mk_block(map);
  for (size_t i = 0; i < asize(map); i++)
    res[i] = *node;
  return res;
}

static inline struct node *mk_child(const struct node *node, rmap map) {
  struct node *res = mk_block(map);
  res[0] = *node;
  return res;
}

static void drop_node(struct node *node, rmap map) {
  if (node->state == INNER) {
    for (size_t i = 0; i < asize(map); i++)
      drop_node(&node->inner.children[i], map);
    free_block(node->inner.children, map);
  } else if (node->state == SKIP) {
    drop_node(node->skip.child, map);
    free_block(node->skip.child, map);
  }
}

void rmap_free(rmap map) {
  if (map == NULL)
    return;
  drop_node(&map->root, map);
  while (map->cache.cap)
    map->free(map->cache.chunks[--map->cache.cap]);
  map->free(map);
}

static inline rmap_key_t mask(bits_t bits) {
  return bits == KEY_BITS ? -1ULL : ((1ULL << bits) - 1ULL);
}

static size_t key_to_i(bits_t bits, bits_t radix, rmap_key_t k) {
  return (k >> (KEY_BITS - bits - radix)) & mask(radix);
}

static rmap_key_t i_to_key(bits_t bits, rmap_key_t k, bits_t radix, rmap_key_t i) {
  rmap_key_t m = mask(KEY_BITS - bits);
  return (((~m) & k) | (i << (KEY_BITS - bits - radix)));
}

static rmap_key_t min_key(bits_t bits, rmap_key_t k) {
  rmap_key_t m = mask(KEY_BITS - bits);
  return (~m) & k;
}

static rmap_key_t max_key(bits_t bits, rmap_key_t k) {
  rmap_key_t m = mask(KEY_BITS - bits);
  return ((~m) & k) | (m & -1ULL);
}

static bool complete_range(bits_t bits, rmap_key_t k0, rmap_key_t k1) {
  rmap_key_t m = mask(KEY_BITS - bits);
  return ((k0 & m) == 0UL) && ((k1 & m) == (-1UL & m));
}

static inline rmap_key_t min_k(rmap_key_t a, rmap_key_t b) {
  return (a < b) ? a : b;
}

static inline rmap_key_t max_k(rmap_key_t a, rmap_key_t b) {
  return (a < b) ? b : a;
}

static const rmap_value_opt_t NONE_VALUE = (rmap_value_opt_t){.defined = false};
#define SOME_VALUE(x) ((rmap_value_opt_t){.defined = true, .v = x})

rmap_value_opt_t rmap_find(rmap_key_t k, rmap map) {
  bits_t radix = map->radix, bits = 0;
  struct node *node = &map->root;
  while (bits <= KEY_BITS)
    switch (node->state) {
      case EMPTY:
        return NONE_VALUE;
      case LEAF:
        return SOME_VALUE(node->leaf);
      case INNER:
        node = &node->inner.children[key_to_i(bits, radix, k)];
        bits += radix;
        break;
      case SKIP:
        if (key_to_i(bits, node->skip.radix, k) != node->skip.path)
          return NONE_VALUE;
        bits += node->skip.radix;
        node = node->skip.child;
        break;
      default:
        assert(false);
    }
  return NONE_VALUE;
}

static result_t node_inject(const struct node *n) {
  switch (n->state) {
    case EMPTY:
      return res_empty;
    case LEAF:
      return res_inject(n->leaf);
    case INNER:
      return n->inner.qres;
    case SKIP:
      return node_inject(n->skip.child);
    default:
      assert(false);
  }
}

static result_t find_range(
    bits_t bits, rmap_key_t k0, rmap_key_t k1, const struct node *node, rmap map) {
  size_t i0, i1;
  result_t acc = res_empty;
  switch (node->state) {
    case EMPTY:
      return res_empty;
    case LEAF:
      return res_inject(node->leaf);
    case INNER:
      if (complete_range(bits, k0, k1))
        return node->inner.qres;

      i0 = key_to_i(bits, map->radix, k0);
      i1 = key_to_i(bits, map->radix, k1);
      bits += map->radix;
      for (size_t i = i0; i <= i1; ++i) {
        struct node *n = &node->inner.children[i];
        result_t v;
        if (i == i0 && i == i1)
          v = find_range(bits, k0, k1, n, map);
        else if (i == i0)
          v = find_range(bits, k0, max_key(bits, k0), n, map);
        else if (i == i1)
          v = find_range(bits, min_key(bits, k1), k1, n, map);
        else
          v = node_inject(n);
        acc = res_append(&acc, &v);
      }
      return acc;
    case SKIP: {
      rmap_key_t path = node->skip.path;
      bits_t radix = node->skip.radix;

      if (key_to_i(bits, radix, k0) > path || key_to_i(bits, radix, k1) < path)
        return res_empty;
      k0 = max_k(k0, min_key(bits + radix, i_to_key(bits, k0, radix, path)));
      k1 = min_k(k1, max_key(bits + radix, i_to_key(bits, k1, radix, path)));
      return find_range(bits + radix, k0, k1, node->skip.child, map);
    }
    default:
      assert(false);
  }
}

static const rmap_range_res_t NONE_RESULT = (rmap_range_res_t){.defined = false};
#define SOME_RESULT(a, b) ((rmap_range_res_t){.defined = true, .min = a, .max = b})

rmap_range_res_t rmap_find_range(rmap_key_t k0, rmap_key_t k1, rmap map) {
  if (k0 == k1) {
    rmap_value_opt_t res = rmap_find(k0, map);
    return !res.defined ? NONE_RESULT : SOME_RESULT(res.v, res.v);
  }
  assert(k0 < k1);
  result_t res = find_range(0, k0, k1, &map->root, map);
  return res_is_empty(&res) ? NONE_RESULT : SOME_RESULT(res.min, res.max);
}

static inline bool eq_fringe_node(const struct node *n1, const struct node *n2) {
  switch (n1->state) {
    case EMPTY:
      return n2->state == EMPTY;
    case LEAF:
      return n2->state == LEAF && n1->leaf == n2->leaf;
    default:
      return false;
  }
}

static struct node *children_to_child(struct node *children, size_t i, rmap map) {
  assert(i < asize(map));
  struct node *child = mk_child(&children[i], map);
  free_block(children, map);
  return child;
}

static struct node *child_to_children(struct node *child, size_t i, rmap map) {
  assert(i < asize(map));
  struct node *children = mk_children(&EMPTY_NODE, map);
  children[i] = child[0];
  free_block(child, map);
  return children;
}

static bool skip_to_radix(struct node *node, rmap map) {
  assert(node->state == SKIP);
  if (node->skip.radix > map->radix) {
    bits_t rx1 = node->skip.radix - map->radix;
    struct node next = SKIP_NODE(node->skip.child, node->skip.path & mask(rx1), rx1);
    *node = SKIP_NODE(mk_child(&next, map), node->skip.path >> rx1, map->radix);
    return true;
  }
  assert(node->skip.radix == map->radix);
  return false;
}

static struct node new_skip(struct node *child, rmap_key_t path, bits_t rx, rmap map) {
  struct node c = *child;
  if (c.state == SKIP) {
    c.skip.path |= (path << c.skip.radix);
    c.skip.radix += rx;
  }
  if (c.state == EMPTY || c.state == SKIP) {
    free_block(child, map);
    return c;
  }
  return SKIP_NODE(child, path, rx);
}

#define NOWHERE (-1UL)

static struct node new_inner(struct node *children, result_t q, bool collapse, rmap map) {
  struct node node = INNER_NODE(children, q);

  if (res_is_empty(&q)) {
    struct node node0 = children[0];
    result_t acc = node_inject(&node0);
    bool same = true;

    for (size_t i = 1; i < asize(map); i++) {
      result_t v = node_inject(&children[i]);
      acc = res_append(&acc, &v);
      same = same && eq_fringe_node(&node0, &children[i]);
    }

    if (same) {
      free_block(children, map);
      return node0;
    }
    node = INNER_NODE(children, acc);
  }

  if (collapse && node.state == INNER) {
    size_t j = NOWHERE;
    for (size_t i = 0; i < asize(map); i++)
      if (node.inner.children[i].state != EMPTY) {
        if (j != NOWHERE)
          return node;
        j = i;
      }

    if (j != NOWHERE)
      return new_skip(children_to_child(children, j, map), j, map->radix, map);
  }
  return node;
}

static inline result_t res_inject_2(rmap_value_t a, rmap_value_t b) {
  result_t r1 = res_inject(a), r2 = res_inject(b);
  return res_append(&r1, &r2);
}

static void put_leaf(bits_t bits,
    rmap_key_t k0,
    rmap_key_t k1,
    const struct node *newn,
    struct node *node,
    rmap map) {
  assert(bits <= KEY_BITS);
  assert(newn->state == EMPTY || newn->state == LEAF);

  if (node->state == newn->state && (node->state == EMPTY || node->leaf == newn->leaf))
    return;

  if (complete_range(bits, k0, k1)) {
    drop_node(node, map);
    *node = *newn;
    return;
  }

  struct node *children;

  if (node->state == EMPTY || node->state == LEAF)
    children = mk_children(node, map);
  else if (node->state == INNER)
    children = node->inner.children;
  else if (node->state == SKIP) {
    do {
      rmap_key_t path = node->skip.path;
      bits_t radix = node->skip.radix;
      if (key_to_i(bits, radix, k0) == path && key_to_i(bits, radix, k1) == path) {
        put_leaf(bits + radix, k0, k1, newn, node->skip.child, map);
        *node = new_skip(node->skip.child, path, radix, map);
        return;
      }
    } while (skip_to_radix(node, map));
    children = child_to_children(node->skip.child, node->skip.path, map);
  } else
    assert(false);

  assert(children);

  size_t i0 = key_to_i(bits, map->radix, k0), i1 = key_to_i(bits, map->radix, k1);
  bits += map->radix;

  for (size_t i = i0; i <= i1; ++i) {
    if (i == i0 && i == i1)
      put_leaf(bits, k0, k1, newn, &children[i], map);
    else if (i == i0)
      put_leaf(bits, k0, max_key(bits, k0), newn, &children[i], map);
    else if (i == i1)
      put_leaf(bits, min_key(bits, k1), k1, newn, &children[i], map);
    else if (i0 < i && i < i1) {
      drop_node(&children[i], map);
      children[i] = *newn;
    }
  }

  if (node->state == LEAF && newn->state == LEAF)
    *node = new_inner(children, res_inject_2(node->leaf, newn->leaf), false, map);
  else if (node->state == LEAF && newn->state == EMPTY)
    *node = new_inner(children, res_inject(node->leaf), true, map);
  else if (node->state == EMPTY && newn->state == LEAF)
    *node = new_inner(children, res_inject(newn->leaf), true, map);
  else if (node->state == INNER || node->state == SKIP)
    *node = new_inner(children, res_empty, true, map);
  else
    assert(false);
}

void rmap_add(rmap_key_t k0, rmap_key_t k1, rmap_value_t v, rmap map) {
  assert(k0 <= k1);
  struct node newn = LEAF_NODE(v);
  put_leaf(0, k0, k1, &newn, &map->root, map);
}

void rmap_remove(rmap_key_t k0, rmap_key_t k1, rmap map) {
  assert(k0 <= k1);
  struct node newn = EMPTY_NODE;
  put_leaf(0, k0, k1, &newn, &map->root, map);
}

#ifdef _RMAP_DEBUG

static bool node_is_wf_1(struct node node, rmap map) {
  switch (node.state) {
    case EMPTY:
    case LEAF:
      return true;
    case INNER:
      for (size_t i = 0; i < asize(map); i++)
        if (!eq_fringe_node(&node.inner.children[0], &node.inner.children[i]))
          goto next;
      return false;
    next:
      for (size_t i = 0; i < asize(map); i++)
        if (!node_is_wf_1(node.inner.children[i], map))
          return false;
      return true;
    case SKIP:
      return node_is_wf_1(node.skip.child[0], map);
    default:
      assert(false);
  }
}

static bool node_is_wf_2(struct node node, rmap map) {
  result_t acc = res_empty;
  switch (node.state) {
    case EMPTY:
    case LEAF:
      return true;
    case INNER:
      for (size_t i = 0; i < asize(map); i++) {
        result_t v = node_inject(&node.inner.children[i]);
        acc = res_append(&acc, &v);
      }
      if (!res_eq(&acc, &node.inner.qres))
        return false;
      for (size_t i = 0; i < asize(map); i++)
        if (!node_is_wf_2(node.inner.children[i], map))
          return false;
      return true;
    case SKIP:
      return node_is_wf_2(node.skip.child[0], map);
    default:
      assert(false);
  }
}

static bool node_is_wf_3(struct node node, rmap map) {
  size_t legs = 0;
  switch (node.state) {
    case EMPTY:
    case LEAF:
      return true;
    case INNER:
      for (size_t i = 0; i < asize(map); i++)
        if (node.inner.children[i].state != EMPTY)
          ++legs;
      if (legs < 2)
        return false;
      for (size_t i = 0; i < asize(map); i++)
        if (!node_is_wf_3(node.inner.children[i], map))
          return false;
      return true;
    case SKIP:
      return node.skip.child->state != EMPTY && node.skip.child->state != SKIP &&
             node_is_wf_3(node.skip.child[0], map);
    default:
      assert(false);
  }
}

bool rmap_is_wf(rmap map) {
  return node_is_wf_1(map->root, map) && node_is_wf_2(map->root, map) &&
         node_is_wf_3(map->root, map);
}

typedef struct {
  bool compress;
  enum branch {
    DONE,
    SPAN,
    END
  } branches[KEY_BITS];
} ctx_t;

static void dump_node(
    FILE *stream, rmap_key_t addr, bits_t bits, struct node node, ctx_t *ctx, rmap map) {
  rmap_key_t k0 = min_key(bits, addr), k1 = max_key(bits, addr);
  int depth = bits / map->radix;
  if (ctx->compress && node.state == EMPTY)
    return;
  for (int i = 0; i < depth; i++)
    switch (ctx->branches[i]) {
      case SPAN:
        fprintf(stream, (i < depth - 1) ? "│ " : "├ ");
        break;
      case END:
        assert(i == depth - 1);
        ctx->branches[i] = DONE;
        fprintf(stream, "└ ");
        break;
      default:
        fprintf(stream, "  ");
    }
  switch (node.state) {
    case EMPTY:
      fprintf(stream, "ø\n");
      break;
    case LEAF:
      if (k0 == k1)
        fprintf(stream, "0x%016lx : %d\n", k0, node.leaf);
      else
        fprintf(stream, "0x%016lx - 0x%016lx : %d\n", k0, k1, node.leaf);
      break;
    case INNER: {
      size_t last = asize(map) - 1;
      fprintf(stream, "0x%016lx - 0x%016lx (q: ", k0, k1);
      res_print(stream, &node.inner.qres);
      fprintf(stream, ")\n");
      ctx->branches[depth] = SPAN;
      while (ctx->compress && node.inner.children[last].state == EMPTY)
        --last;
      for (size_t i = 0; i <= last; i++) {
        rmap_key_t addr1 =
            ((i & mask(map->radix)) << (KEY_BITS - bits - map->radix)) | addr;
        if (i == last)
          ctx->branches[depth] = END;
        dump_node(stream, addr1, bits + map->radix, node.inner.children[i], ctx, map);
      }
      break;
    }
    case SKIP: {
      fprintf(stream,
          "0x%016lx - 0x%016lx --> (0x%lx, radix = %d)\n",
          k0,
          k1,
          node.skip.path,
          node.skip.radix);
      rmap_key_t addr1 = ((node.skip.path & mask(node.skip.radix))
                             << (KEY_BITS - bits - node.skip.radix)) |
                         addr;
      dump_node(stream, addr1, bits + node.skip.radix, node.skip.child[0], ctx, map);
      break;
    }
    default:
      assert(false);
  }
}

void dump(FILE *stream, bool compress, rmap map) {
  ctx_t ctx = {.compress = compress};
  if (map->root.state == EMPTY)
    fprintf(stream, "ø\n");
  else
    dump_node(stream, 0, 0, map->root, &ctx, map);
}

#endif /* _RMAP_DEBUG */
