#include "./headers.h"

/*@
function [rec] ({datatype List fst, datatype List snd}) 
                 split (datatype List xs)
{
  match xs {
    Nil {} => {
      {fst: Nil{}, snd: Nil{}}
    }
    Cons {Head: h1, Tail: Nil{}} => {
      {fst: Nil{}, snd: xs}
    }
    Cons {Head: h1, Tail: Cons {Head : h2, Tail : tl2 }} => {
      let P = split(tl2);
      {fst: Cons { Head: h1, Tail: P.fst},
       snd: Cons { Head: h2, Tail: P.snd}}
    }
  }
}

function [rec] (datatype List) merge(datatype List xs, datatype List ys) {
  match xs {
      Nil {} => { ys }
      Cons {Head: x, Tail: xs1} => {
        match ys {
          Nil {} => { xs }
          Cons{ Head: y, Tail: ys1} => {
            (x < y) ?
              (Cons{ Head: x, Tail: merge(xs1, ys) })
            : (Cons{ Head: y, Tail: merge(xs, ys1) })
          }
        }
      }
  }
}

function [rec] (datatype List) mergesort(datatype List xs) {
  match xs {
      Nil{} => { xs }
      Cons{Head: x, Tail: Nil{}} => { xs }
      Cons{Head: x, Tail: Cons{Head: y, Tail: zs}} => {
        let P = split(xs);
        let L1 = mergesort(P.fst);
        let L2 = mergesort(P.snd);
        merge(L1, L2)
      }
    }
}
@*/

struct sllist_pair {
  struct sllist* fst;
  struct sllist* snd;
};

struct sllist_pair split(struct sllist *xs)
/*@ requires take Xs = SLList_At(xs); @*/
/*@ ensures take Ys = SLList_At(return.fst); @*/
/*@ ensures take Zs = SLList_At(return.snd); @*/
/*@ ensures {fst: Ys, snd: Zs} == split(Xs); @*/
{
  if (xs == 0) {
    /*@ unfold split(Xs); @*/
    struct sllist_pair r = {.fst = 0, .snd = 0};
    return r;
  } else {
    struct sllist *cdr = xs -> tail;
    if (cdr == 0) {
      /*@ unfold split(Xs); @*/
      struct sllist_pair r = {.fst = 0, .snd = xs};
      return r;
    } else {
      /*@ unfold split(Xs); @*/
      struct sllist_pair p = split(cdr->tail);
      xs->tail = p.fst;
      cdr->tail = p.snd;
      struct sllist_pair r = {.fst = xs, .snd = cdr};
      return r;
    }
  }
}

struct sllist* merge(struct sllist *xs, struct sllist *ys)
/*@ requires take Xs = SLList_At(xs); @*/
/*@ requires take Ys = SLList_At(ys); @*/
/*@ ensures take Zs = SLList_At(return); @*/
/*@ ensures Zs == merge(Xs, Ys); @*/
{
  if (xs == 0) {
    /*@ unfold merge(Xs, Ys); @*/
    return ys;
  } else {
    /*@ unfold merge(Xs, Ys); @*/
    if (ys == 0) {
      /*@ unfold merge(Xs, Ys); @*/
      return xs;
    } else {
      /*@ unfold merge(Xs, Ys); @*/
      if (xs->head < ys->head) {
        struct sllist *zs = merge(xs->tail, ys);
        xs->tail = zs;
        return xs;
      } else {
        struct sllist *zs = merge(xs, ys->tail);
        ys->tail = zs;
        return ys;
      }
    }
  }
}

struct sllist* mergesort(struct sllist *xs)
/*@ requires take Xs = SLList_At(xs); @*/
/*@ ensures take Ys = SLList_At(return); @*/
/*@ ensures Ys == mergesort(Xs); @*/
{
  if (xs == 0) {
    /*@ unfold mergesort(Xs); @*/
    return xs;
  } else {
    struct sllist *tail = xs->tail;
    if (tail == 0) {
      /*@ unfold mergesort(Xs); @*/
      return xs;
    } else {
      /*@ unfold mergesort(Xs); @*/
      struct sllist_pair p = split(xs);
      p.fst = mergesort(p.fst);
      p.snd = mergesort(p.snd);
      return merge(p.fst, p.snd);
    }
  }
}
