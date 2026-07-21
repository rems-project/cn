#include "./headers.h"
#include "./append.h"
#include "./rev.h"
#include "./rev_lemmas.h"

struct sllist* rev_aux(struct sllist* l1, struct sllist* l2)
/*@ requires take L1 = SLList_At(l1);
             take L2 = SLList_At(l2);
    ensures take R = SLList_At(return);
            R == Append(RevList(L2), L1); @*/
{
  if (l2 == 0) {
    /*@ unfold RevList(L2); @*/
    /*@ unfold Append(Nil{}, L1); @*/
    return l1;
  } else {
    /*@ unfold RevList(L2); @*/
    /*@ apply Append_Cons_RList(RevList(Tl(L2)), Hd(L2), L1); @*/
    struct sllist *tmp = l2->tail;
    l2->tail = l1;
    return rev_aux(l2, tmp);
  }
}

struct sllist* rev(struct sllist* l1)
/*@ requires take L1 = SLList_At(l1);
    ensures take L1_Rev = SLList_At(return);
            L1_Rev == RevList(L1); @*/
{
  /*@ apply Append_Nil_RList(RevList(L1)); @*/
  return rev_aux (0, l1);
}
