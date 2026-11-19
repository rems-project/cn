
/*@
lemma struct_lemma (pointer p)
  requires 
    take P = Owned<struct point>(p);
  ensures 
    take P_post = Owned<struct point>(p);
    P == P_post;
@*/