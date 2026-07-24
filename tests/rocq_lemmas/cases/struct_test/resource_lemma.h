/*@
lemma resource_lemma (pointer p)
  requires 
      take v1 = Owned<int>(p);
      (i32) v1 == 0i32;
  ensures 
      take v2 = Owned<int>(p);
      (i32) v2 == 0i32;
@*/