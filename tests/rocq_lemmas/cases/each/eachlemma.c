/*@ 
  lemma array_lemma (pointer p, i32 n, i32 i)
    requires take A = each(i32 j; 0i32 <= j && j < n) { 
                        Owned<int>(array_shift<int>(p,j)) };
    ensures take A_post = Owned<int>(p); 
@*/


