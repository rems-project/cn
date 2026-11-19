#include "./headers.h" 

/*@
lemma each_lemma (pointer p, i32 n)
  requires 
    take v = Owned<int>(p);
    take A = each(i32 j; 1i32 <= j && j < n) 
              { Owned<int>(array_shift<int>(p,j)) };
    1i32 <= n; 
    n <= MAXi32();
  ensures 
    take A_post = each(i32 j; 0i32 <= j && j < n) 
              { Owned<int>(array_shift<int>(p,j)) };
    A_post[0i32] == v;
@*/

/*@
lemma each_concrete (pointer p)
  requires 
    take A = each(i32 j; 0i32 <= j && j < 2i32) { 
                        Owned<int>(array_shift<int>(p,j)) };
        take v = Owned<int>(array_shift<int>(p,2i32));
  ensures 
    take A_post = each(i32 j; 0i32 <= j && j < 3i32) { 
                        Owned<int>(array_shift<int>(p,j)) };
@*/

/*@
lemma each_concrete2 (pointer p)
  requires 
      take v = Owned<int>(p);
      take A = each(i32 j; 0i32 <= j && j < 2i32) { 
                        Owned<int>(array_shift<int>(array_shift<int>(p,1i32),j)) };
        
  ensures 
    take A_post = each(i32 j; 0i32 <= j && j < 3i32) { 
    Owned<int>(array_shift<int>(p,j)) };
@*/
