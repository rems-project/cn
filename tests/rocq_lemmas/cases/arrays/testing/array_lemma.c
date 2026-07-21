#include "./headers.h" 

/*@
lemma each_combine (pointer p, i32 n, i32 m)
  requires 
    take A1 = each(i32 j; 0i32 <= j && j < n) { 
                        Owned<int>(array_shift<int>(p,j)) };
    take A2 = each(i32 j; n <= j && j < (m + n)) { 
                        Owned<int>(array_shift<int>(p,j)) };
    n >= 0i32; n <= MAXi32();
    m >= 0i32; m <= MAXi32();
    m + n <= MAXi32();
  ensures 
    take A_post = each(i32 j; 0i32 <= j && j < (m + n)) { 
        Owned<int>(array_shift<int>(p,j)) 
        };
@*/

