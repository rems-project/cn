#include "./headers.h" 

/*@
lemma array_lemma (pointer p, i32 n, i32 m)
  requires 
    take vs = Array(p,n);
    take ws = Array(array_shift<unsigned int>(p,n),m);
  ensures 
    take xs = Array(p,n+m);
    xs == Append(vs,ws);
@*/