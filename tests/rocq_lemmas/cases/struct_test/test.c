#include "./headers.h" 

void do_nothing (struct point *p) 
/*@ requires 
    take P = Owned<struct point>(p);
  ensures 
    take P_post = Owned<struct point>(p);
    P == P_post;
@*/
{
  /*@ apply struct_lemma (p); @*/
  *p;
}

int read (int *p)
/*@ requires 
      take P = Owned<int>(p);
      (i32) P == 0i32;
  ensures 
      take Q = Owned<int>(p);
      (i32) Q == 0i32;
@*/
{
  /*@ apply resource_lemma (p); @*/
  return *p;
}