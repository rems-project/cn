// NOTE: terminates with cvc5 but not Z3
#include "refinedc.h"

/*@ 

lemma and_rem(integer i)
  requires i >= 0;
  ensures i & 3 == rem(i,4);

lemma or_plus(integer i)
  requires i >= 0; rem(i,4) == 0;
  ensures i | 1 == i+1;

lemma and_not_div(integer i)
  requires 0 <= i; i <= MAXu64();
  ensures i & (MAXu64() - 3) == i - rem(i,4);

@*/


#include <assert.h>
//CN_VIP #include <stdio.h>
#include <stdint.h>
int x=1;
int main()
/*CN_VIP*//*@ accesses x; @*/
{
  int *p = &x;
  // cast &x to an integer
  uintptr_t i = (uintptr_t) p;
  // check the bottom two bits of an int* are not used
  assert(_Alignof(int) >= 4);
  /*@ apply and_rem(i); @*/
  assert((i & 3u) == 0u);
  // construct an integer like &x with low-order bit set
  /*@ apply or_plus(i); @*/
  i = i | 1u;
  // cast back to a pointer
#ifdef ANNOT
  int *q = copy_alloc_id(i, p);
#else
  int *q = (int *) i; // does this have defined behaviour?
#endif
  // cast to integer and mask out the low-order two bits
  /*@ apply and_not_div((integer) q); @*/
  uintptr_t j = ((uintptr_t)q) & ~((uintptr_t)3u);
  // cast back to a pointer
#ifdef ANNOT
  int *r = copy_alloc_id(j, p);
#else
  int *r = (int *) j;
#endif
  // are r and p now equivalent?
  *r = 11;           //  CN VIP UB (no annot)
  _Bool b = (r==p);  //  is this true?
  //CN_VIP printf("x=%i *r=%i (r==p)=%s\n",x,*r,b?"true":"false");
  /*CN_VIP*//*@ assert(x == 11 && *r == 11 && b == 1); @*/
}
