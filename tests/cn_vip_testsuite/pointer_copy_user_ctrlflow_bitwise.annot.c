#include "refinedc.h"

//CN_VIP #include <stdio.h>
#include <inttypes.h>
#include <limits.h>

#include <stddef.h>
#include "cn_lemmas.h"
int x=1;
/*@
lemma now_same_value(integer i, integer j)
  requires true;
  ensures i == j;
@*/
int main()
/*CN_VIP*//*@ accesses x; @*/
{
  int *p = &x;
  uintptr_t i = (uintptr_t)p;
  //CN_VIP  int uintptr_t_width = sizeof(uintptr_t) * CHAR_BIT;
  /*CN_VIP*/size_t uintptr_t_width = sizeof(uintptr_t) * (size_t) CHAR_BIT;
  uintptr_t bit, j;
  j=0;
  /*CN_VIP*/int *q = NULL;
  /*CN_VIP*/bit=0;
  for (int k=0; k<uintptr_t_width; k++)
  /*@ inv i == (integer) p;
          ptr_eq(p, &x);
          uintptr_t_width == 64;
          (0 <= k) && (k <= 64);
//          let k_mask = shift_left(1, k) - 1;
//          j == i & k_mask;
  @*/
  {
    bit = (i & (((uintptr_t)1) << k)) >> k;
    if (bit == 1)
      j = j | ((uintptr_t)1 << k);
    else
      j = j;
  }
  /*@ apply now_same_value(i,j); @*/
#ifdef ANNOT
  q = copy_alloc_id(j, &x);
#else
  q = (int *)j;
#endif
  *q = 11; // CN VIP UB (no annot)
  //CN_VIP printf("*p=%d  *q=%d\n",*p,*q);
  /*CN_VIP*//*@ assert(*p == 11 && *q == 11); @*/
}
