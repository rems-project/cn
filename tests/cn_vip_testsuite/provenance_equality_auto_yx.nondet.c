//CN_VIP #include <stdio.h>
//CN_VIP #include <string.h>
#include "cn_lemmas.h"
int main() {
  int y=2, x=1;
  /*CN_VIP*//*@ apply assert_equal((integer)&y, (integer)&x + sizeof<int>); @*/
  int *p = &x + 1;
  int *q = &y;
  //CN_VIP printf("Addresses: p=%p q=%p\n",(void*)p,(void*)q);
  _Bool b = (p==q);
  //CN_VIP printf("(p==q) = %s\n", b?"true":"false");
#if defined(NON_DET_TRUE)
  /*CN_VIP*//*@ assert (b == 1); @*/ // non-det in PNVI-ae-udi; true in VIP
#elif defined(NON_DET_FALSE)
  /*CN_VIP*//*@ assert (b == 0); @*/ // non-det in PNVI-ae-udi; true in VIP
#else
  /*CN_VIP*//*@ assert (b == 0 || b == 1); @*/ // non-det in PNVI-ae-udi; true in VIP
#endif
  return 0;
}
