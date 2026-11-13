//CN_VIP #include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include "cn_lemmas.h"
int x=1;
int main()
/*@ accesses x; @*/
{
  int *p = &x;
  int *q;
  /*CN_VIP*//*@ to_bytes RW<int*>(&p); @*/
  /*CN_VIP*//*@ to_bytes W<int*>(&q); @*/
  /*CN_VIP*//*@ focus RW<byte>, 0,1,2,3,4,5,6,7; @*/
  memcpy((byte*)&q, (byte*)&p, sizeof p);
  /*CN_VIP*//*@ from_bytes RW<int*>(&p); @*/
  /*CN_VIP*//*@ from_bytes RW<int*>(&q); @*/
  *q = 11; // is this free of undefined behaviour?
  //CN_VIP printf("*p=%d  *q=%d\n",*p,*q);
  /*CN_VIP*//*@ assert(*p == 11 && *q == 11); @*/
}
