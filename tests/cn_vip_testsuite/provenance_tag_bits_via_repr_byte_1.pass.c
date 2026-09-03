// NOTE: terminates with cvc5 but not Z3
#include <assert.h>
//CN_VIP #include <stdio.h>
#include <stdint.h>
/*CN_VIP*/ [[cerb::byte]] typedef unsigned char byte;
int x=1;
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
int main()
/*@
accesses
    x;

requires
    x & 3 == 0;
@*/
{
  int *p=&x, *q=&x;
  // read low-order (little endian) representation byte of p
  /*CN_VIP*//*@ to_bytes RW<int*>(&p); @*/
  byte* p_char = (byte*)&p;
  /*@ focus RW<byte>, 0; @*/
  unsigned char i = (unsigned char)*p_char;
  // check the bottom two bits of an int* are not usec
  assert(_Alignof(int) >= 4);
  /*@ apply and_rem(i); @*/
  assert((i & 3u) == 0u);
  /*@ apply or_plus(i); @*/
  // set the low-order bit of the byte
  i = i | 1u;
  // write the representation byte back
  *p_char = (byte)i;
  // [p might be passed around or copied here]
  // clear the low-order bits again
  /*@ apply and_not_div((integer) p_char); @*/
  *(byte*)&p = (byte)((unsigned char)(*(byte*)&p) & ~3u);
  // are p and q now equivalent?
  /*CN_VIP*//*@ from_bytes RW<int*>(&p); @*/
  *p = 11;          // does this have defined behaviour?
  _Bool b = (p==q); // is this true?
  //CN_VIP printf("x=%i *p=%i (p==q)=%s\n",x,*p,b?"true":"false");
  /*CN_VIP*//*@ assert(x == 11 && *p == 11 && ptr_eq(p, q)); @*/
}

