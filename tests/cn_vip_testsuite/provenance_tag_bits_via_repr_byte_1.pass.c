// NOTE: terminates with cvc5 but not Z3
#include <assert.h>
//CN_VIP #include <stdio.h>
#include <stdint.h>
/*CN_VIP*/ [[cerb::byte]] typedef unsigned char byte;
int x=1;
int main()
/*@
accesses
    x;

requires
    bw_and_uf((u32)x, 3u32) == 0u32;
@*/
{
  int *p=&x, *q=&x;
  // read low-order (little endian) representation byte of p
  /*CN_VIP*//*@ to_bytes RW<int*>(&p); @*/
  byte* p_char = (byte*)&p;
  /*@ focus RW<byte>, 0u64; @*/
  unsigned char i = (unsigned char)*p_char;
  // check the bottom two bits of an int* are not usec
  assert(_Alignof(int) >= 4);
  assert((i & 3u) == 0u);
  // set the low-order bit of the byte
  i = i | 1u;
  // write the representation byte back
  *p_char = (byte)i;
  // [p might be passed around or copied here]
  // clear the low-order bits again
  *(byte*)&p = (byte)((unsigned char)(*(byte*)&p) & ~3u);
  // are p and q now equivalent?
  /*CN_VIP*//*@ from_bytes RW<int*>(&p); @*/
  *p = 11;          // does this have defined behaviour?
  _Bool b = (p==q); // is this true?
  //CN_VIP printf("x=%i *p=%i (p==q)=%s\n",x,*p,b?"true":"false");
  /*CN_VIP*//*@ assert(x == 11i32 && *p == 11i32 && ptr_eq(p, q)); @*/
}
