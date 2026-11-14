/* A pointer-to-integer conversion out of range stays undefined
   (C11 6.3.2.3p6) under --impl=x86_64; only integer conversions wrap.
   (The default-profile version is tests/cn/pointer_to_char_cast.c, which
   passes there because 128..255 fits an unsigned char.) */
#include<inttypes.h>
void f() {
  int x=1;
  if ((128 <= (intptr_t)&x && (intptr_t) &x <= 255))
    (char)&x;
  return;
}

int main(void) {
  f();
  return 0;
}
