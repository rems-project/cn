[[cerb::byte]] typedef unsigned char byte;

void f(byte *p)
/*@
requires
    take x = each (integer i; 0 <= i && i < sizeof<int>) { RW(array_shift(p, i)) };
    each (integer i: 0,3; is_none(x[i]));
ensures
    take x2 = each (integer i; 0 <= i && i < sizeof<int>) { RW(array_shift(p, i)) };
    x == x2;
@*/
{
    /*@ focus RW<byte>, 0; @*/
    (unsigned char)*p;
}

int main() {
  int x;
  /*@ to_bytes W<int>(&x); @*/
  /*@ focus RW<byte>, 0,1,2,3; @*/
  f((byte*)&x);
  /*@ from_bytes RW<int>(&x); @*/
}
