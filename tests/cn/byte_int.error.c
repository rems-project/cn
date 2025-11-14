[[cerb::byte]] typedef unsigned char byte;

void f(byte *p)
/*@
requires
    take x = each (u64 i; i < sizeof<int>) { RW(array_shift(p, i)) };
    each (u64 i: 0,7; is_none(x[i]));
ensures
    take x2 = each (u64 i; i < sizeof<int>) { RW(array_shift(p, i)) };
    x == x2;
@*/
{
    /*@ focus RW<byte>, 0u64; @*/
    (unsigned char)*p;
}

int main() {
  int x;
  /*@ to_bytes W<int>(&x); @*/
  f((byte*)&x);
  /*@ from_bytes RW<int>(&x); @*/
}
