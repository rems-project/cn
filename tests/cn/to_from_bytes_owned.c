[[cerb::byte]] typedef unsigned char byte;

int main()
{
    int x = 0;
    int *p = &x;
    /*@ to_bytes RW(p); @*/
    byte *p_char = (byte*)p;

    // This could be allowed by extending Cerberus' elaboration and memory
    // interface with to/from byte casts, but for now it's not a priority.
    // /*@ focus RW<byte>, 2u64; @*/
    // p_char[2] = 0xff;

    /*@ from_bytes RW<int>(p); @*/
    /*@ assert (x == 0i32); @*/
    /*@ to_bytes RW<int>(p); @*/
    /*@ from_bytes RW<int>(p); @*/
    /*@ assert (x == 0i32); @*/
}
