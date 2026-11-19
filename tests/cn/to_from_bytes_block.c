[[cerb::byte]] typedef unsigned char byte;

void from_bytes(int *p)
/*@ 
requires 
    good<int*>(p);
    take X0 = W(array_shift<byte>(p, 0));
    take X1 = W(array_shift<byte>(p, 1));
    take X2 = W(array_shift<byte>(p, 2));
    take X3 = W(array_shift<byte>(p, 3));
ensures
    take Y = W(p);
@*/
{
    /*@ from_bytes W(p); @*/
}

void to_bytes(int *p)
/*@ 
requires 
    take Y = W(p);
ensures
    take X0 = W(array_shift<byte>(p, 0));
    take X1 = W(array_shift<byte>(p, 1));
    take X2 = W(array_shift<byte>(p, 2));
    take X3 = W(array_shift<byte>(p, 3));
@*/
{
    /*@ to_bytes W(p); @*/
}

int main()
{
    int x = 0;
    int *p = &x;
    to_bytes(p);
    from_bytes(p);
    to_bytes(p);
    from_bytes(p);
}
