int foo(int);
/*@ spec foo(integer y);
requires
    y < MAXi32();
ensures 
    return == y + 1;
@*/

int foo(int x)
{
    /*@ assert (x == y); @*/
    x = x + 1;
    /*@ assert (x == y + 1); @*/
    return x;
}

int main()
/*@ trusted; @*/
{
    foo(1001);
    return 0;
}
