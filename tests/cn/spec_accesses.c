int y;
int z;

int foo(int);
/*@ spec foo(integer x);
accesses y;
requires
    x >= 0;
    y >= 0;
    x < MAXi32() / 2;
    y < MAXi32() / 2;
ensures
    return == x + y;
@*/

int foo(int x)
{
    return x + y;
}

int main()
/*@ trusted; @*/
{
    y = 20;
    foo(30);
    return 0;
}
