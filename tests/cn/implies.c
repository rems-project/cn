int identity(int x)
{
    int y = x;
    /*@ assert((x == 0) implies (y == 0));@*/
    return y;
}
