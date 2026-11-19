int f(int *p, int *q)
/*@
ensures
    return == 0;
@*/
{
    return p == q;
}

