int f(int *p, int *q)
/*@
ensures
    return == 1;
@*/
{
    return p == q;
}

