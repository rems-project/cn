int f(int *p, int *q)
/*@
requires
    has_alloc_id(p);
    has_alloc_id(q);
    (integer) p != (integer) q;
ensures
    return == 0;
@*/
{
    return p == q;
}

int main()
{
    int x = 0;
    int y = 1;
    /*@ derive_constraints(RW<int>(&x), RW<int>(&y)); @*/
    f(&x, &y);
}
