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
    return p != q;
}

