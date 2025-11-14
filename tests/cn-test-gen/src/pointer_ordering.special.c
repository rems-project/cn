void need_ordered_pointers(char *p, char *q, char *r)
/*@
    requires
        take P = RW(p);
        take Q = RW(q);
        take R = RW(r);
        (p < q && q < r);

    ensures
        take P_ = RW(p);
        take Q_ = RW(q);
        take R_ = RW(r);
@*/
{}
