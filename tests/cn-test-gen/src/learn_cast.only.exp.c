void p_cast(int x, int y)
/*@
    requires
        0i32 < x;
        (i64)x + 1000i64 < (i64)y;
        x < x + 1000i32;
        y < MAXi32() - 5i32;
@*/
{}
