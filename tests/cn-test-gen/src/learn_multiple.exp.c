void p(int x, int y)
/*@
    requires
        0i32 < x;
        x + 1000i32 < y;
        x < x + 1000i32;
        y < MAXi32() - 5i32;
@*/
{}
