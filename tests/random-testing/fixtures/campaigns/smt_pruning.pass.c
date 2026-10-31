// Test with `-d1` and see if there are two (pruned) or four branches
void f(int x)
/*@
    requires
        MINi32() <= x; x <= MAXi32();
        x < 10 || x > 10;
        x < 10 || x > 10;
@*/
{}
