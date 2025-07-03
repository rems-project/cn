// Test with `-d1` and see if there are two (pruned) or four branches
void f(int x)
/*@
    requires
        x < 10i32 || x > 10i32;
        x < 10i32 || x > 10i32;
@*/
{}
