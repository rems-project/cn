/* With len > 0, mod(x_1, len) lies in [0, len), so the old u64 cast
   preserves its value. Both pointer shifts remain part of this regression. */
void f(int *p, int len)
/*@
    requires
        take x_1 = Owned<int>(p);
        len > 0;
        let offset = array_shift(array_shift(p, mod(x_1, len)), 5);
        take x_2 = Owned(offset);
    ensures
        take x_1_after = Owned<int>(p);
        take x_2_after = Owned(offset);
        x_1 == x_1_after;
        x_2 == x_2_after;
@*/
{}
