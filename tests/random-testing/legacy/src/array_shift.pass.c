void f(int *p, int len)
/*@
    requires
        take x_1 = Owned<int>(p);
        len > 0i32;
        let offset = array_shift(array_shift(p, (u64)mod(x_1, len)), 5u32);
        take x_2 = Owned(offset);
    ensures
        take x_1_after = Owned<int>(p);
        take x_2_after = Owned(offset);
        x_1 == x_1_after;
        x_2 == x_2_after;
@*/
{}
