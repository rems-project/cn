void hard_to_generate(int *x)
/*@
    requires
        take v = RW(x);
        (u64)v == 123456u64;
    ensures
        take v_ = RW(x);
        v == v_;
  @*/
{}
