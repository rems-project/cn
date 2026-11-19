struct x {
  int z;
  int w;
};

void f(struct x *p)
/*@
requires
    take l = Owned(member_shift<struct x>(p, w));
ensures
    take l_ = Owned(member_shift<struct x>(p, w));
@*/
{}
