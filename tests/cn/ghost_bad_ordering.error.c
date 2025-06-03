int foo(int x)
/*@
  requires
    x + n - n < MAXi32();
    cn_ghost i32 n; true;
  ensures
    return == x + 1i32; 
@*/
{
    return x + 1;
}
