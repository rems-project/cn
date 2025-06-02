int foo(int x)
/*@ requires x + n - n < MAXi32();
    requires ghost i32 n; true;
    ensures return == x + 1i32; @*/
{
    return x + 1;
}
