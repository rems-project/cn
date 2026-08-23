int inc(int x)
/*@ requires x < 2147483647i32;
    ensures true; @*/
{
    return x + 1;
}
