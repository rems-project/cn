int inc(int x)
/*@ requires x < 2147483647;
    ensures return < 2147483647; @*/
{
    return x + 1;
}
