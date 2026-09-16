void inc(int* p)
/*@ requires take X = RW(p);
             X < 2147483647;
    ensures take X2 = RW(p);
            X2 < 2147483647; @*/
{
    *p += 1;
}
