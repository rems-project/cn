/* An out-of-range conversion to a signed type reduces modulo 2^N under
   --impl=x86_64 (as in tests/cn/division_return_size.error.c, which the
   default profile rejects). */
int narrow(long x)
/*@ requires x == 4294967297; ensures return == 1; @*/
{
    return x;
}

int main(void) {
    narrow(4294967297L);
    return 0;
}
