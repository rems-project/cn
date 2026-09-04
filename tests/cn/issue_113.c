void f(char *p, unsigned x)
/*@
    requires is_null(array_shift<unsigned char>(array_shift<unsigned char>(p, x), x));
@*/
{
}
