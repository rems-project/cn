// This tests surfaces the implementation choice for shifting NULL
// pointers as an unspecified pointer value, rather than NULL or
// converting to the offset with an empty provenance or the previous
// default value. Users should not be shifting NULL in their specs
void f(int *p, int *q)
/*@
requires
    is_null(p);
ensures
    let x = array_shift<char>(p,1u64);
    ptr_eq(x, NULL) || (u64) x == 1u64;
@*/
{
}
