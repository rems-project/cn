// This tests surfaces the implementation choice for shifting NULL
// pointers as an unspecified pointer value, rather than NULL or
// converting to the offset with an empty provenance or the previous
// default value. Users should not be shifting NULL in their specs
void f(int *p)
/*@
requires
    is_null(p);
ensures
    let x = array_shift<char>(p,1);
    let y = array_shift<char>(p,2);
    ptr_eq(x, y);
@*/
{
}

int main()
{
    f(0);
}
