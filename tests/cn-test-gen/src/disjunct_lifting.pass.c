// Regression test for conjunct handling in disjunction lifting.
// `v` is bound by a predicate call, so `v == 0i32` is a leftover disjunct that
// must become its own pick branch. The buggy lifting instead conjoined it into
// the `x == 1i32` branch, which contradicts `x != 1i32`, making input
// generation always fail.

/*@
predicate (i32) AnyInt(pointer p) {
    take v = RW<int>(p);
    return v;
}
@*/

void f(int x, int *p)
/*@ requires take v = AnyInt(p);
             v == 0i32 || x == 1i32;
             x != 1i32;
    ensures  take w = AnyInt(p);
@*/
{
}
