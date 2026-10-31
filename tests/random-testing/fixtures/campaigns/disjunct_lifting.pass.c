// Regression test for conjunct handling in disjunction lifting.
// `v` is bound by a predicate call, so `v == 0` is a leftover disjunct that
// must become its own pick branch. The buggy lifting instead conjoined it into
// the `x == 1` branch, which contradicts `x != 1`, making input
// generation always fail.

/*@
predicate (integer) AnyInt(pointer p) {
    take v = RW<int>(p);
    return v;
}
@*/

void f(int x, int *p)
/*@ requires take v = AnyInt(p);
             MINi32() <= x; x <= MAXi32();
             MINi32() <= v; v <= MAXi32();
             v == 0 || x == 1;
             x != 1;
    ensures  take w = AnyInt(p);
@*/
{
}
