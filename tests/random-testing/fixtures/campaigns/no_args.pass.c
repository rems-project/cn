int no_args(int unused)
/*@ requires MINi32() <= unused; unused <= MAXi32();
    ensures return == 42;
@*/
{
  return 42;
}

// Predicate whose only argument gets pruned away, leaving a
// zero-argument predicate call in the generated code
/*@
predicate (boolean) UnusedArg (pointer p)
{
    return true;
}
@*/

int pred_no_args(int *p)
/*@ requires take P = UnusedArg(p);
    ensures take Q = UnusedArg(p);
            return == 7;
@*/
{
  return 7;
}
