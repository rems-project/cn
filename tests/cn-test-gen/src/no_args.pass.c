int no_args(int unused)
/*@ ensures return == 42i32;
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
            return == 7i32;
@*/
{
  return 7;
}
