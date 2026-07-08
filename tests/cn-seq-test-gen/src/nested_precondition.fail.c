/* Regression test: a function under test that violates its callee's
   precondition is a bug and must be reported and not discarded.
*/

void callee(int x)
/*@
requires
  x >= 0i32;
ensures
  true;
@*/
{
}

void buggy()
/*@
requires
  true;
ensures
  true;
@*/
{
  callee(-1);
}
