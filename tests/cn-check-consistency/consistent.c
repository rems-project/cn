// The built-in unreachable_proxy is specified as `requires false` by design;
// --check-consistency must not report it.
int f(int x)
/*@ requires 0 <= x; x < 100;
    ensures return == x + 1; @*/
{
  return x + 1;
}
