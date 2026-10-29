/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Case 3: a specified caller (requires/ensures) calls an unspecified callee five times. Expect
 * _addr_cn values in f only, none in h, and a flat allocator. Before the fix h had them too (a, b,
 * c); the allocator stayed flat only because f's bump frame freed them at f's exit. Linked with
 * driver.c by ../run.sh. */
// expect addr_cn: f
// expect run: flat
static int h(int a, int b) { int c = a + b; return c; }
int f(int n)
/*@ requires 0 <= n; n < 100;
    ensures return >= 0; @*/
{
  int s = 0;
  s = h(s, n); s = h(s, n); s = h(s, n); s = h(s, n); s = h(s, n);
  return s;
}
void run(void) { f(3); }
