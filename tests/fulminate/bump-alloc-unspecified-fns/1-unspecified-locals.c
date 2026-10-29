/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Case 1: unspecified functions only: parameters, nested-block locals, if/while locals, a struct
 * local, a pointer parameter, an early return, static and void functions. Expect no _addr_cn value
 * anywhere and a flat allocator. Before the fix: _addr_cn values in all four functions, +336 bytes
 * per call, out of memory at call 5. Linked with driver.c by ../run.sh. */
// expect addr_cn: none
// expect run: flat
struct pt { int x; int y; };
static int sq(int v) { return v * v; }
static void bump(int *p) { int t = *p; t++; *p = t; }
static int body(int n, int *acc) {
  int s = 0;
  if (n > 2) { int k = n - 1; s += sq(k); }
  while (n > 0) { int w = n; s += w; n--; }
  { struct pt p = { s, n }; bump(&p.x); s = p.x + p.y; }
  if (s < 0) return -1;
  *acc += s;
  return s;
}
void run(void) { int acc = 0; body(5, &acc); body(1, &acc); }
