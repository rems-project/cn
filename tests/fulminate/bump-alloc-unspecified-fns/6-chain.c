/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Case 6: a chain: a specified entry calls an unspecified function that calls another unspecified
 * one in a loop with block locals. Expect _addr_cn values in top only. NB: on cn (2026-10-22) this
 * case does not compile for an unrelated reason, the re-printed for init-clause (as case 2). Linked
 * with driver.c by ../run.sh. */
// expect addr_cn: top
// expect compile: no (the for init-clause is re-printed wrongly, as case 2)
static int leaf(int a) { int b = a + 1; return b; }
static int mid(int n) { int s = 0; for (int i = 0; i < n; i++) { int t = leaf(i); s += t; } return s; }
int top(int n)
/*@ requires 0 <= n; n < 50; ensures return >= 0; @*/
{ int r = mid(n); return r; }
void run(void) { top(5); }
