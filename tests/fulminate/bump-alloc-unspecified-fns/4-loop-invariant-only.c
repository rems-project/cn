/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Case 4 (the consumer side): no requires/ensures, only a loop invariant naming a local and a
 * parameter. The function counts as specified (has_cn_spec), so the _addr_cn values for i, s and n
 * must still be generated, since the invariant is compiled against them, and the allocator must
 * stay flat. Unchanged by the fix. Linked with driver.c by ../run.sh. */
// expect addr_cn: sum
// expect run: flat
int sum(int n) {
  int s = 0;
  int i = 0;
  while (i < n)
  /*@ inv 0 <= i; i <= n; s >= 0; {n} unchanged; @*/
  { s = s + 1; i = i + 1; }
  return s;
}
void run(void) { sum(4); }
