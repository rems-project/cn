/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Case 2: unspecified functions with for (int i = ...) and do/while loops, a switch and a goto,
 * block locals in loop bodies. Expect no _addr_cn value and a flat allocator. NB: on cn
 * (2026-10-22) this case does not compile for an unrelated reason: the for init-clause is re-
 * printed as `int i = c_add_to_ghost_state(...), 0;`. Linked with driver.c by ../run.sh. */
// expect addr_cn: none
// expect compile: no (the for init-clause is re-printed as `int i = c_add_to_ghost_state(...), 0;`)
static int f(int n) {
  int s = 0;
  for (int i = 0; i < n; i++) { int d = i * 2; s += d; }
  do { int e = s & 1; s += e; n--; } while (n > 0);
  switch (s & 3) { case 0: { int z = 1; s += z; break; } default: { int y = 2; s += y; } }
  if (s > 100) goto out;
  s += 1;
out:
  return s;
}
void run(void) { f(6); f(0); }
