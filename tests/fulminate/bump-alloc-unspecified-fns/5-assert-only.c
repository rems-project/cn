/* #681: Fulminate allocated a CN value for every parameter and local at entry, in a bump allocator
 * reset only at the exit of a function with a specification, so a function without one grew it with
 * every call; now only the functions that need them (a requires/ensures, a loop invariant or a CN
 * statement) get them.
 *
 * Case 5: no requires/ensures, only a CN assert statement in the body. The function counts as
 * specified (has_cn_spec), so _addr_cn values are generated for n and r, and the allocator must
 * stay flat (it gets a bump frame). Unchanged by the fix. Linked with driver.c by ../run.sh. */
// expect addr_cn: twice
// expect run: flat
int twice(int n) {
  int r = n + n;
  /*@ assert (r == n + n); @*/
  return r;
}
void run(void) { twice(2); twice(3); }
