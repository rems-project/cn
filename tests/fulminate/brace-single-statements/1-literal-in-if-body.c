/* #683: Fulminate braced every single-statement body of an if, else, loop or case, and refused one
 * that contains a compound literal, in case the braces shortened the literal's lifetime; now it
 * braces only the bodies that receive a statement-level injection, by default.
 *
 * Case 1: a compound literal in an expression statement that is the body of an if, including the
 * shape `if (res) *res = (typeof(*res)){r0, r1, 0, 0};` of a result-filling macro. Expect:
 * instruments without the error, the body is not braced (an expression statement receives no
 * statement-level injection), compiles, runs, exit 0. Run: cn instrument (the bracing is on by
 * default since #683). (No literal inside a loop body: CN's typing crashes on that, unrelated to
 * the bracing.) */
// expect unbraced: if (res)
// expect unbraced: if (c) CN_STORE(r,
// expect unbraced: if (c) CN_STORE(q,
// expect unbraced: else CN_STORE(q,
// expect compile: no (CN_LOAD round a compound literal; its commas split the macro's arguments)
struct res { unsigned long a0, a1, a2, a3; };
struct pt { int x, y; };
int sum(struct pt p) { return p.x + p.y; }
static void fill(struct res *res, unsigned long r0, unsigned long r1) {
  if (res)
    *res = (typeof(*res)){r0, r1, 0, 0};
}
int main(void) {
  int c = 1, r = 0;
  struct res res;
  struct pt q;
  if (c) r = sum((struct pt){ 1, 2 });
  if (c) q = (struct pt){ 3, 4 }; else q = (struct pt){ 0, 0 };
  fill(&res, 7, 8);
  return r == 3 && q.y == 4 && res.a0 == 7 && res.a1 == 8 ? 0 : 1;
}
