/* #683: Fulminate braced every single-statement body of an if, else, loop or case, and refused one
 * that contains a compound literal, in case the braces shortened the literal's lifetime; now it
 * braces only the bodies that receive a statement-level injection, by default.
 *
 * Case 5: a compound literal in the statement after a case label, `case X: u = ((uuid_t){...});`.
 * This is the one place where bracing really would end the literal's lifetime early (a labeled
 * statement is not a block of its own, unlike an if or loop body, C11 6.8.4p3 and 6.8.5p5). Expect:
 * not braced (an expression statement), compiles, runs, exit 0. NB: cn (2026-10-26) crashes in CN's
 * typing on a compound literal in an unbraced loop body or directly under a case ("Context.get_a:
 * not found"), before any bracing, unrelated to the bracing. */
// expect instrument: no (CN typing: Context.get_a on a compound literal after a case label)
typedef struct { unsigned char b[4]; } uuid_t;
int main(void) {
  int sel = 1;
  uuid_t uuid = { { 0, 0, 0, 0 } };
  switch (sel) {
  case 1: uuid = ((uuid_t){ { 1, 2, 3, 4 } }); break;
  default: break;
  }
  return uuid.b[3] == 4 ? 0 : 1;
}
