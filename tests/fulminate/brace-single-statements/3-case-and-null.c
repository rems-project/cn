/* #683: Fulminate braced every single-statement body of an if, else, loop or case, and refused one
 * that contains a compound literal, in case the braces shortened the literal's lifetime; now it
 * braces only the bodies that receive a statement-level injection, by default.
 *
 * Case 3: the other bodies the inserter looks at: the statement after a case label (`case 2:
 * break;`, a jump, braced; `case 1: r = 4;`, an expression, not), null statements as bodies, a
 * nested `if (a) if (b == 0) break;` in a loop (the inner body braced). Expect: compiles, runs,
 * exit 0. Run: cn instrument (the bracing is on by default since #683). (The case-label-with-
 * literal shape is case 5.) */
// expect braced: case 2:
// expect braced: if (z == 1)
// expect unbraced: case 1: CN_STORE(r, 4)
// expect unbraced: default: CN_STORE(r, -1)
// expect unbraced: if (a) ;
// expect unbraced: while (b) CN_STORE(b, 0)
// expect run: 0
int g(int sel, int a, int b) {
  int r = 0;
  switch (sel) {
  case 1: r = 4; break;
  case 2: break;
  default: r = -1;
  }
  if (a) ;
  while (b) b = 0;
  int i = 0;
  while (i < 2) { int z = i; i++; if (a) if (z == 1) break; }
  return r + i;
}
int main(void) { return g(1, 1, 1) == 6 && g(2, 0, 0) == 2 && g(3, 1, 0) == 1 ? 0 : 1; }
