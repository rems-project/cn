/* #683: Fulminate braced every single-statement body of an if, else, loop or case, and refused one
 * that contains a compound literal, in case the braces shortened the literal's lifetime; now it
 * braces only the bodies that receive a statement-level injection, by default.
 *
 * Case 6: a labeled statement as the single-statement body of an if, reached by a goto. Fulminate
 * re-maps every in-scope variable right after a label a goto can reach, so the body must be braced:
 * unbraced, the label binds only the first re-mapping and the rest of them and the statement run
 * outside the if (f(0) returned 6 instead of 0 before the proper fix). Expect: braced, compiles,
 * runs, exit 0. Run: cn instrument (the bracing is on by default since #683). */
// expect braced: if (c)
// expect line: if (c) {L:
// expect run: 0
int f(int c) {
  int x = 0;
  int y = 5;
  if (c) goto L;
  y = 6;
  if (c) L: x = y;
  return x;
}
int main(void) { return f(1) == 5 && f(0) == 0 ? 0 : 1; }
