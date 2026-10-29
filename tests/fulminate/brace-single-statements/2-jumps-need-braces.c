/* #683: Fulminate braced every single-statement body of an if, else, loop or case, and refused one
 * that contains a compound literal, in case the braces shortened the literal's lifetime; now it
 * braces only the bodies that receive a statement-level injection, by default.
 *
 * Case 2: the single-statement bodies that DO receive a statement-level injection, a ghost-state
 * removal placed before the jump: `break`, `continue` and `goto` out of a block with locals, and
 * `return` (which rewrites itself into a braced block). Expect: these bodies are braced, the file
 * compiles, and it runs with ownership checking, exit 0. Run: cn instrument (the bracing is on by
 * default since #683). (No `for (int i = ...)`: cn re-prints that init-clause wrongly, see the
 * bump-alloc cases.) */
// expect braced: if (t > 6)
// expect braced: if (t == 2)
// expect braced: if (w == 3)
// expect braced: if (s < 0)
// expect braced: if (n == 100)
// expect braced: else
// expect run: 0
int f(int n) {
  int s = 0;
  int i = 0;
  while (i < n) {
    int t = i * 2;
    i++;
    if (t > 6) break;
    if (t == 2) continue;
    s += t;
  }
  int k = 0;
  while (k < n) {
    int w = k;
    if (w == 3) goto out;
    k++;
  }
out:
  if (s < 0) return -1;
  if (n == 100) return 1; else return s + k;
}
int main(void) { return f(5) == 13 ? 0 : 1; }   /* s = 0 + 4 + 6, k = 3 */
