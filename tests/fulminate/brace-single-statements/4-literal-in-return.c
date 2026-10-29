/* #683: Fulminate braced every single-statement body of an if, else, loop or case, and refused one
 * that contains a compound literal, in case the braces shortened the literal's lifetime; now it
 * braces only the bodies that receive a statement-level injection, by default.
 *
 * Case 4: a compound literal inside a jump that is a single-statement body: `if (c) return (struct
 * pt){1, 2};`. The return is a statement with an injection, so it is braced; the literal's value is
 * copied out by the return, so the braces are harmless. Records whether the compound-literal
 * refusal still fires on this shape. Run: cn instrument (the bracing is on by default since #683). */
// expect instrument: no (the compound-literal refusal is kept for a braced return)
struct pt { int x, y; };
struct pt mk(int c) {
  if (c) return (struct pt){ 1, 2 };
  return (struct pt){ 0, 0 };
}
int main(void) { struct pt p = mk(1); return p.x + p.y == 3 ? 0 : 1; }
