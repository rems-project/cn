/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 12: lemmas. `step` has a requires/ensures mixing an uninterpreted function (magic) and an
 * uninterpreted predicate (Inv) with ordinary conjuncts and an RW resource; it is applied in f with
 * plain arguments. `other` is applied with an argument that applies magic. Expect with the flag:
 * step's C keeps only its ordinary parts (checked when applied), the `apply other(magic(x))`
 * statement is dropped; compiles, runs, exit 0. Without the flag: refused. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: static void step(cn_integer*, cn_pointer*);
// expect code-has: step(read_x0, read_p0);
// expect code-has: cn_assert(cn_integer_lt(x, cn_integer_add(x, convert_to_cn_integer(1))), POST)
// expect code-lacks: * magic(
// expect code-lacks: * Inv(
// expect code-lacks: other(read
// expect code-lacks: other(magic
/*@ function (integer) magic(integer x) @*/
/*@ predicate (integer) Inv(pointer p) @*/

/*@ lemma step(integer x, pointer p)
    requires 0 <= x; x < 100; magic(x) > 0; take v = RW<int>(p); take iv = Inv(p); v == x;
    ensures take v2 = RW<int>(p); v2 == v; x + 1 > x; magic(x) == magic(x + 1); take iv2 = Inv(p); @*/

/*@ lemma other(integer y)
    requires y > 0;
    ensures y + 1 > 0; @*/

int f(int x, int *p)
/*@ requires 0 <= x; x < 50; take v = RW<int>(p); v == x;
    ensures take v2 = RW<int>(p); v2 == v; return == x + 1; @*/
{
  /*@ apply step(x, p); @*/
  /*@ apply other(magic(x)); @*/
  return x + 1;
}

int main(void) { int c = 3; return f(3, &c) == 4 ? 0 : 1; }
