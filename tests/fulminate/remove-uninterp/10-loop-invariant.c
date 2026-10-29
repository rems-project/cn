/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 10: loop invariants that mention an uninterpreted function (bound(n)) and take an
 * uninterpreted predicate (Inv(q)), beside ordinary conjuncts and an ordinary RW resource. Expect
 * with the flag: the uninterpreted conjuncts and the Inv resource are dropped from the invariant,
 * the ordinary ones (0 <= i, i <= n, the RW on q) are kept and checked at every iteration;
 * compiles, runs, exit 0. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: cn_assert(cn_integer_le(O_i0, O_n0), LOOP)
// expect code-has: cn_assert(cn_integer_equality(w, v_cn), LOOP)
// expect code-has: cn_assert(cn_pointer_equality(O_q0, q_cn), LOOP)
// expect code-lacks: * bound(
// expect code-lacks: * Inv(
// expect code-lacks: iw
// expect code-lacks: iv_cn
/*@ function (integer) bound(integer n) @*/
/*@ predicate (integer) Inv(pointer q) @*/

int sum(int n, int *q)
/*@ requires 0 <= n; n < 100; take v = RW<int>(q); take iv = Inv(q);
    ensures take v2 = RW<int>(q); take iv2 = Inv(q); return == n; @*/
{
  int i = 0;
  int s = 0;
  while (i < n)
  /*@ inv 0 <= i; i <= n; s <= bound(n); take w = RW<int>(q); take iw = Inv(q); w == v; {n} unchanged; {q} unchanged; @*/
  { s = s + 1; i = i + 1; }
  return s;
}

int main(void) { int c = 4; return sum(5, &c) == 5 ? 0 : 1; }
