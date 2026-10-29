/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 2: an uninterpreted resource predicate in requires and ensures, beside an ordinary RW
 * resource on another pointer, and a constraint relating the two predicate outputs. Expect with the
 * flag: the P resources, their outputs and the constraint on them are dropped; the RW resource and
 * its constraint are kept and checked; compiles, runs, exit 0. Without the flag: an error on the
 * uninterpreted predicate. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: x_cn = owned_signed_int(p_cn, PRE, 0)
// expect code-has: cn_assert(cn_integer_equality(x2_cn, cn_integer_add(x_cn, convert_to_cn_integer(1))), POST)
// expect code-lacks: * P(
// expect code-lacks: v_cn
// expect code-lacks: v2_cn
/*@ predicate (integer) P(pointer p, integer n) @*/

int incr(int *p, int *q, int n)
/*@ requires take x = RW<int>(p); x < 100; take v = P(q, n);
    ensures take x2 = RW<int>(p); x2 == x + 1; take v2 = P(q, n); v2 == v; return == x2; @*/
{ *p = *p + 1; return *p; }

int main(void) { int a = 1, b = 7; return incr(&a, &b, 3) == 2 && a == 2 ? 0 : 1; }
