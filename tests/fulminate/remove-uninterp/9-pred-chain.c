/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 9: chains through predicates: R takes Q takes uninterpreted P; and S, whose clause is
 * ordinary resource-wise but whose constraint applies the uninterpreted function g. Both used in a
 * specification beside a plain RW. Expect with the flag: R, Q, P, S and g get no C, the resources
 * on them and their outputs' constraints are dropped, the RW<int>(p) resource and `x2 == x + 1` are
 * kept; compiles, runs, exit 0. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: x_cn = owned_signed_int(p_cn, PRE, 0)
// expect code-has: cn_assert(cn_integer_equality(x2_cn, cn_integer_add(x_cn, convert_to_cn_integer(1))), POST)
// expect code-lacks: * P(
// expect code-lacks: * Q(
// expect code-lacks: * R(
// expect code-lacks: * S(
// expect code-lacks: * g(
// expect code-lacks: a_cn
// expect code-lacks: b_cn
/*@ function (integer) g(integer x) @*/
/*@ predicate (integer) P(pointer p, integer n) @*/
/*@ predicate (integer) Q(pointer p) { take v = P(p, 0); return v; } @*/
/*@ predicate (integer) R(pointer p) { take v = Q(p); return v + 1; } @*/
/*@ predicate (integer) S(pointer p) { take v = RW<int>(p); assert (g(v) > 0); return v; } @*/

int f(int *p, int *q, int *r)
/*@ requires take x = RW<int>(p); take a = R(q); take b = S(r);
    ensures take x2 = RW<int>(p); x2 == x + 1; take a2 = R(q); a2 == a; take b2 = S(r); b2 == b; @*/
{ *p = *p + 1; return *p; }

int main(void) { int a = 1, b = 7, c = 9; return f(&a, &b, &c) == 2 && a == 2 ? 0 : 1; }
