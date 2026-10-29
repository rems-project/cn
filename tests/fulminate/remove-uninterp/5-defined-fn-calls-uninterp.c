/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 5: a DEFINED function whose body calls an uninterpreted one, and a DEFINED predicate whose
 * clause takes an uninterpreted one, used in specifications. The specification mentions only the
 * defined names, so the analysis keeps the constraints, but the generated C for h and Q must call
 * the C for g and P, which are not generated. Records what happens (expected: a compile error, the
 * transitive case not being handled yet). */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: cn_assert(cn_integer_lt(convert_to_cn_integer(0), x_cn), PRE)
// expect code-has: cn_assert(cn_integer_equality(return_cn, x_cn), POST)
// expect code-lacks: * g(
// expect code-lacks: * h(
// expect code-lacks: * P(
// expect code-lacks: * Q(
// expect code-lacks: v_cn
/*@ function (integer) g(integer x) @*/
/*@ function (integer) h(integer x) { g(x) + 1 } @*/
/*@ predicate (integer) P(pointer p, integer n) @*/
/*@ predicate (integer) Q(pointer p) { take v = P(p, 0); return v; } @*/

int f(int x, int *q)
/*@ requires x > 0; h(x) > 0; take v = Q(q);
    ensures return == x; take v2 = Q(q); @*/
{ return x; }

int main(void) { int b = 0; return f(3, &b) == 3 ? 0 : 1; }
