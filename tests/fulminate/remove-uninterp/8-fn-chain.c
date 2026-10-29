/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 8: a chain of defined functions ending in an uninterpreted one, k -> h -> g with only g
 * uninterpreted, and a specification that mentions only k. Expect with the flag: k and h are
 * treated as uninterpreted (the closure), so `k(x) > 0` is dropped and no C is generated for k, h
 * or g; the rest is kept; compiles, runs, exit 0. A check of only the direct callees would drop h
 * but still emit k, whose C calls the missing h. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: cn_assert(cn_integer_lt(convert_to_cn_integer(0), x_cn), PRE)
// expect code-has: cn_assert(cn_integer_equality(return_cn, cn_integer_add(x_cn, convert_to_cn_integer(1))), POST)
// expect code-lacks: * g(
// expect code-lacks: * h(
// expect code-lacks: * k(
/*@ function (integer) g(integer x) @*/
/*@ function (integer) h(integer x) { g(x) + 1 } @*/
/*@ function (integer) k(integer x) { h(x) * 2 } @*/

int f(int x)
/*@ requires x > 0; k(x) > 0;
    ensures return == x + 1; return < k(x); @*/
{ return x + 1; }

int main(void) { return f(3) == 4 ? 0 : 1; }
