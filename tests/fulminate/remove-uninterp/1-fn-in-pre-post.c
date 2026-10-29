/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 1: an uninterpreted function in requires and ensures, directly and through a `let`. Expect
 * with the flag: the constraints that mention g, and the `let` defined from it, are dropped, the
 * rest of the specification is kept and checked, the file compiles and runs, exit 0. Without the
 * flag: "Uninterpreted CN functions not supported at runtime". */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: cn_assert(cn_integer_lt(convert_to_cn_integer(0), x_cn), PRE)
// expect code-has: cn_assert(cn_integer_equality(return_cn, cn_integer_add(x_cn, convert_to_cn_integer(1))), POST)
// expect code-has: cn_assert(cn_integer_lt(x_cn, return_cn), POST)
// expect code-lacks: * g(
// expect code-lacks: y_cn
/*@ function (integer) g(integer x) @*/

int f(int x)
/*@ requires x > 0; g(x) > 0;
    ensures return == x + 1; return == g(x) + x; @*/
{ return x + 1; }

int h(int x)
/*@ requires let y = g(x); y > 0; x < 100;
    ensures return >= y; return > x; @*/
{ return x + 2; }

int main(void) { return f(3) == 4 && h(3) == 5 ? 0 : 1; }
