/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 4: the kept part of a specification must still be checked: the body violates the ordinary
 * postcondition next to an uninterpreted one. Expect with the flag: compiles, and the run FAILS
 * with a CN postcondition violation (non-zero exit), on `return == x + 1`. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: fail
// expect code-has: cn_assert(cn_integer_equality(return_cn, cn_integer_add(x_cn, convert_to_cn_integer(1))), POST)
// expect code-lacks: * g(
/*@ function (integer) g(integer x) @*/

int f(int x)
/*@ requires x > 0; g(x) > 0;
    ensures return == x + 1; return == g(x); @*/
{ return x + 2; }

int main(void) { return f(3) == 5 ? 0 : 1; }
