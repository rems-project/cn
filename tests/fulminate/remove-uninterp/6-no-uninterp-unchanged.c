/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 6: no uninterpreted names at all: the flag must change nothing. The output with and without
 * the flag must be identical (`expect without-flags: same`), compile, run, exit 0. */
// expect flags: --remove-uninterp
// expect without-flags: same
// expect run: 0
// expect code-has: static cn_integer* twice(cn_integer*);
// expect code-has: c_cn = Cell(q_cn, PRE, 0)
// expect code-has: cn_assert(cn_integer_equality(return_cn, twice(x_cn)), POST)
/*@ function (integer) twice(integer x) { x * 2 }
    predicate (integer) Cell(pointer p) { take v = RW<int>(p); return v; } @*/

int f(int x, int *q)
/*@ requires x > 0; x < 100; take c = Cell(q);
    ensures return == twice(x); take c2 = Cell(q); c2 == c; @*/
{ return x * 2; }

int main(void) { int b = 0; return f(3, &b) == 6 ? 0 : 1; }
