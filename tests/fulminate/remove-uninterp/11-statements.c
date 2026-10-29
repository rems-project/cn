/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 11: CN statements in a body: an assert that mentions an uninterpreted function, an assert
 * that does not (kept and checked), and a second function whose kept assert is false, which must
 * still fail at run time. Expect with the flag: f runs clean; g's run fails, exit non-zero; main
 * calls only f. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: cn_assert(cn_integer_equality(read_y1, cn_integer_add(read_x1, convert_to_cn_integer(1))), STATEMENT)
// expect code-has: cn_assert(cn_integer_equality(read_y3, cn_integer_add(read_x2, convert_to_cn_integer(1))), STATEMENT)
// expect code-lacks: * magic(
/*@ function (integer) magic(integer x) @*/

int f(int x)
/*@ requires 0 <= x; x < 100;
    ensures return == x + 1; @*/
{
  int y = x + 1;
  /*@ assert (y == magic(x)); @*/
  /*@ assert (y == x + 1); @*/
  return y;
}

int g(int x)
/*@ requires 0 <= x; x < 100;
    ensures return == x + 2; @*/
{
  int y = x + 2;
  /*@ assert (magic(y) > 0); @*/
  /*@ assert (y == x + 1); @*/
  return y;
}

int main(void) { return f(3) == 4 ? 0 : 1; }
