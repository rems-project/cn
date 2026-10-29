/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 3: an uninterpreted function inside a resource's pointer argument, and a boolean one as a
 * bare precondition. Expect with the flag: the RW whose pointer mentions idx is dropped (its output
 * becomes uninterpreted, so the constraint on it goes too); the plain RW<int>(p) stays; compiles,
 * runs, exit 0. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: x_cn = owned_signed_int(p_cn, PRE, 0)
// expect code-has: cn_assert(cn_integer_equality(x2_cn, x_cn), POST)
// expect code-lacks: * idx(
// expect code-lacks: * ok(
// expect code-lacks: e_cn = 
// expect code-lacks: e2_cn = 
/*@ function (integer) idx(integer n) @*/
/*@ function (boolean) ok(pointer p) @*/

int get(int *p, int *arr, int n)
/*@ requires take x = RW<int>(p); ok(p); take e = RW<int>(array_shift(arr, idx(n)));
    ensures take x2 = RW<int>(p); x2 == x; take e2 = RW<int>(array_shift(arr, idx(n))); e2 == e; return == x; @*/
{ return *p; }

int main(void) { int a = 5; int arr[4] = { 0, 1, 2, 3 }; return get(&a, arr, 2) == 5 ? 0 : 1; }
