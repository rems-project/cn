/* cn instrument --remove-uninterp: CN functions and predicates declared without a body, which cn
 * verify treats as uninterpreted, used to be refused by Fulminate ("Uninterpreted CN functions not
 * supported at runtime"). With the flag, every let, resource, constraint, loop-invariant conjunct,
 * CN statement and lemma application whose term mentions an uninterpreted name, or a definition
 * that uses one, or a variable defined from one, is dropped, and no C is generated for the
 * uninterpreted definitions or those using them; the rest is checked as before.
 *
 * Case 7: a typical shape: an uninterpreted predicate over a struct, an uninterpreted function of
 * the struct's value, a specified function calling another specified one, and an unspecified
 * caller. Expect with the flag: compiles, runs, exit 0, and the kept resources (RW<struct s>) still
 * checked. */
// expect flags: --remove-uninterp
// expect without-flags: refused
// expect run: 0
// expect code-has: v_cn = owned_struct_s(p_cn, PRE, 0)
// expect code-has: cn_assert(cn_integer_equality(v2_cn->b, v_cn->b), POST)
// expect code-lacks: * Inv(
// expect code-lacks: * wf(
// expect code-lacks: i_cn
// expect code-lacks: i2_cn
struct s { int a; int b; };
/*@ predicate (struct s) Inv(pointer p) @*/
/*@ function (boolean) wf(struct s v) @*/

int set_a(struct s *p, int a)
/*@ requires take v = RW<struct s>(p); take i = Inv(p); wf(v);
    ensures take v2 = RW<struct s>(p); v2.a == a; v2.b == v.b; take i2 = Inv(p); return == a; @*/
{ p->a = a; return a; }

int set_both(struct s *p, int a, int b)
/*@ requires take v = RW<struct s>(p); take i = Inv(p);
    ensures take v2 = RW<struct s>(p); v2.a == a; v2.b == b; take i2 = Inv(p); @*/
{ p->b = b; return set_a(p, a); }

static int unspecified_caller(struct s *p) { return set_both(p, 1, 2) + p->b; }

int main(void) { struct s x = { 0, 0 }; return unspecified_caller(&x) == 3 && x.a == 1 && x.b == 2 ? 0 : 1; }
