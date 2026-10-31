// Regression test for the Darcy struct/product-arg-destruction fix
// (commit "[Darcy] Fix struct arg destruction"). Under the darcy engine with
// --experimental-product-arg-destruction, the struct-by-value argument `s` is
// split into leaf record fields (s_ModeA, s_W_A, s_Plane_Counter);
// SpecTests.convert_from must rebuild the whole struct as a compound literal
// when calling the function-under-test. Before the fix it emitted `res->s`
// (a non-existent record member) and the generated harness failed.

struct State {
  int ModeA;
  int W_A;
  int Plane_Counter;
};

/*@
function (boolean) valid_state (struct State s) {
  (s.ModeA == 0i32 || s.ModeA == 1i32) &&
  s.W_A >= 0i32 &&
  (0i32 <= s.Plane_Counter && s.Plane_Counter <= 3i32)
}
@*/

struct State reset_Plane_Counter(struct State s)
/*@ requires valid_state(s);
    ensures  valid_state(return);
             return.Plane_Counter == 0i32;
             s.ModeA == return.ModeA;
             s.W_A == return.W_A;
@*/
{
  struct State temp = s;
  temp.Plane_Counter = 0;
  return temp;
}
