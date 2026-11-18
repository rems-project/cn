/* A nested pattern followed by a default case: when specialising the
   default row to a constructor, Fulminate must expand its wildcard to one
   wildcard per member (none for A, two for P). */
/*@
datatype D { A {}, P { integer x, integer y }, C { datatype D d } }

function (integer) g (datatype D d) {
  match d {
    C { d : A {} } => { 1 }
    C { d : P { x : x, y : y } } => { x - y }
    _ => { 0 }
  }
}
@*/

void check_g (int x)
/*@ requires 0 <= x; x < 100;
    ensures g (C { d : A {} }) == 1;
            g (C { d : P { x : x, y : 2 } }) == x - 2;
            g (C { d : C { d : A {} } }) == 0;
            g (A {}) == 0;
            g (P { x : x, y : 2 }) == 0;
@*/
{
  ;
}

int main (void)
{
  check_g (5);
  return 0;
}
