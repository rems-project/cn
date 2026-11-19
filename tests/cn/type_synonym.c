

/*@
type_synonym xy_tup = ({integer x, integer y})

function (xy_tup) mk_tup (integer x, integer y)
  { {x : x, y : y} }
@*/

void
f (unsigned int x, unsigned int y)
/*@ requires let tup = mk_tup(x, y);
    ensures tup == tup; @*/
{
  return;
}

int main(void)
/*@ trusted; @*/
{
  f(4, 5);
}
