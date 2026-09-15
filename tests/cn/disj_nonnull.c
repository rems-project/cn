int x = 1;
int y = 2;

void globals()
/*@ accesses x; accesses y; @*/
{
  /*@ derive_constraints(RW<int>(&x), RW<int>(&y)); @*/

  /*@ assert((integer) &x != (integer) &y); @*/

  /*@ assert((integer)&x < (integer)&x + 4); @*/
  /*@ assert((integer)&x < MAXu64() - 4); @*/

  /*@ assert((integer)&y < MAXu64() - 4); @*/
  /*@ assert((integer)&y < (integer)&y + 4); @*/

  /*@ assert((integer)&x < (integer)&y || (integer)&x > (integer)&y); @*/
  /*@ assert((integer)&x + 4 <= (integer)&y || (integer)&y + 4 <= (integer)&x); @*/

}

int main()
{
    int p = 1;
    int q = 2;

  /*@ derive_constraints(RW<int>(&p), RW<int>(&q)); @*/

  /*@ assert((integer) &p != (integer) &q); @*/

  /*@ assert((integer)&p < (integer)&p + 4); @*/
  /*@ assert((integer)&p < MAXu64() - 4); @*/

  /*@ assert((integer)&q < MAXu64() - 4); @*/
  /*@ assert((integer)&q < (integer)&q + 4); @*/

  /*@ assert((integer)&p < (integer)&q || (integer)&p > (integer)&q); @*/
  /*@ assert((integer)&p + 4 <= (integer)&q || (integer)&q + 4 <= (integer)&p); @*/

}
