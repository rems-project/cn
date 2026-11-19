

/* Simple demo of a kind of situation where ownership is obtained via an array
   that encloses an object passed by pointer. This kind of step is required by
   the buddy-allocator proof. */

int *global_array;

/*@ 
function (integer) global_array_width() {42}

predicate (map<integer, integer>) Global_Array (pointer p)
{
  take Arr = each (integer i; 0 <= i && i < global_array_width ())
    { RW(array_shift<int>(p, i)) };
  return Arr;
}
@*/

void set_a_pointer(int *p, int x)
/*@ accesses global_array;
    requires (alloc_id) global_array == (alloc_id) p;
             take Arr = Global_Array(global_array);
             let offs = ((integer)p - (integer)global_array);
             mod(offs, (sizeof<int>)) == 0;
             let idx = (offs / (sizeof<int>));
             0 <= idx && idx < ( (global_array_width ()));
    ensures take Arr2 = Global_Array(global_array); @*/
{
  /*@ focus RW<int>, (idx); @*/
  *p = x;
}

int main(void)
/*@ trusted; @*/
{
  int p[1];
  set_a_pointer(p, 5);
}
