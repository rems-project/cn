
int global_x;
void *global_void_ptr;

extern int extern_f (void);

int
g (int x)
{
  return 0;
}

/*@
function ({integer x1, integer x2}) get_globals ()
  { {x1: (integer) (&g), x2: (integer) (&extern_f)} }
@*/

int
f (int x)
/*@ accesses global_x; @*/
{
  /* resolution of the 'g' & 'extern_f' addrs triggered a bug at one point */
  /*@ assert (((integer) (&x)) == ((integer) (&x))); @*/;
  /*@ assert (((integer) (&global_x)) == ((integer) (&global_x))); @*/;
  /*@ assert (get_globals () == get_globals ()); @*/;
  /*@ assert (((integer) (&g)) == ((integer) (&g))); @*/;

  return x == global_x;
}

int main(void)
/*@ trusted; @*/
{
  int r = f(42);
}
