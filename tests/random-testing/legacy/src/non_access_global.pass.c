static int im_a_global;

/*@
  predicate (void) MaybeGlobal(pointer p, boolean cond)
  {
      if (cond) {
          take H = RW<int>(p);
          return;
      } else {
          take HA1 = W<int>(&im_a_global);
          return;
      }
  }
@*/

void access_global(int *p)
/*@ requires
      take y = MaybeGlobal(p, true);
    ensures
      take y_ = MaybeGlobal(p, true);
@*/
{}
