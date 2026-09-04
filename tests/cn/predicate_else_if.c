/*@
predicate integer IfChain(pointer p, integer i) {
    if (i <= 0) {
      return 0;
    } else if (i == 1) {
      take X = Owned<int>(p);
      return 0;
    } else {
      take X = Owned<int>(p);
      take X2 = Owned(array_shift<int>(p,1));
      return 0;
   }
}
@*/    

int main()
{

    return 0;
}
