/*@
predicate i32 IfChain(pointer p, i32 i) {
    if (i <= 0i32) {
      return 0i32;
    } else if (i == 1i32) {
      take X = Owned<int>(p);
      return 0i32;
    } else {
      take X = Owned<int>(p);
      take X2 = Owned(array_shift<int>(p,1u64));
      return 0i32;
   }
}
@*/    

int main()
{

    return 0;
}
