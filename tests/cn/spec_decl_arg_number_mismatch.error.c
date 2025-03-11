extern int *createArray (unsigned int len);
/*@ spec createArray(pointer p, u32 len);
    requires true;
    ensures  take vp1 = each(u32 i; i < len) { RW(array_shift(return, i)) };
@*/
