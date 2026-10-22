struct char_vector { 
  int len; 
  char chars[]; 
};

/*@
type_synonym CV = {
  integer len,
  map<integer,integer> chars
}

predicate CV CharVector (pointer p) {
  take n = RW<int>(member_shift<struct char_vector>(p, len));
  let base = member_shift<struct char_vector>(p,chars);
  take cs = each (integer i; 0 <= i && i < n) { RW<char>(base+i) };
  return { len: n, chars: cs };
}
@*/

int count(struct char_vector *p) 
/*@ requires take cv1 = CharVector(p); 
    ensures  take cv2 = CharVector(p); 
             cv1 == cv2;
	     return == cv1.len;
@*/
{ 
  return p->len;
}

char access(struct char_vector *p, unsigned int n) 
/*@ requires take cv1 = CharVector(p); 
             n < cv1.len;
    ensures  take cv2 = CharVector(p); 
             cv1 == cv2;
	     return == cv1.chars[n];
@*/
{ 
  /*@ focus RW<char>, n; @*/
  /*@ instantiate n; @*/
  return p->chars[n];
}

/* int main(void)  */
/* /\*@ trusted @*\/ */
/* {  */
/*   struct char_vector v = { 0 };  */
/*   return count(&v);  */
/*   return access(&v);  */
/* } */
