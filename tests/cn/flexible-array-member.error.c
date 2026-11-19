struct char_vector { 
  int len; 
  char chars[]; 
};

void bad_member_access (struct char_vector v) 
/*@ requires v.chars[0] == 0; @*/
{
  return;
}
