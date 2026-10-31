/* A confirmed postcondition failure must be backed by an executed input. */
int constant(void)
/*@ ensures return == 0; @*/
{
  return 1;
}
