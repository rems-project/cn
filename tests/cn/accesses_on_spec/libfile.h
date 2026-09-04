int myval; 

int foo(int i); 
/*@
spec foo(integer i);
accesses myval; 
requires myval == 1; 
         i == 1; 
ensures  return == 0; 
@*/
