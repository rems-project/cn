/* Since the second operand is constant that is not equal to zero,
   You can execute the division with no worries for Modulo By Zero */

/* Also see comment on CN's definition of rem_t_, in makeTerms.ml */

int x_mod_three (int x)
/*@ requires let r = mod(abs(x),3);
    ensures return == ((x < 0) ? (-r) : r); @*/
{
    return x % 3;
}

int x_mod_neg_three (int x)
/*@ requires let r = mod(abs(x),-3);
    ensures return == ((x < 0) ? (-r) : r); @*/
{
    return x % -3;
}

/* NOTE:
    If the first operand is positive or both operands are positive, the result will be positive.
            Ex: ( x % y ) =  ( x % - y )

    If the first operand is negative or both operands are negative, the result will be negative.
            Ex: ( -x % y ) = ( -x % -y ) = - ( x % y )
*/

int mod_first_operand_neg ()
/*@  ensures return == -2; @*/
{
    return -5 % 3;
}


