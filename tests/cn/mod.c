/* Modulo by zero is an undefined behavior.
   Must specify that the second operand is not equal to zero */


int mod_bad (int x)
{
    return x % 0;
}

int mod1 (int x)
/*@ ensures return == 2; @*/
{
    return 5 % 3;
}

int mod2 (int x)
/*@ ensures return == 2; @*/
{
    return 5 % (-3);
}

int mod3 (int x)
/*@ ensures return == (-2); @*/
{
    return (-5) % 3;
}

int mod4 (int x)
/*@ ensures return == (-2); @*/
{
    return (-5) % (-3);
}
