int div_bad (int x)
{
    return x / 0;
}

int div1 ()
/*@ ensures return == 1; @*/
{
    return 5 / 3;
}

int div2 ()
/*@ ensures return == -1; @*/
{
    return 5 / (-3);
}

int div3 ()
/*@ ensures return == -1; @*/
{
    return (-5) / 3;
}

int div4 ()
/*@ ensures return == 1; @*/
{
    return (-5) / (-3);
}
