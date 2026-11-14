// Under --impl=x86_64, plain char is signed.
int negative_char(void)
/*@ ensures return == 1; @*/
{
    char c = -1;
    return c < 0;
}

int char_param(char c)
/*@ ensures -128 <= return; return <= 127; @*/
{
    return c;
}

int main(void) {
    negative_char();
    char_param(0);
    return 0;
}
