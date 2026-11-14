// Under --impl=x86_64, a plain char may be negative.
int char_param(char c)
/*@ ensures 0 <= return; @*/
{
    return c;
}

int main(void) {
    char_param(0);
    return 0;
}
