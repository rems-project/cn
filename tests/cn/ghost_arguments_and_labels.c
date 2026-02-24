void f() /*@ requires cn_ghost u64 size; true; @*/ {}

int main() {
    f(/*@ 14u64 @*/);

    int i = 0;
    while (i < 5) {
        f(/*@ 13u64 @*/);
        i++;
    }

    switch (i) {
        case 0:
            f(/*@ 27u64 @*/);
            break;
        default:
            f(/*@ 72u64 @*/);
    }

    goto t;

t:
    f(/*@ 15u64 @*/);
    return 0;
}
