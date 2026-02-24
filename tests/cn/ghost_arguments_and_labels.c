void f() /*@ requires cn_ghost u64 size; true; @*/ {}

int main() {
    // works
    f(/*@ 14u64 @*/);

    int i = 0;
    while (i < 5) {
        // works
        f(/*@ 13u64 @*/);
        i++;
    }

    switch (i) {
        case 0:
            // does not work
            f(/*@ 27u64 @*/);
            break;
        default:
            // does not work
            f(/*@ 72u64 @*/);
    }

    goto t;

t:
    // does not work
    f(/*@ 15u64 @*/);
    return 0;
}
