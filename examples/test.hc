struct XorWowState {

    i64 test() { return 3; }
    u0 test2(i8 *c, i8 d) {
        i8 a = d;
        a *= a += a;
        i8 b = *c;
        if (a < b) {
            print(1);
        } else {
            print(0 + 2 - 2);
        }

        while (a) {
            a -= 1;
        }

        for (i8 e = 10; e > a; e -= 1) {
            print(e);
        }
    }
};

i64 main() {
    for (;;) {
    }
    i8 c = 5, *b = &c;
}