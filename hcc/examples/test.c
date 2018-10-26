struct XorWowState : struct Hello {
    struct test** state2, state3;

    u0 test();
    u0 test2(i8 *c, i8 d) {
        i8 a = d;
        a *= a += a;
        i8 b = c;
        if (a < b) {
            print(1);
        } else {
            print(0 + 2 - 2);
        }

        while (a) {
            a -= 1;
        }

        for (i8 b = 10; b > a; b -= 1) {
            print(b);
        }
    }
};

i64 main() {
    for (;;) {
    }
    i8 c = 5, *b = &c;
}