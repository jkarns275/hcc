struct XorWowState : struct Hello {
    struct test** state2, state3;

    u0 test();
    u0 test2(u8 *c, u8 d) {
        u8 a = d;
        a *= a += a;
        u8 b = c;
        if (a < b) {
            print(1);
        } else {
            print(0 + 2 - 2);
        }

        while (a) {
            a -= 1;
        }

        for (u8 b = 10; b > a; b -= 1) {
            print(b);
        }
    }
};

int main() {
    for (;;) {
    }
    u8 c = 5, *b = &c;
}