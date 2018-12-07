struct XorWowState {

    i64 test() { return 3; }
    u0 test2(i8 *c, i8 d) {
        print(*c + d);
    }
};

struct Junk : struct XorWowState {
    i8 poo_poo;
};

i64 main() {
    struct XorWowState* x = new(struct XorWowState);
    i8 c = 24;
    x->test2(&c, 2);
}