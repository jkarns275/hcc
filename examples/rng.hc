struct XorWowState {
    i64* state;

    i64 aaa() {
        return 0;
    }

    i64 aaa() {
        return 2;
    }
};

i64 xorwow(struct XorWowState* state) {
    i64 s, t = state->state[3];
    state->state[3] = state->state[2];
    state->state[2] = state->state[1];
    state->state[1] = s = state->state[0];
    state->state[0] = t;
    return t + (state->state[4] += 362437);
}

void seed_rng(struct XorWowState* rng) {
    rng->state[0] = 123356245;
    rng->state[1] = *rng;
    rng->state[2] = 1235555;
    rng->state[3] = 154124;
    rng->state[4] = 11111;
}
