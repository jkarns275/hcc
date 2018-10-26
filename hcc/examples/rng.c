struct XorWowState {
    i32 state[5];
};

i32 xorwow(struct XorWowState* state) {
    i32 s, t = state->state[3];
    t ^= t >> 2;
    t ^= t << 1;
    state->state[3] = state->state[2];
    state->state[2] = state->state[1];
    state->state[1] = s = state->state[0];
    t ^= s;
    t ^= s << 4;
    state->state[0] = t;
    return t + (state->state[4] += 362437);
}

void seed_rng(struct XorWowState* rng) {
    rng->state[0] = 123356245;
    rng->state[1] = 9890890;
    rng->state[2] = 1235555;
    rng->state[3] = 154124;
    rng->state[4] = 11111;
}
