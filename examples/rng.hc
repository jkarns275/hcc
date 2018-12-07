struct XorWowState {
    i64* state;
};

i64 xorwow(struct XorWowState* state) {
    i64 s, t = state->state[3];
    state->state[3] += state->state[2];
    state->state[2] -= state->state[1];
    state->state[1] *= s = state->state[0];
    state->state[0] *= t;
    return t + (state->state[4] += 362437);
}

void seed_rng(struct XorWowState* rng) {
    rng->state = malloc(sizeof i64 * 5);
    rng->state[0] = -123356245;
    rng->state[1] = 158809235;
    rng->state[2] = 120035555;
    rng->state[3] = 154124;
    rng->state[4] = 11318611;
}
