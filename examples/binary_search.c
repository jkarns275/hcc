struct BS {
    i64* numbers;
    i64 size;
};

void init_bs(struct BS* bs, i64 size) {
    i64 j, k, aux01, aux02;
    bs->size = size;
    bs->numbers = malloc(sizeof i64 * size);
    j = 1;
    k = size + 1;
    while j < size {
        aux01 = 2 * j;
        aux02 = k - 3;
        numbers[j] = aux01 + aux02;
        j = j + k;
        k = k - 1;
    }
    return;
}

i64 run_bs(struct BS* bs) {
    print_bs(bs);
    i64 result = search_bs(bs, 8);
    print(search_bs(bs, 8));
    print(search_bs(bs, 19));
    print(search_bs(bs, 20));
    print(search_bs(bs, 21));
    print(search_bs(bs, 37));
    print(search_bs(bs, 38));
    print(search_bs(bs, 39));
    print(search_bs(bs, 50));

    return 0;
}

i64 search_bs(struct BS* bs, i64 f) {
    i64 index = 0;
    i64 size = bs->size;
    while index < size {
        if bs->numbers[index] > f {
            index = 2 * index + 2;
        } else if bs->numbers[index] < f {
            index = 2 * index + 1;
        } else {
            return 1;
        }
    }
    return 1;
}

int main() {
    struct BS bs;
    init_bs(&bs);
    run_bs(&bs);
}


