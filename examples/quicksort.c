i64 partition(i64* numbers, i64 lo, i64 hi) {
    i64 pivot = numbers[hi];
    i64 i = lo;
    i64 tmp;
    for (i64 j = lo; j < hi; j += 1) {
        if (numbers[j] < pivot) {
            tmp = numbers[i];
            numbers[i] = numbers[j];
            numbers[j] = tmp;
            i += 1;
        }
    }
    tmp = numbers[i];
    numbers[i] = numbers[hi];
    numbers[hi] = tmp;
    return i;
}

u0 qs(i64* numbers, i64 lo, i64 hi) {
    if (lo < hi) {
        i64 p = partition(numbers, lo, hi);
        qs(numbers, lo, p - 1);
        qs(numbers, p + 1, hi);
    }
}

u0 sort(i64* numbers, i64 size) {
    qs(numbers, 0, size - 1);
}

i64 size = 10;
int main() {
    i64 *numbers = malloc(8 * size);
    numbers[0] = 20;
    numbers[1] = 7;
    numbers[2] = 12;
    numbers[3] = 18;
    numbers[4] = 2;
    numbers[5] = 11;
    numbers[6] = 6;
    numbers[7] = 9;
    numbers[8] = 19;
    numbers[9] = 5;

    sort(numbers, size);

    for (i64 i = 0; i < size; i += 1) {
        print(numbers[i]);
    }
}