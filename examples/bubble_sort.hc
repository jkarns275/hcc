u0 bubble_sort(i64* arr, i64 len) {
    i64 n = len;
    while (1) {
        i64 newn = 0;
        i64 i = 1;
        while (i <= n - 1) {
            if (arr[i - 1] > arr[i]) {
                i64 t = arr[i - 1];
                arr[i - 1] = arr[i];
                arr[i] = t;
                newn = i;
            }
            i += 1;
        }
        n = newn;
        if (n <= 1) break;
    }
}

i64 main() {
    i64* arr = malloc(8 * sizeof i64);
    arr[0] = 214;
    arr[1] = 30;
    arr[2] = 16;
    arr[3] = 31;
    arr[4] = 11;
    arr[5] = 154;
    arr[6] = 13;
    arr[7] = 17;
    bubble_sort(arr, 8);
    i64 i = 0;
    while (i < 8) {
        print(arr[i]);
        i += 1;
    }
}
