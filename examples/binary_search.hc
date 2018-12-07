struct BS {
    i64* numbers;
    i64 size;

    u0 init(i64 size) {
        i64 j, k, aux01, aux02;
        this->size = size;
        this->numbers = malloc((sizeof i64) * size);
        j = 1;
        k = size + 1;
        while (j < size) {
            aux01 = 2 * j;
            aux02 = k - 3;
            this->numbers[j] = aux01 + aux02;
            print(this->numbers[j]);
            j = j + 1;
            k = k - 1;
        }
        print(999999);
    }

    i64 run() {
        this->init(20);
        print(this->search(8));
        print(this->search(19));
        print(this->search(20));
        print(this->search(21));
        print(this->search(37));
        print(this->search(38));
        print(this->search(39));
        print(this->search(50));
        print(999);
    }

    i64 search(i64 num) {
        i8 bs1;
        i64 left, right;
        i8 var_cont;
        i64 medium, aux01, nt;
        aux01 = 0;
        bs1 = 0;
        right = this->size - 1;
        left = 0;
        var_cont = 1;
        while (var_cont) {
            medium = (left + right) / 2;
            aux01 = this->numbers[medium];
            if (num < aux01) right = medium - 1;
            else left = medium + 1;
            if (aux01 == num) var_cont = 0;
            else var_cont = 1;
            if (right < left) var_cont = 0;
            else nt = 0;
        }
        return aux01 == num;
    }
};

i64 main() {
    struct BS* bs = new(struct BS);
    bs->run();
}


