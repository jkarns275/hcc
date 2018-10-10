struct BS {
    i64* numbers;
    i64 size;

    u0 init(i64 size) {
        i64 j, k, aux01, aux02;
        this->size = size;
        this->numbers = malloc(sizeof i64 * size);
        j = 1;
        k = size + 1;
        while (j < size) {
            aux01 = 2 * j;
            aux02 = k - 3;
            this->numbers[j] = aux01 + aux02;
            j = j + k;
            k = k - 1;
        }
    }

    i64 run() {
        this->init(64);
        print(this->search(8));
        print(this->search(19));
        print(this->search(20));
        print(this->search(21));
        print(this->search(37));
        print(this->search(38));
        print(this->search(39));
        print(this->search(50));
    }

    i64 search(i64 f) {
        i64 index = 0;
        i64 size = this->size;
        while (index < size) {
            if (this->numbers[index] > f) {
                index = 2 * index + 2;
            } else if (this->numbers[index] < f) {
                index = 2 * index + 1;
            } else {
                return 1;
            }
        }
        return 1;
    }
};

int main() {
    struct BS bs;
    bs.run();
}


