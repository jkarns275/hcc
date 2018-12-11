struct LS {
    i64* numbers;
    i64 len;

    u0 init(i64 sz) {
        i64 j, k, aux1, aux2;
        this->len = sz;
        this->numbers = malloc(sz * sizeof i64);
        j = 1;
        k = sz + 1;
        while ( j < sz ) {
            aux1 = 2 * j;
            aux2 = k - 3;
            this->numbers[j] = aux1 + aux2;
            j += 1;
            k -= 1;
        }
    }

    i64 search(i64 num) {
        i64 j, ifound, aux1, aux2, nt, ls1;
        j = 1;
        ls1 = 0;
        ifound = 0;

        while ( j < this->len ) {
            aux1 = this->numbers[j];
            aux2 = num + 1;
            if ( aux1 < num ) nt = 0;
            else if ( (aux1 < aux2) == 0 ) nt = 0;
            else {
                ls1 = 1;
                ifound = 1;
                j = this->len;
            }
            j += 1;
        }
        return ifound;
    }

    i0 print() {
        i64 i = 0;
        while (i < this->len) {
            print(this->numbers[i]);
            i += 1;
        }
    }
};

i64 main() {
    struct LS* ls = new struct LS;
    ls->init(10);
    ls->print();
    print(9999);
    print(ls->search(8));
    print(ls->search(12));
    print(ls->search(17));
    print(ls->search(50));
}
