#include "rng.hc"

struct Tree {
    struct Tree *left, *right;
    i64 key;
    i8 empty;

    void init() {
        this->empty = 1;
        this->left = 0;
        this->right = 0;
    }

    u0 insert(i64 key) {
        if (this == 0) { return 0; }
        if (this->empty) {
            this->key = key;
            this->empty = 0;
        } else if (this->key > key) {
            if (this->left == 0) {
                this->left = new(struct Tree);
                this->left->init();
            }
            this->left->insert(key);
        } else if (this->key < key) {
            if (this->right == 0) {
                this->right = new(struct Tree);
                this->right->init();
            }
            this->right->insert(key);
        }
    }

    i8 search(i64 key) {
        if (this == 0) { return 0; }
        if (this->key > key) {
            this->left->search(key);
        } else if (this->key < key) {
            this->right->search(key);
        } else {
            return 1;
        }
    }
};

i64 main() {
    struct Tree* btp = new(struct Tree);
    btp->init();

    struct XorWowState rng;
    seed_rng(&rng);
    i64 i = 0;
    while (i < 2) {
        print(i);
        btp->insert(xorwow(&rng) % 32);
        i += 1;
    }

    i = 0;
    seed_rng(&rng);
    while (i < 2) {
        print(i);
        i += 1;
        if (btp->search(xorwow(&rng) % 32)) {
            continue;
        }
        return -1;
    }

    return 0;
}
