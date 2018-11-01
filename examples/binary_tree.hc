#include "rng.c"

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
        if (this->empty) {
            this->key = key;
            this->empty = 0;
        } else if (this->key > key) {
            if (this->left == 0) {
                this->left = malloc(sizeof (struct Tree));
                this->left->init();
            }
            this->left->insert(key);
        } else if (this->key < key) {
            if (this->right == 0) {
                this->right = malloc(sizeof (struct Tree));
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

int main() {
    struct Tree binary_tree;
    struct Tree* btp = &binary_tree;

    btp->init();

    struct XorWowState rng;
    seed_rng(&rng);
    for (i64 i = 0; i < 100; i += 1) {
        btp->insert(xorwow(&rng));
    }

    seed_rng(&rng);
    for (i64 i = 0; i < 100; i += 1) {
        if (btp->search(xorwow(&rng))) {
            continue;
        }
        return -1;
    }

    return 0;
}
