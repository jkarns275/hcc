#include "rng.c"

struct Tree {
    struct Tree *left, *right;
    i64 key;
    i8 empty;
};

void tree_init(struct Tree* tree) {
    tree->empty = 1;
    tree->left = 0;
    tree->right = 0;
}

void tree_insert(struct Tree* tree, i64 key) {
    if (tree->empty) {
        tree->key = key;
        tree->empty = 0;
    } else if (tree->key > key) {
        if (tree->left == 0) {
            tree->left = malloc(sizeof (struct Tree));
            tree_init(tree->left);
        }
        tree_insert(tree->left, key);
    } else if (tree->key < key) {
        if (tree->right == 0) {
            tree->right = malloc(sizeof (struct Tree));
            tree_init(tree->right);
        }
        tree_insert(tree->right, key);
    }
}

i8 tree_search(struct Tree* tree, i64 key) {
    if (tree == 0) { return 0; }
    if (tree->key > key) {
        tree_search(tree->left, key);
    } else if (tree->key < key) {
        tree_search(tree->right, key);
    } else {
        return 1;
    }
}


int main() {
    struct Tree binary_tree;
    struct Tree* btp = &binary_tree;

    tree_init(btp);

    struct XorWowState rng;
    seed_rng(&rng);
    for (i64 i = 0; i < 100; i += 1) {
        tree_insert(btp, xorwow(&rng));
    }

    seed_rng(&rng);
    for (i64 i = 0; i < 100; i += 1 {
        if tree_search(btp, xorwow(&rng)) {
            continue;
        }
        return -1;
    }

    if  (( tree_search(btp, 0) & tree_search(btp, 3) & tree_search(btp, 5)
        & tree_search(btp, 8) & tree_search(btp, 10) & tree_search(btp, 12)
        & tree_search(btp, 17) & tree_search(btp, 25)) == 0)  {
        return -1;
    }
    return 0;
}
