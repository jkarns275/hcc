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

    i8 delete(i64 key) {
        struct Tree* cnode;
        struct Tree* pnode;
        i64 cont, found, is_root, key_aux;
        cnode = this;
        pnode = this;
        cont = 1;
        found = 0;
        is_root = 1;
        while (cont) {
            key_aux = cnode->key;
            if (key < key_aux) {
                if (cnode->left != 0) {
                    pnode = cnode;
                    cnode = cnode->left;
                } else cont = 0;
            } else {
                if (key_aux < key) {
                    if (cnode->right != 0) {
                        pnode = cnode;
                        cnode = cnode->right;
                    } else cont = 0;
                } else {
                    if (is_root) {
                        if (and((cnode->right == 0), (cnode->left == 0))) {
                            
                        } else {
                            this->remove(pnode, cnode);
                        }
                    } else {
                        this->remove(pnode, cnode);
                    }
                    found = 1;
                    cont = 0;
                }
            }
        }
        return found;
    }

    i0 remove(struct Tree* pnode, struct Tree* cnode) {
        i64 ak1, ak2;
        if (cnode->left != 0)
            this->remove_left(pnode, cnode);
        else {
            if (cnode->right != 0)
                this->remove_right(pnode, cnode);
            else {
                ak1 = cnode->key;
                ak2 = pnode->left->key;
                if (ak1 == ak2) {
                    pnode->left = 0;
                } else {
                    pnode->right = 0;
                }
            }
        }
    }

    i0 remove_left(struct Tree* pnode, struct Tree* cnode) {
        while (cnode->left != 0) {
            cnode->key = cnode->left->key;
            pnode = cnode;
            cnode = cnode->left;
        }
        pnode->left = 0;
    }


    i0 remove_right(struct Tree* pnode, struct Tree* cnode) {
        while (cnode->right != 0) {
            cnode->key = cnode->right->key;
            pnode = cnode;
            cnode = cnode->right;
        }
        pnode->right = 0;
    }

    i8 search(i64 key) {
        if (this == 0) { return 0; }
        if (this->key > key) {
            if (this->left == 0) return 0;
            return this->left->search(key);
        } else if (this->key < key) {
            if (this->right == 0) return 0;
            return this->right->search(key);
        } else {
            return 1;
        }
    }

    i0 print() {
        this->recprint();
    }

    i0 recprint() {
        if (this->left != 0) {
            this->left->recprint();
        }
        print(this->key);
        if (this->right != 0) {
            this->right->recprint();
        }
    }
};

i64 main() {
    struct Tree* btp = new(struct Tree);
    btp->init();
    btp->insert(16);
    btp->print();
    print(10000000);
    btp->insert(8);
    btp->print();
    btp->insert(24);
    btp->insert(4);
    btp->insert(12);
    btp->insert(20);
    btp->insert(28);
    btp->insert(14);
    btp->print();

    print(btp->search(24));
    print(btp->search(12));
    print(btp->search(16));
    print(btp->search(50));
    print(btp->search(12));
    btp->delete(12);
    btp->print();
    print(btp->search(12));
    return 0;
}
