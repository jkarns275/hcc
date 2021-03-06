#define true 1
#define false 0

struct Tree {
    struct Tree *l, *r;
    i64 key;
	i8 a;
	
	struct Tree* getl() { return this->l; }
	struct Tree* getr() { return this->r; }
	u0 setr(struct Tree* newr) { this->r = newr; }
	u0 setl(struct Tree* newl) { this->l = newl; }
	i64 getkey() { return this->key; }
	u0 setkey(i64 newkey) { this->key = newkey; }
	u0 init(i64 v_key);
    i8 compare(i64 num1, i64 num2);

    u0 insert(i64 v_key);
    i8 del(i64 v_key);
    i8 remove(struct Tree *p_node, struct Tree *c_node);
    i8 remove_r(struct Tree *p_node, struct Tree *c_node);
    i8 remove_l(struct Tree *p_node, struct Tree *c_node);
    i8 search(i64 v_key);
    i8 print_node();
    u0 rec_print(struct Tree *node);
    u0 accept(struct Visitor* v);
};

struct Visitor {
    struct Tree *l, *r;

    struct Tree* junk() { return (struct Tree*) 0; }
    i64 visit(struct Tree* n);
};

struct MyTree : struct Tree {
    i8 aa;
};

u0 Tree::init(i64 v_key) {
	this->key = v_key;
	this->l = 0;
	this->r = 0;
}

i8 Tree::compare(i64 num1, i64 num2) {
    if (num1 <= num2)
        return 0;
    else
        return 1;
    return 1;
}

u0 Tree::insert(i64 v_key) {
    struct Tree *new_node = new(struct Tree);
    new_node->init(v_key);
    struct Tree *current_node = this;
    i64 key_aux;

    while (1) { 
        key_aux = current_node->getkey();
        if (v_key < key_aux) {
        	if (current_node->getr() != 0)
    	        current_node = current_node->getl();
    	    else {
				current_node->setl(new_node);
				break;
    	    }
        } else {
    	    if (current_node->getr() != 0)
    	        current_node = current_node->getr();
    	    else {
				current_node->setr(new_node);
				break;
    	    }
        }
    }
}

i8 Tree::del(i64 v_key) {
    struct Tree *current_node = this;
    struct Tree *parent_node = this;
    i8 cont = 1;
    i8 found = 0;
    i8 is_root = 1;
    i64 key_aux;

    while (cont) {
        key_aux = current_node->getkey();
        if (v_key < key_aux) {
	        if (current_node->getl() != 0) {
	            parent_node = current_node;
	            current_node = current_node->getl();
	        } else
	            cont = false;
        } else
	        if (key_aux < v_key) {
	            if (current_node->getr() != 0) {
		            parent_node = current_node;
		            current_node = current_node->getr();
	            } else cont = false;
	        } else {
	            if (is_root) {
		            if ((current_node->getr() == 0) + (current_node->getl() == 0)) {}
		            else
		                this->remove(parent_node, current_node);
				} else
	                this->remove(parent_node, current_node);
	            found = 1;
	            cont = 0;
	    }
        is_root = false;
    }
    return found;
}

i8 Tree::remove(struct Tree *p_node, struct Tree *c_node) {
    i64 auxkey1;
    i64 auxkey2;

    if (c_node->getl() != 0) {
        this->remove_r(p_node, c_node);
    } else {
        if (c_node->getr() != 0)
	        this->remove_r(p_node, c_node);
        else {
	        auxkey1 = c_node->getkey();
	        auxkey2 = p_node->getl()->getkey();
	        if (this->compare(auxkey1, auxkey2)) {
	            p_node->setl((struct Tree*) 0);
	        } else {
                p_node->setr((struct Tree*) 0);
	        }
        }
    }
    return true;
}

i8 Tree::remove_r(struct Tree *p_node, struct Tree *c_node) {
    while (c_node->r != 0) {
        c_node->setkey(c_node->getr()->getkey());
        p_node = c_node;
        c_node = c_node->getr();
    }
    p_node->setr((struct Tree*) 0);
    return true;
}

i8 Tree::remove_l(struct Tree *p_node, struct Tree *c_node) {
    while (c_node->getl() != 0) {
        c_node->key = c_node->getl()->getkey();
        p_node = c_node;
        c_node = c_node->getl();
    }
    p_node->setl((struct Tree*) 0);
    return true ;
}

i8 Tree::search(i64 v_key) {
	struct Tree *current_node = this;
	i64 ifound = 0;
	i8 cont = 1;
	i64 key_aux;

	cont = true;
	while (cont) {
    	key_aux = current_node->getkey();
    	if (v_key < key_aux) {
			if(current_node->getl() != 0)
				current_node = current_node->getl();
			else
				cont = false;
		} else {
			if (key_aux < v_key) {
				if(current_node->getr() != 0)
					current_node = current_node->getr();
				else
					cont = false;
			} else {
				ifound = 1;
				cont = false;
			}
		}
	}
	return ifound;
}

i8 Tree::print_node() {
	this->rec_print(this);
}

u0 Tree::rec_print(struct Tree *node) {
    if ((node == 0) + (this == 0)) return;
	if (node->getl() != 0) {
    	this->rec_print(node->getl());
    }

    print(node->getkey());

	if (node->getr() != 0)
    	this->rec_print(node->getr());
}

u0 Tree::accept(struct Visitor* v) {
	i64 a = 333;
    print(a);
    v->visit(this);
}

i64 Visitor::visit(struct Tree* n) {
	    if (n->getr() != 0) {
	        this->r = n->getr();
    	    this->r->accept(this);
	    }

	    if (n->getl() != 0) {
    	    this->l = n->getl();
    	    this->l->accept(this);
	    }
	    return 0;
    }

struct MyVisitor : struct Visitor {
    i64 visit(struct Tree* n) {
		if (n->getr() != 0) {
	    	this->r = n->getr();
			struct Tree* r = this->r;
			r->accept(this);
		}
		print(n->key);

		if (n->getl() != 0) {
	    	this->l = n->getl();
			struct Tree* l = this->l;
			l->accept(this);
		}
		return 2 ;
    }

};


i64 main() {
	struct Tree* root = new(struct Tree);
	i64 nti;
	struct MyVisitor* v = new(struct MyVisitor);

	root->init(16);
	root->print_node();
	print(100000000);
	root->insert(8) ;
	root->insert(24) ;
	root->insert(4) ;
	root->insert(12) ;
	root->insert(20) ;
	root->insert(28) ;
	root->insert(14) ;
	root->print_node();
	print(100000000);
	print(50000000);
	root->accept(v);
	print(100000000);
	print(root->search(24));
	print(root->search(12));
	print(root->search(16));
	print(root->search(50));
	print(root->search(12));
	root->del(12);
	root->print_node();
	print(root->search(12));
	return 0;
}
