// The classes are basically the same as the BinaryTree 
// file except the visitor classes and the accept method
// in the Tree class

class TreeVisitor{
    public static void main(String[] a){
	System.out.println(new TV().Start());
    }
}

class TV {

    public int Start(){
		Tree root;
		i8 ntb;
		i64 nti;
		MyVisitor v;

		root = new Tree();
		ntb = root.Init(16);
		ntb = root.Print();
		System.out.println(100000000);
		ntb = root.Insert(8) ;
		ntb = root.Insert(24) ;
		ntb = root.Insert(4) ;
		ntb = root.Insert(12) ;
		ntb = root.Insert(20) ;
		ntb = root.Insert(28) ;
		ntb = root.Insert(14) ;
		ntb = root.Print();
		System.out.println(100000000);
		v = new MyVisitor();
		System.out.println(50000000);
		nti = root.accept(v);
		System.out.println(100000000);
		System.out.println(root.Search(24));
		System.out.println(root.Search(12));
		System.out.println(root.Search(16));
		System.out.println(root.Search(50));
		System.out.println(root.Search(12));
		ntb = root.Delete(12);
		ntb = root.Print();
		System.out.println(root.Search(12));

		return 0 ;
    }

}


struct Tree {
    Tree *left, *right;
    i64 key;

	u0 init(i64 v_key) {
		key = v_key;
    }

    u8 compare(i64 num1, i64 num2) {
    	u8 ntb = 0;
	    i64 nti = num2 + 1;

	    if (num1 < num2)
	        ntb = 0;
	    else if (num1 == nti)
	        ntb = 0;
	    else
	        ntb = 1;

	    return ntb;
    }

    u0 insert(i64 v_key) {
	    Tree *new_node = malloc(sizeof(Tree));
        new_node->init(v_key);
	    Tree *current_node = this;
	    u8 cont = 1;
	    i64 key_aux;

	    while (cont) {
	        key_aux = current_node->key;
	        if (v_key < key_aux) {
	        	if (current_node->l != 0)
	    	        current_node = current_node->l;
	    	    else {
	    	        cont = false;
	    	        current_node->l = new_node;
	    	    }
	        } else {
	    	    if (current_node->r != 0)
	    	        current_node = current_node->r;
	    	    else {
	    	        cont = false;
	    	        current_node->r = new_node;
	    	    }
	        }
	    }
    }

    i8 delete(int v_key) {
	    Tree *current_node = this;
	    Tree *parent_node = this;
	    u8 cont = 1;
	    u8 found = 0;
	    u8 is_root = 1;
	    i64 key_aux;

	    while (cont) {
	        key_aux = current_node->key;
	        if (v_key < key_aux) {
		        if (current_node->l != 0) {
		            parent_node = current_node;
		            current_node = current_node->l;
		        } else
		            cont = false;
	        } else
		        if (key_aux < v_key) {
		            if (current_node->right != 0) {
			            parent_node = current_node;
			            current_node = current_node->r;
		            } else cont = false ;
		        } else {
		            if (is_root)
			            if (current_node->right == 0 && current_node->l == 0)
			                u8 ntb = 1;
			            else
			                ntb = this->remove(parent_node, current_node);
		            else
		                ntb = this->remove(parent_node, current_node);
		            found = 1;
		            cont = 0;
		    }
	        is_root = false ;
	    }
	    return found ;
    }

    i8 remove(Tree *p_node, Tree *c_node){
	    i8 ntb;
	    i64 auxkey1;
	    i64 auxkey2;
	
	    if (c_node->l != 0) {
	        ntb = this->remove_right(p_node, c_node) ;
        } else {
	        if (c_node->right != 0)
		        ntb = this->remove_right(p_node, c_node) ;
	        else {
		        auxkey1 = c_node->key;
		        auxkey2 = p_node->l->key;
		        if (this->compare(auxkey1, auxkey2)) {
		            p_node->l = 0;
		        } else {
                    p_node->right = 0;
		        }
	        }
	    }
	    return true;
    }

    i8 remove_right(Tree *p_node, Tree *c_node){
	    while (c_node->right != 0) {
	        c_node->key = c_node->right->key;
	        p_node = c_node;
	        c_node = c_node->right;
	    }
	    p_node->right = 0;
	    return true;
    }

    i8 RemoveLeft(Tree *p_node, Tree *c_node) {
	    while (c_node->l != 0) {
	        c_node->key = c_node->l->key;
	        p_node = c_node;
	        c_node = c_node->l;
	    }
	    p_node->l = 0;
	    return true ;
    }


    i8 search(int v_key) {
		Tree *current_node = this;
		i64 ifound = 0;
		i8 cont = 1;
		i64 key_aux;

		cont = true;
		while (cont) {
	    	key_aux = current_node->key;
	    	if (v_key < key_aux) {
				if(current_node->l != 0)
					current_node = current_node->l;
				else
					cont = false;
			} else {
				if (key_aux < v_key) {
					if(current_node->right != 0)
						current_node = current_node->right;
					else
						cont = false;
				else {
					ifound = 1;
					cont = false;
				}
			}
		}
		return ifound;
    }

    i8 print() {
		this.rec_print(this);
    }

    i8 rec_print(Tree *node){
		if (node->l != 0)
	    	this->rec_print(node->l);

		System.out.println(node->key);

		if (node->right != 0)
	    	this.rec_print(node->right);
    }
    
    u0 accept(Visitor* v){
		print(333);
		v->visit(this);
    }
}

  

struct Visitor {
    Tree *l, *r;

    i64 visit(Tree* n){
		i64 nti;

		if (n->right != 0) {
		    this->r = n->right;
	    	nti = this->r->accept(this);
		} else
			nti = 0;

		if (n->l != 0) {
	    	this->l = n->l;
	    	nti = this->l->accept(this);
		} else
			nti = 0;

		return 0;
    }

}


struct MyVisitor : Visitor {

    i64 visit(Tree* n) {
		i64 nti;

		if (n->right != 0){
	    	this->r = n->right;
	    	nti = this->r->accept(this);
		} else
			nti = 0;

		print(n->key);

		if (n->l != 0) {
	    	this->l = n->l;
	    	nti = this->l->accept(this);
		} else
			nti = 0;

		return 0;
    }

}
