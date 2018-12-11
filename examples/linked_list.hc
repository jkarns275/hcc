
struct Element {
    i64 age;
    i64 salary;
    i8 married;

    i0 init(i64 age, i64 salary, i8 married) {
        this->age = age;
        this->salary = salary;
        this->married = married;
    }

    i8 equals(struct Element* other) {
        if (this->age != other->age) return 0;
        if (this->salary != other->salary) return 0;
        if (this->married != other->married) return 0;
        return 1;
    }
};

struct LinkedList {
    struct Element* el;
    struct LinkedList* next;

    u0 init() {
        this->el = 0;
        this->next = 0;
    }

    u0 init_new(struct Element* velem, struct LinkedList* vnext) {
        this->el = velem;
        this->next = vnext;
    }

    struct LinkedList* insert(struct Element* elem) {
        struct LinkedList* a3, *a2;
        a3 = this;
        a2 = new struct LinkedList;
        a2->init_new(elem, a3);
        return a2;
    }
    
    struct LinkedList* delete(struct Element* elem) {
        struct LinkedList* prev = 0;
        struct LinkedList* cur = this;
        while (cur->next != 0) {
            if (cur->el->equals(elem)) {
                if (prev == 0) return cur->next;
                print(-555);
                print(-555);
                prev->next = cur->next;
                return this;
            }
            prev = cur;
            cur = cur->next;
        }
        return this;
    }

    i0 print() {
        struct LinkedList* cur = this;
        while (cur->next) {
            print(cur->el->age);
            cur = cur->next;
        }
    }

    i8 search(struct Element* e) {
        struct LinkedList* cur = this;
        while (cur->next) {
            if (cur->el->equals(e)) return 1;
            cur = cur->next;
        }
    }
};

i64 main() {
    struct LinkedList* head = new struct LinkedList;
    struct Element* e1 = new struct Element;
    struct Element* e2;
    struct Element* e3; 
    struct Element* e4; 
 
    e1->init(25, 3700, 0);
    head = head->insert(e1);
    head->print(); // Prints 25
    print(100000);
    
    e2 = new struct Element;
    e2->init(39, 42000, 0);
    head = head->insert(e2);
    head->print();
    print(100000);
    
    e3 = new struct Element;
    e3->init(22, 34000, 0);
    head = head->insert(e3);
    head->print(); // prints 22 39 25
    
    e4 = new struct Element;
    e4->init(27, 24000, 0);
    print(head->search(e2)); // 1
    print(head->search(e4)); // 0
    print(10000000);
    e1 = new struct Element;
    e1->init(28, 35000, 0);
    head = head->insert(e1);
    head->print(); // 28 22 39 25
    print(2220000);

    head = head->delete(e2);
    head->print(); // 28 22 25
    print(33300000);
 
    head = head->delete(e1);
    head->print(); // 28 22
    print(44440000);
}



