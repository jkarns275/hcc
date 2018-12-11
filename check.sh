mkdir out
mkdir bin

hcc examples/binary_search.hc -o bin/binary_search
hcc examples/binary_tree.hc -o bin/binary_tree
hcc examples/bubble_sort.hc -o bin/bubble_sort
hcc examples/quicksort.hc -o bin/quicksort
hcc examples/tree_visitor.hc -o bin/tree_visitor
hcc examples/linear_search.hc -o bin/linear_search
hcc examples/linked_list.hc -o bin/linked_list


bin/binary_search > out/binary_search_hc
bin/binary_tree > out/binary_tree_hc
bin/bubble_sort > out/bubble_sort_hc
bin/quicksort > out/quicksort_hc
bin/tree_visitor > out/tree_visitor_hc
bin/linked_list > out/linked_list_hc
bin/linear_search > out/linear_search_hc

cd java_examples

javac BinaryTree.java
java BinaryTree > ../out/binary_tree_java

javac BinarySearch.java
java BinarySearch > ../out/binary_search_java

javac BubbleSort.java
java BubbleSort > ../out/bubble_sort_java

javac QuickSort.java
java QuickSort > ../out/quicksort_java

javac TreeVisitor.java
java TreeVisitor > ../out/tree_visitor_java

javac LinkedList.java
java LinkedList > ../out/linked_list_java

javac LinearSearch.java
java LinearSearch > ../out/linear_search_java

cd ..
echo "LinearSearch"
diff out/linear_search_java out/linear_search_hc

echo "LinkedList"
diff out/linked_list_java out/linked_list_hc

echo "TreeVisitor"
diff out/tree_visitor_java out/tree_visitor_hc

echo "BubbleSort"
diff out/bubble_sort_java out/bubble_sort_hc

echo "BinarySearch"
diff out/binary_search_java out/binary_search_hc

echo "BinaryTree"
diff out/binary_tree_java out/binary_tree_hc

echo "QuickSort"
diff out/quicksort_java out/quicksort_hc
