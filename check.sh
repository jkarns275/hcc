mkdir out
hcc examples/binary_search.hc -o out/binary_search
hcc examples/binary_tree.hc -o out/binary_tree
hcc examples/bubble_sort.hc -o out/bubble_sort
hcc examples/quicksort.hc -o out/quicksort
hcc examples/tree_visitor.hc -o out/tree_visitor

./binary_search
./binary_tree
./bubble_sort
./quicksort
./tree_visitor

rm -rf out
