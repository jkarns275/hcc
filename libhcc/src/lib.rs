#![feature(box_syntax)]

#[macro_use]
extern crate nom;

extern crate pest;

#[macro_use]
extern crate pest_derive;

pub mod ast;
pub mod parser;

#[cfg(test)]
mod test {
    use pest::Parser;
    use parser::*;
    use ast::Ast;

    const _parser_text: &'static str = include_str!(../src/c.pest);

    const BINARY_SEARCH: &'static str = include_str!(../../../examples/binary_search.c);
    const BINARY_TREE: &'static str = include_str!("../../examples/binary_tree.c");
    const QUICKSORT: &'static str = include_str!("../../examples/quicksort.c");
    const TREE_VISITOR: &'static str = include_str!("../../examples/tree_visitor.c");

    fn test_file(filename: &str) {
        let (src, lines, line_info)
            = apply_pre_processor(filename, "../examples");
        let result = parser(&src[..], &lines, &line_info);
        match &result {
            Ok(pairs) => { println!("Successfully parsed:\n {:#?}", pairs); },
            Err(e) => { println!("{}", e); }
        };
        let a = result.unwrap();
        let ast = Ast::from_pairs(a);
    }

    // #[test] fn tree_visitor() { test_file("tree_visitor.c"); }

    // #[test] fn binary_tree() { test_file("binary_tree.c"); }

    // #[test] fn heap() { test_file("binary_search.c"); }

    // #[test] fn quicksort() { test_file("quicksort.c"); }

     #[test]
    fn rng() { test_file("test.c"); }
}