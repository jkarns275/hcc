#[macro_use]
extern crate nom;

extern crate pest;

#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "c.pest"]
struct CParser;

fn apply_pre_processor(file: &str, working_dir: &str) -> String {
    use std::process::{Command, Stdio};
    String::from_utf8_lossy(
        &Command::new("cpp")
            .arg(file)
            .current_dir(working_dir)
            .output()
            .unwrap().stdout[..])
        .to_string()

}

#[cfg(test)]
mod test {
    use pest::Parser;
    use super::{ CParser, Rule, apply_pre_processor };

    const _parser_text: &'static str = include_str!("c.pest");

    const binary_search_example: &'static str = include_str!("../../examples/binary_search.c");
    const binary_tree_example: &'static str = include_str!("../../examples/binary_tree.c");

    #[test]
    fn oofc() {
        let s = apply_pre_processor("binary_tree.c", "../examples");
        let res =
            CParser::parse(Rule::program, &s)
                .unwrap_or_else(|e| panic!(format!("{ }", e)));
        println!("{}", res);
    }
}
