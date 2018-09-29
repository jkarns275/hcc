#[macro_use]
extern crate nom;

extern crate pest;

#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "c.pest"]
struct CParser;

#[cfg(test)]
mod test {
    use pest::Parser;
    use super::{ CParser, Rule };

    const _parser_text: &'static str = include_str!("c.pest");

    const binary_search_example: &'static str = include_str!("../../examples/binary_search.c");

    #[test]
    fn oofc() {
        let res =
            CParser::parse(Rule::program, binary_search_example)
                .unwrap_or_else(|e| panic!(format!("{ }", e)));
        println!("{}", res);
    }
}
