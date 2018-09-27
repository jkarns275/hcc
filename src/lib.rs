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

    const simple_fn: &'static str = r#"extern i32 factorial(i32 n) {
    if (n <= 1) return 1;
    else return n * factorial(n - 1);
}
"#;

    #[test]
    fn oofc() {
        let res =
            CParser::parse(Rule::function_definition, simple_fn) //(i32 b, i32 c) {}")
                .unwrap_or_else(|e| panic!(format!("{ }", e)));
        println!("{}", res);
    }
}
