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

    #[test]
    fn oofc() {
        let res = CParser::parse(Rule::function_definition, "int main(int a) { int b = 4; }").unwrap_or_else(|e| panic!(format!("{:?}", e)));
        println!("{}", res);
    }
}
