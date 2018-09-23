#[macro_use]
extern crate nom;

extern crate pest;

#[macro_use]
extern crate pest_derive;

use pest::Parser;
#[derive(Parser)]
#[grammar = "grammar.pest"]
struct ParserStruct;

#[cfg(test)]
mod test {
    use super::{ParserStruct, Rule};
    use pest::Parser;

    #[test]
    fn oof() {
        let pairs = ParserStruct::parse(Rule::program,
r#"struct Point { x: i64, y: i64 }
impl Point {
    fn dist(&self, other: Point) -> i64 {
         return (self.x - other.x) + (self.y - other.y);
    }
 }
fn main() {
    let point: Point = Point { x: 19, y: 10 };

 }
"#)
            .unwrap_or_else(|e| panic!("{}", e));
        println!("{:?}", pairs);
    }

    #[test]
    fn foof() {
        return;
        let pairs = ParserStruct::parse(Rule::function,
r#"fn main() { let x: Point = Point{x:0x4,y:z}; }"#)
            .unwrap_or_else(|e| panic!("{}", e));
        println!("{}", pairs);
    }
}
