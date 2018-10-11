use ast::structure::Structure;
use ast::function::Function;
use pest::iterators::Pairs;
use parser;

pub mod expr;
pub mod ty;
pub mod structure;
pub mod id;
pub mod function;
pub mod statement;

pub struct AST {
    structs: Vec<Structure>,
    functions: Vec<Function>,
}

impl AST {
    pub fn from_paris<'r>(pairs: Pairs<'r, parser::Rule>) {

    }
}