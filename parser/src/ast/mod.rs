use ast::structure::Structure;
use pest::iterators::{Pair, Pairs};
use parser;
use std::collections::HashMap;

pub mod expr;
pub mod ty;
pub mod structure;
pub mod id;
pub mod function;
pub mod statement;

use self::ty::Ty;
use std::rc::Rc;
use self::id::{Id, IdStore};
use self::expr::Expr;
use self::function::Function;
use super::parser::Rule;

pub struct Declaration {
    pub ty: Ty, pub name: Id, pub initializer: Option<Expr>,
}
impl Declaration {
    pub fn from_pair<'a>(pair: Pair<'a, Rule>, idstore: &mut IdStore)
        -> Result<(Ty, Vec<Self>), String> {
        debug_assert!(pair.as_rule() == Rule::declaration);
        panic!()
    }
}

pub struct AST<'a> {
    idstore: IdStore<'a>,
    declarations: Vec<Declaration>,
    structs: HashMap<Id, Rc<Structure>>,
    functions: HashMap<Id, Function>,
}

impl<'a> AST<'a> {
    fn new<'r>() -> AST<'r> {
        AST {
            idstore: IdStore::new(),
            declarations: vec![],
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn from_pairs(mut pairs: Pairs<'a, Rule>) -> AST<'a> {
        let mut ast = AST::new();

        let program = pairs.next().unwrap();

        if program.as_rule() != Rule::program {
            panic!("From pairs requires a program pair as root");
        }

        let mut program = program.into_inner();

        while let Some(pair) = program.next() {

        }

        ast
    }
}
