use ast::structure::Structure;
use pest::iterators::{Pair, Pairs};
use parser;
use std::collections::HashMap;

macro_rules! ident {
    ( $pairs_name:ident, $ident_bank:expr, $span:expr ) => {
        if let Some(r) = $pairs_name .next() {
            if r.as_rule() != Rule::ident {
                return Err(AstError::new("Expected ident.", r.as_span()));
            }
            let ident = r.into_span().as_str();
            $ident_bank .get_id(ident)
        } else {
            return Err(AstError::new("Expected ident.", $span));
        }
    }
}

macro_rules! expect {
    ( $pairs_name:ident, $rule:expr, $rule_str:expr, $span:expr ) => {
        if let Some(r) = $pairs_name .next() {
            if r.as_rule() != $rule {
                return Err(AstError::new(concat!("Expected rule \"", $rule_str, ".\""), r.as_span()));
            }
            r
        } else {
            return Err(AstError::new("Unexpected end of tokens.", $span));
        }
    }
}

pub mod context;
pub mod declarator;
pub mod expr;
pub mod ty;
pub mod structure;
pub mod id;
pub mod function;
pub mod statement;
pub mod declaration;

use self::ty::Ty;
use std::rc::Rc;
use self::id::{Id, IdStore};
use self::expr::Expr;
use self::function::Function;
use super::parser::Rule;
use pest::Span;

pub struct PosSpan {
    start: usize,
    end: usize
}

impl PosSpan {
    pub fn from_span(span: Span) -> Self {
        PosSpan { start: span.start(), end: span.end() }
    }
}

pub struct AstError {
    pub err_msg: String,
    pub start: usize
}

impl AstError {
    pub fn new<S: Into<String>>(err_msg: S, span: Span)-> Self {
        let start = span.start();
        let err_msg = err_msg.into();
        AstError { start, err_msg }
    }

    pub fn eof<S: Into<String>>(s: Option<S>) -> Self {
        if let Some(s) = s {
            AstError { start: 0, err_msg: s.into() }
        } else {
            AstError { start: 0, err_msg: "Unexpected end of input.".into() }
        }
    }
}


pub struct Ast<'a> {
    idstore: IdStore<'a>,
    structs: HashMap<Id, Rc<Structure>>,
    functions: HashMap<Id, Function>,
}

impl<'a> Ast<'a> {
    fn new<'r>() -> Ast<'r> {
        Ast {
            idstore: IdStore::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
        }
    }

    pub fn from_pairs(mut pairs: Pairs<'a, Rule>) -> Ast<'a> {
        let mut ast = Ast::new();

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
