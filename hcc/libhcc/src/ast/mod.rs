use ast::structure::Structure;
use pest::iterators::{Pair, Pairs};
use parser;
use std::collections::HashMap;
use ast::context::*;

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
                return Err(AstError::new(format!(concat!("Expected rule \"", $rule_str, ".\" Instead got {:?}"), r.as_rule()), r.as_span()));
            }
            r
        } else {
            return Err(AstError::new(concat!("Unexpected end of tokens in ", stringify!($rule), " ", file!(), ":", line!()), $span));
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

#[derive(Clone, Copy)]
pub struct PosSpan {
    pub start: usize,
    pub end: usize
}

impl PosSpan {
    pub fn from_span(span: Span) -> Self {
        PosSpan { start: span.start(), end: span.end() }
    }
}

#[derive(Debug)]
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
    pub idstore: IdStore<'a>,
    pub structs: HashMap<Id, Rc<Structure>>,
    pub functions: HashMap<Id, Function>,
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
        let mut context = Context::new();

        if program.as_rule() != Rule::program {
            panic!("From pairs requires a program pair as root");
        }

        let mut program = program.into_inner();

        while let Some(pair) = program.next() {
            match pair.as_rule() {
                Rule::struct_or_union_spec => {
                    let s = match Structure::from_pair(pair, &mut context) {
                        Ok(s) => s,
                        Err(e) => { println!("{:?}", e); context.errors.push(e); continue }
                    };
                    context.structs.insert(s.name, Rc::new(s));
                },
                Rule::function_definition => {
                    let f = match Function::from_pair(pair, &mut context) {
                        Ok(s) => s,
                        Err(e) => { println!("{:?}", e); context.errors.push(e); continue }
                    };
                    let fnlist = context.functions.entry(f.name).or_insert_with(|| vec![]);
                    fnlist.push(Rc::new(f));
                    // TODO: Could do a type / overload check before adding?
                },
                Rule::EOI => {},
                _ => {
                    println!("Encountered unexpected rule {:?}", pair.as_rule());
                }
            }
        }

        ast
    }
}
