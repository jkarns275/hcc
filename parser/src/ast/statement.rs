use ast::ty::Ty;
use ast::id::Id;
use std::collections::HashMap;
use ast::expr::Expr;
use ast::context::Context;
use pest::iterators::Pair;
use parser::Rule;
use ast::AstError;
use ast::function::Function;
use std::prelude::v1::Result::Err;

/// Body / Compound Statement
pub struct Body {
    pub locals: HashMap<Id, Ty>,
    pub stmts: Vec<Statement>,
}

impl Body {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Function, AstError> {
        debug_assert!(pair.as_rule() == Rule::compount_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let mut stmts = vec![];
        panic!();
        while let Some(pair) = pairs.next() {
            let stmt = match pair.as_rule() {
                _ => return Err(AstError::new(format!("Unexpected rule: {:?}", pair.as_rule()), span)),
            };
        };
    }
}

pub struct IfStmt {
    pub cond: Expr,
    pub true_body: Statement,
    pub false_body: Statement
}

pub struct WhileStmt {
    pub cond: Expr,
    pub body: Statement,
}

pub enum Statement {
    Body(Box<Body>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
    Expr(Box<Expr>),
    Return(Box<Expr>),
}

