use ast::ty::Ty;
use ast::id::Id;
use std::collections::HashMap;
use ast::expr::Expr;

/// Body / Compound Statement
pub struct Body {
    pub locals: HashMap<Id, Ty>,
    pub stmts: Vec<Statement>,
}

pub struct IfStmt {
    pub cond: Expr,
    true_body: Statement,
    false_body: Statement
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

