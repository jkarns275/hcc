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
use pest::iterators::Pairs;
use pest::Span;
use ast::declaration::Declaration;

/// Body / Compound Statement
pub struct Body {
    pub locals: HashMap<Id, Ty>,
    pub stmts: Vec<Statement>,
}

impl Body {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Body, AstError> {
        debug_assert!(pair.as_rule() == Rule::compound_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let mut stmts = vec![];
        let mut locals = HashMap::new();
        while let Some(pair) = pairs.next() { stmts.push(Statement::from_pair(pair, context)?); }
        Ok(Body {
            locals,
            stmts
        })
    }
}

pub struct IfStmt {
    pub cond: Expr,
    pub true_body: Statement,
    pub false_body: Statement
}

impl IfStmt {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<IfStmt, AstError> {
        panic!()
    }
}

pub struct WhileStmt {
    pub cond: Expr,
    pub body: Statement,
}

impl WhileStmt {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Statement, AstError> {
        debug_assert!(pair.as_rule() == Rule::iteration_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();

        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::while_kw  => WhileStmt::while_from_pairs(pairs, span, context),
                Rule::for_kw    => WhileStmt::for_from_pairs(pairs, span, context),
                _               =>
                    Err(AstError::new(format!("Unexpected rule {:?}.", pair.as_rule()), pair.as_span())),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens.", span))
        }
    }

    fn for_from_pairs<'r>(mut pairs: Pairs<'r, Rule>, span: Span<'r>, context: &mut Context<'r>)
        -> Result<Statement, AstError> {
        panic!();
    }

    fn for_first<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Option<Declaration>, AstError> {
        panic!();
    }

    fn for_second<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Option<Expr>, AstError> {

    }

    fn for_third<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Option<Expr>, AstError> {

    }

    fn while_from_pairs<'r>(mut pairs: Pairs<'r, Rule>, span: Span<'r>, context: &mut Context<'r>)
        -> Result<Statement, AstError> {
        let expr = expect!(pairs, Rule::expr, "expr", span);
        let cond = Expr::from_pair(expr, context)?;
        let body = expect!(pairs, Rule::stmt, "stmt", span);
        let body = Statement::from_pair(body, context)?;
        Ok(Statement::While(box WhileStmt { cond, body }))
    }
}

pub enum JumpStmt {
    Break,
    Continue,
    Return(Option<Expr>),
}

impl JumpStmt {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<JumpStmt, AstError> {
        debug_assert!(pair.as_rule() == Rule::jump_stmt);
        let span = pair.as_span();
        panic!()
    }
}

pub enum Statement {
    Body(Box<Body>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
    Expr(Box<Expr>),
    Jump(Box<JumpStmt>),
}

impl Statement {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Statement, AstError> {
        debug_assert!(pair.as_rule() == Rule::stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::compound_stmt =>
                    Ok(Statement::Body(box Body::from_pair(pair, context)?)),
                Rule::iteration_stmt =>
                    WhileStmt::from_pair(pair, context),
                Rule::selection_stmt =>
                    Ok(Statement::If(box IfStmt::from_pair(pair, context)?)),
                Rule::expr_stmt =>
                    Ok(Statement::Expr(box Expr::from_expr_stmt_pair(pair, context)?)),
                Rule::jump_stmt =>
                    Ok(Statement::Jump(box JumpStmt::from_pair(pair, context)?)),
                _ => Err(AstError::new(format!("Unexpected rule: {:?}", pair.as_rule()), span)),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens.", span))
        }
    }
}