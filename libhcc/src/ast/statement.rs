use ast::context::Context;
use ast::declaration::Declaration;
use ast::expr::*;
use ast::id::Id;
use ast::ty::*;
use ast::AstError;
use ast::PosSpan;
use parser::Rule;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Span;
use std::collections::HashMap;
use std::prelude::v1::Result::Err;

/// Body / Compound Statement
pub struct Body {
    pub locals: HashMap<Id, Ty>,
    pub stmts: Vec<Statement>,
}

impl Body {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context) -> Result<Body, AstError> {
        debug_assert!(pair.as_rule() == Rule::compound_stmt);
        let mut pairs = pair.into_inner();
        let mut stmts = vec![];
        let mut locals = HashMap::new();
        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::declaration => {
                    for declaration in Declaration::from_pair(pair, context)? {
                        locals.insert(declaration.name, declaration.ty.clone());
                        stmts.push(Statement::Declaration(declaration));
                    }
                }
                _ => {
                    stmts.push(Statement::from_pair(pair, context)?);
                }
            };
        }
        Ok(Body { locals, stmts })
    }
}

pub struct IfStmt {
    pub cond: Expr,
    pub true_body: Statement,
    pub false_body: Option<Statement>,
}

impl IfStmt {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context) -> Result<IfStmt, AstError> {
        debug_assert!(pair.as_rule() == Rule::selection_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let _if = expect!(pairs, Rule::if_kw, "ifkw", span);
        let cond = Expr::from_pair(expect!(pairs, Rule::expr, "expr", span), context)?;
        let true_body = Statement::from_pair(expect!(pairs, Rule::stmt, "stmt", span), context)?;
        let false_body = if let Some(_else) = pairs.next() {
            let false_body = pairs.next().unwrap();
            Some(Statement::from_pair(false_body, context)?)
        } else {
            None
        };
        Ok(IfStmt {
            cond,
            true_body,
            false_body,
        })
    }
}

pub struct WhileStmt {
    pub cond: Expr,
    pub body: Statement,
}

impl WhileStmt {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Statement, AstError> {
        debug_assert!(pair.as_rule() == Rule::iteration_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();

        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::while_kw => WhileStmt::while_from_pairs(pairs, span, context),
                Rule::for_kw => WhileStmt::for_from_pairs(pairs, span, context),
                _ => Err(AstError::new(
                    format!("Unexpected rule {:?}.", pair.as_rule()),
                    pair.as_span(),
                )),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens in WhileStmt", span))
        }
    }

    fn for_from_pairs<'r>(
        mut pairs: Pairs<'r, Rule>,
        span: Span<'r>,
        context: &mut Context,
    ) -> Result<Statement, AstError> {
        let first =
            WhileStmt::for_first(expect!(pairs, Rule::for_first, "for first", span), context)?;
        let second = WhileStmt::for_second(
            expect!(pairs, Rule::for_second, "for second", span),
            context,
        )?;
        let third =
            WhileStmt::for_third(expect!(pairs, Rule::for_third, "for third", span), context)?;
        let body = Statement::from_pair(expect!(pairs, Rule::stmt, "stmt", span), context)?;
        let mut pbody = Body {
            locals: HashMap::new(),
            stmts: vec![body],
        };

        if let Some(exp) = third {
            pbody.stmts.push(Statement::Expr(exp));
        }

        let wh = Statement::While(box WhileStmt {
            cond: second,
            body: Statement::Body(box pbody),
        });

        let mut rbody = Body {
            locals: HashMap::new(),
            stmts: vec![],
        };

        if let Some(decls) = first {
            for dec in decls {
                rbody.stmts.push(Statement::Declaration(dec));
            }
        }

        rbody.stmts.push(wh);
        Ok(Statement::Body(box rbody))
    }

    fn for_first<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Option<Vec<Declaration>>, AstError> {
        debug_assert!(pair.as_rule() == Rule::for_first);
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            Ok(Some(Declaration::from_pair(next, context)?))
        } else {
            Ok(None)
        }
    }

    fn for_second<'r>(pair: Pair<'r, Rule>, context: &mut Context) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::for_second);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            if next.as_str() == ";" {
                Ok(Expr {
                    span: PosSpan::from_span(span),
                    expr: ExprKind::Number(1),
                    ty: Some(Ty::new(TyKind::I64)),
                })
            } else {
                return Expr::from_expr_stmt_pair(next, context);
            }
        } else {
            Ok(Expr {
                span: PosSpan::from_span(span),
                expr: ExprKind::Number(1),
                ty: Some(Ty::new(TyKind::I64)),
            })
        }
    }

    fn for_third<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Option<Expr>, AstError> {
        debug_assert!(pair.as_rule() == Rule::for_third);
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            Ok(Some(Expr::from_pair(next, context)?))
        } else {
            Ok(None)
        }
    }

    fn while_from_pairs<'r>(
        mut pairs: Pairs<'r, Rule>,
        span: Span<'r>,
        context: &mut Context,
    ) -> Result<Statement, AstError> {
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
    Return((Option<Expr>, PosSpan)),
}

impl JumpStmt {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<JumpStmt, AstError> {
        debug_assert!(pair.as_rule() == Rule::jump_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(r) = pairs.next() {
            Ok(match r.as_rule() {
                Rule::break_kw => JumpStmt::Break,
                Rule::return_kw => {
                    if let Some(pair) = pairs.next() {
                        JumpStmt::Return((
                            Some(Expr::from_pair(pair, context)?),
                            PosSpan::from_span(span),
                        ))
                    } else {
                        JumpStmt::Return((None, PosSpan::from_span(span)))
                    }
                }
                Rule::continue_kw => JumpStmt::Continue,
                _ => unreachable!("Unexpected inner pair rule for jump_stmt"),
            })
        } else {
            Err(AstError::new("Unexpected end of tokens.", span))
        }
    }
}

pub enum Statement {
    Body(Box<Body>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
    Expr(Expr),
    Jump(JumpStmt),
    Declaration(Declaration),
}

impl Statement {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Statement, AstError> {
        debug_assert!(pair.as_rule() == Rule::stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::compound_stmt => Ok(Statement::Body(box Body::from_pair(pair, context)?)),
                Rule::iteration_stmt => WhileStmt::from_pair(pair, context),
                Rule::selection_stmt => Ok(Statement::If(box IfStmt::from_pair(pair, context)?)),
                Rule::expr_stmt => Ok(Statement::Expr(Expr::from_expr_stmt_pair(pair, context)?)),
                Rule::jump_stmt => Ok(Statement::Jump(JumpStmt::from_pair(pair, context)?)),
                _ => Err(AstError::new(
                    format!("Unexpected rule: {:?}", pair.as_rule()),
                    span,
                )),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens.", span))
        }
    }
}
