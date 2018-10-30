use ast::context::Context;
use ast::id::Id;
use ast::ty::Ty;
use ast::AstError;
use ast::PosSpan;
use parser::Rule;

use pest::iterators::Pair;
use pest::iterators::Pairs;

use visitors::typecheck::*;

macro_rules! arithmetic_expr {
    ( $name:ident, $op_ty_name:ident ) => {
        pub struct $name {
            pub head: Expr,
            pub tail: Vec<($op_ty_name, Expr)>,
            pub span: PosSpan,
        }
    };
    ( $name:ident, $op_ty_name:ident, $rule:ident, $composed_of:ident, $composed_of_rule:ident ) => {
        arithmetic_expr!($name, $op_ty_name);
        impl $name {
            pub fn from_pair<'r>(
                pair: Pair<'r, Rule>,
                context: &mut Context<'r>,
            ) -> Result<Expr, AstError> {
                debug_assert!(pair.as_rule() == Rule::$rule);
                let span = pair.as_span();
                let mut pairs = pair.into_inner().peekable();
                if let Some(pair) = pairs.next() {
                    let span = pair.as_span();
                    if pairs.peek() == None {
                        $composed_of::from_pair(pair, context)
                    } else {
                        let mut tail = vec![];
                        let head = $composed_of::from_pair(pair, context)?;
                        loop {
                            if pairs.peek().is_none() {
                                break;
                            }
                            let pair = pairs.next();
                            if let Some(p) = pair {
                                let op = $op_ty_name::from_pair(p, context)?;
                                let rhs = $composed_of::from_pair(
                                    expect!(
                                        pairs,
                                        Rule::$composed_of_rule,
                                        stringify!($composed_of_rule),
                                        span
                                    ),
                                    context,
                                )?;
                                tail.push((op, rhs));
                            }
                        }
                        let expr = $name {
                            head,
                            tail,
                            span: PosSpan::from_span(span.clone()),
                        };
                        Ok(Expr {
                            span: PosSpan::from_span(span),
                            expr: ExprKind::$name(box expr),
                            ty: None,
                        })
                    }
                } else {
                    Err(AstError::new(
                        concat!("Unexpected end of tokens in ", stringify!($name)),
                        span,
                    ))
                }
            }
        }
    };
    ( $name:ident ) => {
        pub struct $name {
            pub items: Vec<Expr>,
        }
    };
}

arithmetic_expr!(MulExpr, MulOp, mul_expr, SizeofExpr, sizeof_expr);
arithmetic_expr!(AddExpr, AddOp, add_expr, MulExpr, mul_expr);
//arithmetic_expr!( ShiftExpr, ShiftOp );
arithmetic_expr!(CmpExpr, CmpOp, cmp_expr, AddExpr, add_expr);
arithmetic_expr!(EqExpr, EqOp, eq_expr, CmpExpr, cmp_expr);
arithmetic_expr!(AssignExpr, AssignOp, assign_expr, EqExpr, eq_expr);
//arithmetic_expr!( AndExpr );
//arithmetic_expr!( XorExpr );
//arithmetic_expr!( OrExpr );

pub enum MulOp {
    Mul,
    Div,
}

impl MulOp {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::mul_operator);
        let span = pair.as_span();
        Ok(match pair.as_str() {
            "/" => MulOp::Mul,
            "*" => MulOp::Div,
            _ => panic!("Invalid MulOp"),
        })
    }
}

pub enum AddOp {
    Sub,
    Add,
}

impl AddOp {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::add_operator);
        let span = pair.as_span();
        Ok(match pair.as_str() {
            "+" => AddOp::Add,
            "-" => AddOp::Sub,
            _ => panic!("Invalid AddOp"),
        })
    }
}
//pub enum ShiftOp { Lsh, Rsh }
pub enum CmpOp {
    Lt,
    Lte,
    Gt,
    Gte,
}

impl CmpOp {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::cmp_operator);
        let span = pair.as_span();
        Ok(match pair.as_str() {
            ">" => CmpOp::Gt,
            ">=" => CmpOp::Gte,
            "<=" => CmpOp::Lte,
            "<" => CmpOp::Lt,
            _ => panic!("Invalid CmpOp"),
        })
    }
}

pub enum AssignOp {
    Eq,
    Mul,
    Div,
    Add,
    Sub,
}

impl AssignOp {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::assign_operator);
        let span = pair.as_span();
        Ok(match pair.as_str() {
            "=" => AssignOp::Eq,
            "*=" => AssignOp::Mul,
            "+=" => AssignOp::Add,
            "-=" => AssignOp::Sub,
            "/=" => AssignOp::Div,
            _ => panic!("Not supported because im lazy"),
        })
    }
}

#[derive(Hash, Eq, PartialEq)]
pub enum EqOp {
    Eq,
    Neq,
}

impl EqOp {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::eq_operator);
        let span = pair.as_span();
        Ok(match pair.as_str() {
            "==" => EqOp::Eq,
            "!=" => EqOp::Neq,
            _ => panic!("Invalid EqOp"),
        })
    }
}

struct IntLit;
impl IntLit {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::int_lit);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            let posspan = PosSpan::from_span(next.as_span());
            let radix: u32 = match next.as_rule() {
                Rule::hex_lit => 16,
                Rule::dec_lit => 10,
                Rule::bin_lit => 2,
                _ => unreachable!("Something is wrong with the integer rule in the parser!"),
            };
            Ok(Expr {
                span: posspan,
                expr: ExprKind::Number(
                    i64::from_str_radix(next.as_str(), radix)
                        .expect("Parser allowed invalid integer literal!"),
                ),
                ty: None,
            })
        } else {
            Err(AstError::new("Unexpected end of tokens in IntLit", span))
        }
    }
}

struct PrimaryExpr;
impl PrimaryExpr {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::primary_expr);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            let span = next.as_rule();
            let posspan = PosSpan::from_span(next.as_span());
            Ok(match next.as_rule() {
                Rule::ident => {
                    let id = context.idstore.get_id(next.as_str());
                    Expr {
                        span: posspan,
                        expr: ExprKind::Ident(id),
                        ty: None,
                    }
                }
                Rule::int_lit => IntLit::from_pair(next, context)?,
                Rule::expr => Expr::from_pair(next, context)?,
                _ => unreachable!("Something is wrong with primary_expr rule"),
            })
        } else {
            Err(AstError::new(
                "Unexpected end of tokens in PrimaryExpr",
                span,
            ))
        }
    }
}

struct PostfixExpr;
impl PostfixExpr {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::postfix_expr);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let primary_expr = PrimaryExpr::from_pair(
            expect!(pairs, Rule::primary_expr, "primary expr", span),
            context,
        )?;
        let mut expr = primary_expr;
        while let Some(next) = pairs.next() {
            let span = next.as_span();
            let posspan = PosSpan::from_span(next.as_span());
            let r = next.as_rule();
            match r {
                Rule::postfix_index => {
                    let mut pairs = next.into_inner();
                    let index = Expr::from_pair(expect!(pairs, Rule::expr, "expr", span), context)?;
                    expr = Expr {
                        span: posspan,
                        expr: ExprKind::Index(box Index {
                            base: expr,
                            offset: index,
                        }),
                        ty: None,
                    };
                }
                Rule::postfix_call => {
                    let mut pairs = next.into_inner();
                    let mut expr_list: Pairs<'r, Rule> =
                        expect!(pairs, Rule::argument_expr_list, "argument expr list", span)
                            .into_inner();
                    let mut args = Vec::with_capacity(0);
                    while let Some(next) = expr_list.next() {
                        args.push(AssignExpr::from_pair(next, context)?);
                    }
                    if let ExprKind::Ident(id) = expr.expr {
                        expr = Expr {
                            span: posspan.clone(),
                            expr: ExprKind::Call(box Call {
                                fn_name: id,
                                args,
                                span: posspan
                            }),
                            ty: None,
                        }
                    } else {
                        return Err(AstError::new("This should have been parsed as a method call!", span))
                    }
                },
                Rule::postfix_dot_call | Rule::postfix_deref_call => {
                    let mut pairs = next.into_inner();
                    let id = ident!(pairs, context.idstore, span);
                    let mut expr_list: Pairs<'r, Rule> =
                        expect!(pairs, Rule::argument_expr_list, "argument expr list", span)
                            .into_inner();
                    let mut args = Vec::with_capacity(0);
                    while let Some(next) = expr_list.next() {
                        args.push(AssignExpr::from_pair(next, context)?);
                    }
                    // deref lhs if the deref operator was used.
                    if r == Rule::postfix_deref_call {
                        expr = Expr {
                            span: posspan,
                            expr: ExprKind::Deref(box expr),
                            ty: None,
                        }
                    }
                    expr = Expr {
                        span: posspan,
                        expr: ExprKind::MethodCall(box MethodCall {
                            lhs: expr,
                            method_name: id,
                            args,
                        }),
                        ty: None,
                    }
                },
                Rule::postfix_dot => {
                    let id = ident!(pairs, context.idstore, span);
                    expr = Expr {
                        span: posspan,
                        expr: ExprKind::Dot(box Dot { lhs: expr, field_name: id }),
                        ty: None,
                    };
                }
                Rule::postfix_deref => {
                    let id = ident!(pairs, context.idstore, span);
                    expr = Expr {
                        span: posspan,
                        expr: ExprKind::Dot(
                            box Dot { 
                                lhs: Expr {
                                    span: posspan,
                                    expr: ExprKind::Deref(box expr),
                                    ty: None,
                                },
                                field_name: id,
                            }
                        ),
                        ty: None,
                    };
                }
                _ => panic!(),
            }
        }
        Ok(expr)
    }
}

enum UnaryOp {
    Negate,
    Invert,
    Not,
    Deref,
    Lea,
}

impl UnaryOp {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::unary_operator);
        let span = pair.as_span();
        Ok(match pair.as_str() {
            "&" => UnaryOp::Lea,
            "-" => UnaryOp::Negate,
            "~" => UnaryOp::Invert,
            "*" => UnaryOp::Deref,
            "!" => UnaryOp::Not,
            _ => panic!("Invalid UnaryOp"),
        })
    }
}

struct UnaryExpr;
impl UnaryExpr {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::unary_expr);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            let span = next.as_span();
            Ok(match next.as_rule() {
                Rule::unary_operator => {
                    let unary_operator = UnaryOp::from_pair(next, context)?;
                    let cast_expr = box CastExpr::from_pair(
                        expect!(pairs, Rule::cast_expr, "cast expr", span),
                        context,
                    )?;
                    let posspan = PosSpan::from_span(span);
                    Expr {
                        span: posspan,
                        expr: match unary_operator {
                            UnaryOp::Lea => ExprKind::LeaExpr(cast_expr),
                            // -x = ~x + 1
                            UnaryOp::Negate => ExprKind::AddExpr(box AddExpr {
                                head: Expr {
                                    expr: ExprKind::InverseExpr(cast_expr),
                                    span: posspan.clone(),
                                    ty: None,
                                },
                                tail: vec![(
                                    AddOp::Add,
                                    Expr {
                                        expr: ExprKind::Number(1),
                                        span: posspan.clone(),
                                        ty: None,
                                    },
                                )],
                                span: posspan.clone(),
                            }),
                            UnaryOp::Invert => ExprKind::InverseExpr(cast_expr),
                            UnaryOp::Deref => ExprKind::Deref(cast_expr),
                            UnaryOp::Not => ExprKind::EqExpr(box EqExpr {
                                head: Expr {
                                    expr: ExprKind::InverseExpr(cast_expr),
                                    span: posspan.clone(),
                                    ty: None,
                                },
                                tail: vec![(
                                    EqOp::Eq,
                                    Expr {
                                        expr: ExprKind::Number(0),
                                        span: posspan.clone(),
                                        ty: None,
                                    },
                                )],
                                span: posspan.clone(),
                            }),
                        },
                        ty: None,
                    }
                }
                Rule::postfix_expr => PostfixExpr::from_pair(next, context)?,
                _ => unreachable!("Invalid unary expr"),
            })
        } else {
            Err(AstError::new("Unexpected end of tokens in UnaryExpr", span))
        }
    }
}

struct CastExpr;
impl CastExpr {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::cast_expr);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            let span = next.as_span();
            match next.as_rule() {
                Rule::unary_expr => UnaryExpr::from_pair(next, context),
                Rule::type_name => {
                    let ty = Ty::from_pair(next, context)?;
                    let cast_expr = CastExpr::from_pair(
                        expect!(pairs, Rule::cast_expr, "cast expr", span),
                        context,
                    )?;
                    Ok(Expr {
                        span: PosSpan::from_span(span),
                        expr: ExprKind::Cast(box Cast { to: ty, expr: cast_expr }),
                        ty: None,
                    })
                }
                _ => unreachable!("Something is wrong with the grammar!"),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens in CastExpr", span))
        }
    }
}

struct SizeofExpr;

impl SizeofExpr {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::sizeof_expr);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::cast_expr => CastExpr::from_pair(pair, context),
                Rule::sizeof_kw => {
                    let next = if let Some(next) = pairs.next() {
                        next
                    } else {
                        return Err(AstError::new("Unexpected end of tokens.", span));
                    };
                    match next.as_rule() {
                        Rule::type_specifier | Rule::type_name => {
                            let span = next.as_span();
                            let ty = Ty::from_pair(next, context)?;
                            Ok(Expr {
                                span: PosSpan::from_span(span),
                                expr: ExprKind::SizeOfExpr(ty),
                                ty: None,
                            })
                        }
                        _ => unreachable!("Something is wrong with the parser!"),
                    }
                }
                _ => unreachable!("Something is wrong with the parser!"),
            }
        } else {
            Err(AstError::new(
                "Unexpected end of tokens in SizeofExpr",
                span,
            ))
        }
    }
}

pub struct Index {
    pub base: Expr,
    pub offset: Expr,
}

pub struct Dot {
    pub field_name: Id,
    pub lhs: Expr,
}

pub struct Call {
    pub fn_name: Id,
    pub span: PosSpan,
    pub args: Vec<Expr>,
}

pub struct MethodCall {
    pub method_name: Id,
    pub args: Vec<Expr>, 
    pub lhs: Expr
}

pub struct Cast {
    to: Ty,
    expr: Expr,
}

pub enum ExprKind {
    Index(Box<Index>),
    Dot(Box<Dot>),
    Deref(Box<Expr>),
    MethodCall(Box<MethodCall>),
    Call(Box<Call>),
    SizeOfExpr(Ty),
    MulExpr(Box<MulExpr>),
    AddExpr(Box<AddExpr>),
    CmpExpr(Box<CmpExpr>),
    EqExpr(Box<EqExpr>),
    /// Bitwise negation
    InverseExpr(Box<Expr>),
    /// boolean negation
    NotExpr(Box<Expr>),
    AssignExpr(Box<AssignExpr>),
    LeaExpr(Box<Expr>),
    Cast(Box<Cast>),
    Ident(Id),
    Number(i64),
    NoOp,
}

pub struct Expr {
    pub span: PosSpan,
    pub expr: ExprKind,
    pub ty: Option<Ty>,
}

impl Expr {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::expr);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        AssignExpr::from_pair(
            expect!(pairs, Rule::assign_expr, "assign expr", span),
            context,
        )
    }

    pub fn from_expr_stmt_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context<'r>,
    ) -> Result<Expr, AstError> {
        debug_assert!(pair.as_rule() == Rule::expr_stmt);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        if let Some(next) = pairs.next() {
            Expr::from_pair(next, context)
        } else {
            Ok(Expr {
                span: PosSpan::from_span(span),
                expr: ExprKind::NoOp,
                ty: None,
            })
        }
    }

}
