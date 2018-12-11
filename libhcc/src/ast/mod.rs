use ast::context::*;
use ast::structure::Structure;
use pest::iterators::*;
use std::collections::HashMap;

macro_rules! ident {
    ( $pairs_name:ident, $ident_bank:expr, $span:expr ) => {
        if let Some(r) = $pairs_name.next() {
            if r.as_rule() != Rule::ident {
                return Err(AstError::new("Expected ident.", r.as_span()));
            }
            let ident = r.into_span().as_str();
            $ident_bank.get_id(ident)
        } else {
            return Err(AstError::new("Expected ident.", $span));
        }
    };
}

macro_rules! expect {
    ( $pairs_name:ident, $rule:expr, $rule_str:expr, $span:expr ) => {
        if let Some(r) = $pairs_name.next() {
            if r.as_rule() != $rule {
                return Err(AstError::new(
                    format!(
                        concat!("Expected rule \"", $rule_str, ".\" Instead got {:?}"),
                        r.as_rule()
                    ),
                    r.as_span(),
                ));
            }
            r
        } else {
            return Err(AstError::new(
                concat!(
                    "Unexpected end of tokens in ",
                    stringify!($rule),
                    " ",
                    file!(),
                    ":",
                    line!()
                ),
                $span,
            ));
        }
    };
}

pub mod context;
pub mod declaration;
pub mod declarator;
pub mod expr;
pub mod function;
pub mod id;
pub mod statement;
pub mod structure;
pub mod ty;

use self::function::Function;
use self::id::{Id, IdStore};
use super::parser::Rule;
use pest::Span;
use std::rc::Rc;

#[derive(Clone, Copy, Debug)]
pub struct PosSpan {
    pub start: usize,
    pub end: usize,
}

impl PosSpan {
    pub fn from_span(span: Span) -> Self {
        PosSpan {
            start: span.start(),
            end: span.end(),
        }
    }
}

#[derive(Debug)]
pub struct AstError {
    pub err_msg: String,
    pub span: PosSpan,
}

impl AstError {
    pub fn new<S: Into<String>>(err_msg: S, span: Span) -> Self {
        AstError {
            span: PosSpan::from_span(span),
            err_msg: err_msg.into(),
        }
    }

    pub fn eof<S: Into<String>>(s: Option<S>) -> Self {
        if let Some(s) = s {
            AstError {
                span: PosSpan { start: 0, end: 0 },
                err_msg: s.into(),
            }
        } else {
            AstError {
                span: PosSpan { start: 0, end: 0 },
                err_msg: "Unexpected end of input.".into(),
            }
        }
    }
}

pub struct Ast {
    pub idstore: IdStore,
    pub structs: HashMap<Id, Rc<Structure>>,
    pub functions: HashMap<Id, Vec<Rc<Function>>>,
}

impl Ast {
    fn from_context<'r>(context: Context) -> Ast {
        Ast {
            idstore: context.idstore,
            functions: context.functions,
            structs: context.structs,
        }
    }

    pub fn from_pairs(mut pairs: Pairs<Rule>) -> Result<Ast, Vec<AstError>> {
        let program = pairs.next().unwrap();
        let mut context = Context::new();

        if program.as_rule() != Rule::program {
            panic!("From pairs requires a program pair as root");
        }

        let mut program = program.into_inner();

        let mut methods = vec![];
        let main = context.idstore.get_id("main");

        while let Some(pair) = program.next() {
            match pair.as_rule() {
                Rule::struct_or_union_spec => {
                    let s = match Structure::from_pair(pair, &mut context) {
                        Ok(s) => s,
                        Err(e) => {
                            context.errors.push(e);
                            continue;
                        }
                    };
                    context.structs.insert(s.name, Rc::new(s));
                }
                Rule::function_definition => {
                    let f = match Function::from_pair(pair, &mut context) {
                        Ok(s) => s,
                        Err(e) => {
                            context.errors.push(e);
                            continue;
                        }
                    };
                    if f.method.is_some() {
                        methods.push(f);
                    } else {
                        let fnlist = context.functions.entry(f.name).or_insert_with(|| vec![]);
                        fnlist.push(Rc::new(f));
                    }
                    // TODO: Could do a type / overload check before adding?
                }
                Rule::EOI => {}
                _ => {
                    println!("Encountered unexpected rule {:?}", pair.as_rule());
                }
            }
        }

        use std::rc::Rc;

        while let Some(ref mut method) = methods.pop() {
            let st_name = method.method.clone().unwrap();
            if let Some(ref mut st) = Rc::get_mut(context.structs.get_mut(&st_name).unwrap()) {
                let match_ind = if let Some(ref methods) = st.methods.get(&method.name) {
                    let mut i = 0;
                    let mut matched = None;
                    for m in methods.iter() {
                        if m.header_equals(method) {
                            if matched.is_some() {
                                context.errors.push(AstError {
                                    err_msg: format!("Duplicate method definition"),
                                    span: method.span.clone(),
                                })
                            } else {
                                matched = Some(i);
                            }
                        }
                        i += 1;
                    }
                    matched
                } else {
                    None
                };
                if let Some(ind) = match_ind {
                    if let Some(ref mut meth) = st.methods.get_mut(&method.name) {
                        if meth[ind].body.is_none() {
                            meth[ind].body = Some(method.body.take().unwrap());
                        } else {
                            context.errors.push(AstError {
                                err_msg: format!("This method already has a definition"),
                                span: method.span.clone(),
                            });
                        }
                    }
                } else {
                    context.errors.push(AstError {
                        err_msg: format!(
                            "This function definition does not match any function header"
                        ),
                        span: method.span.clone(),
                    });
                }
            } else {
                context.errors.push(AstError {
                    err_msg: "No such struct.".to_string(),
                    span: method.span.clone(),
                });
            }
        }

        use parser::*;
        use pest::Parser;
        let mut print = Function::from_pair(
            CParser::parse(Rule::function_header, "i0 print(i64 a);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut malloc = Function::from_pair(
            CParser::parse(Rule::function_header, "i0* malloc(i64 nbytes);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut puti64 = Function::from_pair(
            CParser::parse(Rule::function_header, "i0 puti64(i64 a);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut putch = Function::from_pair(
            CParser::parse(Rule::function_header, "i0 putch(i64 a);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut flush = Function::from_pair(
            CParser::parse(Rule::function_header, "i0 flush();")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut sleepms = Function::from_pair(
            CParser::parse(Rule::function_header, "i0 sleepms(i64 ms);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut xor = Function::from_pair(
            CParser::parse(Rule::function_header, "i64 xor(i64 a, i64 b);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut and = Function::from_pair(
            CParser::parse(Rule::function_header, "i64 and(i64 a, i64 b);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();
        let mut or = Function::from_pair(
            CParser::parse(Rule::function_header, "i64 or(i64 a, i64 b);")
                .unwrap()
                .next()
                .unwrap(),
            &mut context,
        )
        .unwrap();

        print.intrinsic = true;
        malloc.intrinsic = true;
        puti64.intrinsic = true;
        putch.intrinsic = true;
        flush.intrinsic = true;
        sleepms.intrinsic = true;
        xor.intrinsic = true;
        and.intrinsic = true;
        or.intrinsic = true;

        context
            .functions
            .entry(context.idstore.get_id("print"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(print));
        context
            .functions
            .entry(context.idstore.get_id("malloc"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(malloc));
        context
            .functions
            .entry(context.idstore.get_id("puti64"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(puti64));
        context
            .functions
            .entry(context.idstore.get_id("putch"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(putch));
        context
            .functions
            .entry(context.idstore.get_id("flush"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(flush));
        context
            .functions
            .entry(context.idstore.get_id("sleepms"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(sleepms));
        context
            .functions
            .entry(context.idstore.get_id("xor"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(xor));
        context
            .functions
            .entry(context.idstore.get_id("and"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(and));
        context
            .functions
            .entry(context.idstore.get_id("or"))
            .or_insert_with(|| vec![])
            .insert(0, Rc::new(or));

        let r = if context.errors.len() != 0 {
            Err(context.errors)
        } else {
            Ok(Ast::from_context(context))
        };
        r
    }
}
