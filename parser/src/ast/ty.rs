use std::rc::Rc;
use ast::structure::Structure;
use ast::AstError;
use pest::iterators::Pair;
use ast::id::Id;
use parser::Rule;
use ast::context::Context;

#[derive(PartialEq, Eq, Clone)]
pub enum Ty {
    I0,
    I8,
    I64,
    Ptr(Box<Ty>),
    Struct(Id),
}

impl Ty {
    pub fn ptr_to(self) -> Ty { Ty::Ptr(box self) }
    pub fn ptr_n_to(self, n: usize) -> Ty {
        if n == 0 {
            self
        } else {
            let mut ret = self;
            for i in 0..n {
                ret = ret.ptr_to();
            }
            ret
        }
    }
    /// Since types usually aren't contained within their own pair
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>) -> Result<Ty, AstError> {
        debug_assert!(pair.as_rule() == Rule::type_specifier);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let next = pairs.next();
        if let Some(r) = next {
            let span = r.as_span();
            match r.as_rule() {
                Rule::struct_or_union_spec => {
                    let mut pairs = r.into_inner();
                    let _ = expect!(pairs, Rule::struct_kw, "struct keyword", span);
                    let struct_name = ident!(pairs, context.idstore, span);
                    Ok(Ty::Struct(struct_name))
                },
                Rule::void_kw => {
                    Ok(Ty::I0)
                },
                Rule::int_type => {
                    match r.as_str() {
                        "i64"   => Ok(Ty::I64),
                        "i8"    => Ok(Ty::I8),
                        _     => panic!("Unsupported integer type"),
                    }
                },
                _ => Err(AstError::new("Unexpected token", span)),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens.", span))
        }
    }
}