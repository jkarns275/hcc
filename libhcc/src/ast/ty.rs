use std::rc::Rc;

use ast::structure::Structure;
use ast::context::Context;
use ast::AstError;
use ast::id::Id;

use parser::Rule;

use pest::iterators::Pair;

#[derive(PartialEq, Eq, Clone)]
pub enum TyKind {
    I0,
    I8,
    I64,
    Struct(Id),
    Error,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
    pub ptr: usize,
}

impl Ty {

    pub fn new(kind: TyKind) -> Self {
        Ty { kind, ptr: 0 }
    }

    pub fn error() -> Self {
        Ty { kind: TyKind::Error, ptr: 0 }
    }

    pub fn ptr_to(mut self) -> Ty {
        self.ptr += 1;
        self
    }
    pub fn ptr_n_to(mut self, n: usize) -> Ty {
        self.ptr += n;
        self
    }

    pub fn derefed(mut self) -> Ty {
        if self.ptr == 0 {
            self.kind = TyKind::Error;
        } else {
            self.ptr -= 1;
        }
        self
    }

    pub fn is_integral_type(&self) -> bool {
        match self {
            TyKind::I8 | TyKind::I64 => {
                true
            },
            _ => false,
        }
    }

    pub fn conforms_to_mag(&self, other: Ty, tc: &TypeChecker) -> u64 {
        let mut conformity = 0;
        
        match (self.kind.clone(), other.kind.clone()) {
            (TyKind::Error, _) => 1,
            (_, TyKind::Error) => 1,
            (TyKind::Struct(mut self_name), TyKind::Struct(mut other_name)) => {
                conformity = 1;
                loop {
                    if self_name == other_name {
                        return conformity
                    }

                    if let Some(parent_name) = tc.structs[self_name].parent.clone() {
                        self_name = parent_name;
                    } else {
                        return 0
                    }
                }
            },
            _ @ a => 
                if a.0 == a.b { 1 } 
                else if a.0.is_integral_type() && a.1.is_integral_type() { 1 }
                else { 0 },
        }
    }

    pub fn conforms_to(&self, other: Ty, tc: &TypeChecker) -> bool {
        self.conforms_to_mag(other, tc) > 0
    }

    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Ty, AstError> {
        let span = pair.as_span();
        match pair.as_rule() {
            Rule::type_name => Self::type_name_from_pair(pair, context),
            Rule::type_specifier => Self::type_specifier_from_pair(pair, context),
            _ => { Err(AstError::new(format!("{:?}", pair.as_rule()), pair.as_span())) },
        }
    }

    fn type_name_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Ty, AstError> {
        debug_assert!(pair.as_rule() == Rule::type_name);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ty_pair = expect!(pairs, Rule::type_specifier, "type specifier!!", span);
        let ty = Self::type_specifier_from_pair(ty_pair, context)?;
        if let Some(next) = pairs.next() {
            if next.as_rule() != Rule::pointer {
                Err(AstError::new("Expected pointer.", next.as_span()))
            } else {
                Ok(ty.ptr_n_to(next.as_str().matches('*').count()))
            }
        } else {
            Ok(ty)
        }
    }

    fn type_specifier_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>) -> Result<Ty, AstError> {
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
                    Ok(Ty::new(TyKind::Struct(struct_name)))
                },
                Rule::void_kw => {
                    Ok(Ty::new(TyKind::I0))
                },
                Rule::int_type => {
                    match r.as_str() {
                        "i64"   => Ok(Ty::new(TyKind::I64)),
                        "i8"    => Ok(Ty::new(TyKind::I8)),
                        _     => panic!("Unsupported integer type"),
                    }
                },
                _ => Err(AstError::new(format!("Unexpected token {:?}", r.as_rule()), span)),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens in ty", span))
        }
    }
}

