use ast::context::Context;
use ast::id::Id;
use ast::AstError;
use ast::PosSpan;

use visitors::typecheck::*;

use parser::Rule;

use pest::iterators::Pair;

#[derive(PartialEq, Eq)]
pub enum TypeCompatibility {
    None,
    CastTo(Ty),
    Ok,
}

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum TyKind {
    I0,
    I8,
    I64,
    Struct(Id),
    Error,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Ty {
    pub kind: TyKind,
    pub ptr: usize,
}

use ast::id::IdStore;

impl Ty {
    pub fn is_error(&self) -> bool {
        self.kind == TyKind::Error
    }

    pub fn to_code(&self, idstore: &IdStore) -> String {
        let mut t = match self.kind.clone() {
            TyKind::I0 => "i0".to_string(),
            TyKind::I8 => "i8".to_string(),
            TyKind::I64 => "i64".to_string(),
            TyKind::Struct(id) => format!("struct _{}", idstore.get_string(id).unwrap()),
            TyKind::Error =>  "/* error */ i64".to_string() // "<error>".to_string(),
        };
        for _ in 0..self.ptr {
            t.push('*')
        }
        t
    }

    pub fn to_string(&self, idstore: &IdStore) -> String {
        let mut t = match self.kind.clone() {
            TyKind::I0 => "i0".to_string(),
            TyKind::I8 => "i8".to_string(),
            TyKind::I64 => "i64".to_string(),
            TyKind::Struct(id) => format!("struct {}", idstore.get_string(id).unwrap()),
            TyKind::Error => "<error>".to_string(),
        };
        for _ in 0..self.ptr {
            t.push('*')
        }
        t
    }

    pub fn new(kind: TyKind) -> Self {
        Ty { kind, ptr: 0 }
    }

    pub fn error() -> Self {
        Ty {
            kind: TyKind::Error,
            ptr: 0,
        }
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
        match self.kind {
            TyKind::I8 | TyKind::I64 => true,
            _ => false,
        }
    }

    pub fn conforms_to_mag(&self, other: Ty, tc: &TypeChecker) -> u64 {
        let conformity;

        match (self.kind.clone(), other.kind.clone()) {
            (TyKind::Error, _) => 1,
            (_, TyKind::Error) => 1,
            (TyKind::Struct(mut self_name), TyKind::Struct(other_name)) => {
                conformity = 1;
                loop {
                    if self_name == other_name {
                        return conformity;
                    }

                    if let Some(parent_name) = tc.structs[&self_name].parent.clone() {
                        self_name = parent_name;
                    } else {
                        return 0;
                    }
                }
            }
            _ => {
                if self.kind == other.kind || (self.is_integral_type() && other.is_integral_type())
                {
                    1
                } else {
                    0
                }
            }
        }
    }

    pub fn compatibility_with(&self, other: &Ty) -> TypeCompatibility {
        match (self.ptr == 0, other.ptr == 0) {
            (true, true) => match (self.kind.clone(), other.kind.clone()) {
                (TyKind::Struct(exp), TyKind::Struct(got_name)) => {
                    if exp == got_name {
                        TypeCompatibility::Ok
                    } else {
                        TypeCompatibility::None
                    }
                }
                (TyKind::I64, TyKind::I64) => TypeCompatibility::Ok,
                (TyKind::I8, TyKind::I8) => TypeCompatibility::Ok,
                (TyKind::I64, TyKind::I8) => TypeCompatibility::CastTo(Ty::new(TyKind::I64)),
                (TyKind::I8, TyKind::I64) => TypeCompatibility::CastTo(Ty::new(TyKind::I8)),
                (_, _) => TypeCompatibility::None,
            },
            (true, false) => match self.kind.clone() {
                TyKind::Struct(_) => TypeCompatibility::None,
                TyKind::I64 => TypeCompatibility::CastTo(Ty::new(TyKind::I64)),
                TyKind::I8 => TypeCompatibility::CastTo(Ty::new(TyKind::I8)),
                _ => TypeCompatibility::None,
            },
            (false, true) => match other.kind.clone() {
                TyKind::I64 | TyKind::I8 => TypeCompatibility::CastTo(self.clone()),
                _ => TypeCompatibility::None,
            },
            (false, false) => TypeCompatibility::CastTo(self.clone()),
        }
    }

    pub fn conforms_to(&self, other: Ty, tc: &TypeChecker) -> bool {
        self.conforms_to_mag(other, tc) > 0
    }

    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context) -> Result<Ty, AstError> {
        match pair.as_rule() {
            Rule::type_name => Self::type_name_from_pair(pair, context),
            Rule::type_specifier => Self::type_specifier_from_pair(pair, context),
            _ => Err(AstError::new(
                format!("{:?}", pair.as_rule()),
                pair.as_span(),
            )),
        }
    }

    fn type_name_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Ty, AstError> {
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

    fn type_specifier_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Ty, AstError> {
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
                }
                Rule::void_kw => Ok(Ty::new(TyKind::I0)),
                Rule::int_type => match r.as_str() {
                    "i64" => Ok(Ty::new(TyKind::I64)),
                    "i8" => Ok(Ty::new(TyKind::I8)),
                    _ => panic!("Unsupported integer type"),
                },
                _ => Err(AstError::new(
                    format!("Unexpected token {:?}", r.as_rule()),
                    span,
                )),
            }
        } else {
            Err(AstError::new("Unexpected end of tokens in ty", span))
        }
    }

    pub fn has_field(&self, name: Id, tc: &TypeChecker) -> bool {
        if let TyKind::Struct(st_name) = self.kind.clone() {
            if let Some(s) = tc.structs.get(&st_name) {
                if s.fields.contains_key(&name) {
                    true
                } else {
                    false // self.super_has_field(name, tc).is_some()
                }
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn super_has_field(&self, name: Id, tc: &TypeChecker) -> Option<PosSpan> {
        if let TyKind::Struct(st_name) = self.kind.clone() {
            let parent;
            if let Some(s) = tc.structs.get(&st_name) {
                if let Some(p) = s.parent.as_ref() {
                    parent = *p;
                } else {
                    return None;
                }
            } else {
                return None;
            }
            if let Some(parent) = tc.structs.get(&parent) {
                if let Some(decl) = parent.fields.get(&name) {
                    return Some(decl.span.clone());
                }
            } else {
                return None;
            }
            Ty::new(TyKind::Struct(parent)).super_has_field(name, tc)
        } else {
            None
        }
    }

    pub fn inherits(&self, other: &Ty, tc: &TypeChecker) -> bool {
        if self.ptr != other.ptr {
            return false;
        }
        match (self.kind.clone(), other.kind.clone()) {
            (TyKind::Struct(self_st), TyKind::Struct(other)) => {
                if self_st == other {
                    true
                } else {
                    let mut curr_struct = tc.structs.get(&self_st);
                    while let Some(curr) = curr_struct.clone() {
                        if curr.name == other {
                            return true;
                        }
                        if let Some(parent) = curr.parent.as_ref() {
                            curr_struct = tc.structs.get(parent);
                        } else {
                            break;
                        }
                    }
                    false
                }
            }
            _ => true,
        }
    }

    pub fn has_circular_inheritence(&self, tc: &TypeChecker) -> bool {
        use std::collections::HashSet;
        if let TyKind::Struct(st_name) = self.kind.clone() {
            let mut parents = HashSet::<usize>::new();
            let mut curr = st_name;
            loop {
                if let Some(s) = tc.structs.get(&curr) {
                    if let Some(p) = s.parent.clone() {
                        parents.insert(curr);
                        if parents.contains(&p) {
                            return true;
                        }
                        curr = p;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        } else {
            false
        }
    }
}
