use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;
use super::Visitor;
use ast::PosSpan;

use std::cmp::Ordering;
use std::rc::Rc;
use std::collections::{VecDeque, HashMap};
use std::u64;

pub enum TypeErrorKind {
    ExpectedIntegral { got: Ty, },
    WrongType { got: Ty, expected: Ty, },
    NoSuchField { got: Ty, field: Id, },
    NoSuchFunction { fn_name: Id, },
    NoSuchMethod { got: Ty, method_name: Id, },
    CannotDeref { got: Ty },
    CannotDerefToVoid,
    InvalidIndex { got: Ty, },
    FunctionNotFound { fn_name: Id, },
    BadFunctionArgs { fn_name: Id },
    AmbiguousFunctionArguments { fn_name: Id, indices: Vec<usize>, },
    AmbiguousMethodArguments { method_name: Id, indices: Vec<(Id, usize)> },
    MethodCallOnPrimitive { ty: Ty, },
    Redeclaration { name: Id },
    NoSuchStruct { struct_name: Id },
    IncompatibleTypes { got: Ty, expected: Ty, },
    IllegalBinaryOperation { op: String, got: Ty, }
}

pub struct TypeError {
    pub err: TypeErrorKind,
    pub span: PosSpan,
}

pub enum TypeWarningKind {
    ImplicitCast { from: Ty, to: Ty, }
}

pub struct TypeWarning {
    pub wrn: TypeWarningKind,
    pub span: PosSpan,
}

pub struct TypeChecker {
    pub var_stack: VecDeque<HashMap<Id, (Ty, PosSpan)>>,
    pub fns: HashMap<Id, Vec<Rc<Function>>>,
    pub structs: HashMap<Id, Rc<Structure>>,
    pub errs: Vec<TypeError>,
    pub wrns: Vec<TypeWarning>,
    // stack
    pub numeric_type_hint: Vec<Ty>,
}

impl TypeChecker {

    pub fn new(structs: HashMap<Id, Rc<Structure>>, fns: HashMap<Id, Vec<Rc<Function>>>) -> Self {
        TypeChecker {
            structs,
            fns,
            errs: vec![],
            wrns: vec![],
            var_stack: VecDeque::with_capacity(8),
            numeric_type_hint: vec![Ty::new(TyKind::I64)]
        }
    }

    fn binary_expr_visit<T>(&mut self, op: &str, span: PosSpan, head: &mut Expr, tail: &mut [(T, Expr)])
        -> Ty {
        let mut errs = false;
        let mut tys = vec![self.visit_expr(head)];
        self.numeric_type_hint.push(tys[0].clone());
        for (_op, exp) in tail.iter_mut() {
            let ty = self.visit_expr(exp);
            self.numeric_type_hint.push(ty.clone());
            match ty.kind.clone() {
                TyKind::Struct(_) | TyKind::I0 => {
                    errs = true;
                    self.errs.push(
                        TypeError {
                            err: TypeErrorKind::IllegalBinaryOperation { op: op.to_string(), got: ty.clone() },
                            span: exp.span.clone(),
                        }
                    );
                },
                _ => {}
            }
            tys.push(ty);
        }

        for i in 0..tail.len() + 1 {
            self.numeric_type_hint.pop();
        }

        if errs { return Ty::error() }

        let mut largest_type = tys[0].clone();
        for ty in tys[1..].iter() {
            if ty.ptr > 0 { largest_type = ty.clone(); continue }
            match ty.kind.clone() {
                TyKind::Struct(_) | TyKind::I0 => unreachable!(),
                TyKind::I64 => 
                    if largest_type.kind == TyKind::I8 {
                        largest_type = ty.clone();
                    },
                _ => { },
            }
        }

        largest_type
    }

}

impl Visitor for TypeChecker {

    fn visit_struct(&mut self, it: &mut Structure) -> Ty {
        for methods in it.methods.iter_mut() {
            for f in methods.1.iter_mut() {
                self.visit_function(f);
            }
        }

        Ty::new(TyKind::Struct(it.name))
    }
    fn visit_while(&mut self, it: &mut WhileStmt) -> Ty {
        self.visit_stmt(&mut it.body);
        self.visit_expr(&mut it.cond);
        Ty::new(TyKind::I0)
    }
    fn visit_body(&mut self, it: &mut Body) -> Ty {
        self.var_stack.push_front(HashMap::new());
        for statement in it.stmts.iter_mut() {
            self.visit_stmt(statement);
            if let Statement::Declaration(ref mut dec) = statement {
                self.var_stack[0].insert(dec.name, (dec.ty.clone(), dec.span.clone()));
            }
        }
        self.var_stack.pop_front();
        Ty::new(TyKind::I0)
    }
    fn visit_if(&mut self, it: &mut IfStmt) -> Ty {
        let cond_ty = self.visit_expr(&mut it.cond);
        self.visit_stmt(&mut it.true_body);
        if let Some(ref mut false_body) = it.false_body.as_mut() {
            self.visit_stmt(false_body);
        }

        if !cond_ty.is_integral_type() {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::ExpectedIntegral { got: cond_ty },
                    span: it.cond.span,
                }
            );
        }
        Ty::new(TyKind::I0)
    }
    fn visit_jmp(&mut self, it: &mut JumpStmt) -> Ty {
        match it {
            JumpStmt::Break 
            | JumpStmt::Continue 
            | JumpStmt::Return(None)                => Ty::new(TyKind::I0),
            JumpStmt::Return(Some(ref mut expr))    => self.visit_expr(expr)  
        }
    }
    fn visit_declaration(&mut self, it: &mut Declaration) -> Ty {
        let expected = it.ty.clone();
        match &expected.kind {
            x @ TyKind::I64 | x @ TyKind::I8 => self.numeric_type_hint.push(Ty::new(x.clone())),
            _ => (),
        };
        let span = it.span.clone();
        let res = if let Some(ref mut expr) = it.initializer.as_mut() {
            let got = self.visit_expr(expr);
            match expected.compatibility_with(&got, self) {
                TypeCompatibility::Ok => expected.clone(),
                TypeCompatibility::None => {
                    self.errs.push(TypeError {
                        err: TypeErrorKind::IncompatibleTypes { got, expected },
                        span
                    });
                    Ty::new(TyKind::Error)
                },
                TypeCompatibility::CastTo(to) => {
                    self.wrns.push(TypeWarning { 
                        wrn: TypeWarningKind::ImplicitCast { from: got.clone(), to: to.clone() },
                        span,
                    });
                    **expr = Expr {
                        expr: ExprKind::Cast(box Cast {
                            to,
                            expr: **expr,
                        }),
                        span,
                        ty: None
                    };
                    expected.clone()
                }
            }
        } else {
            Ty::new(TyKind::I0)
        };
        self.numeric_type_hint.pop();
        res
    }

    fn visit_index(&mut self, it: &mut Index) -> Ty {
        let base_ty = self.visit_expr(&mut it.base);
        let index_ty = self.visit_expr(&mut it.offset);

        match index_ty.kind.clone() {
            TyKind::I64 | TyKind::I8 => { },
            _ => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::InvalidIndex { got: index_ty.clone(), },
                        span: it.offset.span.clone(),
                    }
                );
            }
        };

        if base_ty.ptr == 0 {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::CannotDeref { got: base_ty.clone() },
                    span: it.base.span,
                }
            );
            Ty::error()
        } else if base_ty.kind == TyKind::I0 && base_ty.ptr == 1 {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::CannotDerefToVoid,
                    span: it.base.span,
                }
            );
            Ty::error()
        } else {
            base_ty.derefed()
        }
    }
    fn visit_dot(&mut self, it: &mut Dot) -> Ty {
       let lhs_ty = self.visit_expr(&mut it.lhs);

        if !lhs_ty.has_field(it.field_name, self) {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::NoSuchField { got: lhs_ty, field: it.field_name },
                    span: it.lhs.span.clone()
                }
            );
        }
        match lhs_ty.kind {
            TyKind::Struct(id) => {
                if let Some(st) = self.structs.get(&id) {
                    if let Some(struct_field) 
                        = st.fields.get(&it.field_name) {
                        struct_field.ty.clone()
                    } else {
                        Ty::error()
                    }
                } else {
                    Ty::error()
                }
            },
            _ => Ty::error(),
        }
    }
    fn visit_deref(&mut self, it: &mut Expr) -> Ty {
        let ty: Ty = self.visit_expr(it);
        match (ty.ptr, ty.kind.clone()) {
            (0, _) => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::CannotDeref { got: ty },
                        span: it.span.clone(),
                    }
                );
                Ty::error()
            },
            (1, TyKind::I0) => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::CannotDerefToVoid,
                        span: it.span.clone(),
                    }
                );
                Ty::error()
            },
            _ => ty.derefed(),
        }
    }
    fn visit_call(&mut self, it: &mut Call) -> Ty {
        let mut args = vec![];
        for arg in it.args.iter_mut() {
            args.push(self.visit_expr(arg));
        }

        let mut min_conformity = u64::MAX;
        let mut conforming_indices = Vec::with_capacity(0);

        if let Some(ref fns) = self.fns.get(&it.fn_name).clone() {
            for i in 0..fns.len() {
                let conformity = fns[i].conforms_to(&mut it.args[..], self);
                if conformity > 0 {
                    if conformity < min_conformity {
                        conforming_indices.clear();
                        min_conformity = conformity;
                    }
                    conforming_indices.push(i);
                }
            }

            match conforming_indices.len().cmp(&1) {
                // fn was found but the arguments were a mismatch
                Ordering::Less => {
                    self.errs.push(
                        TypeError {
                            err: TypeErrorKind::BadFunctionArgs { fn_name: it.fn_name },
                            span: it.span.clone(),
                        }
                    );
                    Ty::error()
                },
                // fn found but there are two or more matches - cant choose which one to use.
                Ordering::Greater => {
                    self.errs.push(
                        TypeError {
                            err: TypeErrorKind::AmbiguousFunctionArguments { 
                                fn_name: it.fn_name,
                                indices: conforming_indices
                            },
                            span: it.span.clone(),
                        }
                    );
                    Ty::error()
                },
                Ordering::Equal => fns[0].return_type.clone(),
            }
        } else {
            // fn not found
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::NoSuchFunction { fn_name: it.fn_name },
                    span: it.span.clone(),
                }
            );
            Ty::error()
        }

    }
    fn visit_method_call(&mut self, it: &mut MethodCall) -> Ty {
        let mut lhs_ty = self.visit_expr(&mut it.lhs);

        let mut args = vec![lhs_ty.clone().ptr_to()];
        for arg in it.args.iter_mut() {
            args.push(self.visit_expr(arg));
        }
        
        let struct_name = match lhs_ty.kind.clone() {
            TyKind::Struct(id) => id,
            ty @ _ => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::MethodCallOnPrimitive { ty: lhs_ty },
                        span: it.lhs.span.clone(),
                    }
                );
                return Ty::error()
            }
        };

        let mut depth = 0;
        let mut min_conformity = u64::MAX;
        let mut conforming_struct = self.structs.get(&struct_name).map(|r| (*r).clone());
        let mut conforming_indices = vec![];
        let mut cur_struct = Some(struct_name);
        while let Some(name) = cur_struct.clone() {
            if let Some(ref st) = self.structs.get_mut(&name).map(|t| t.clone()) {
                let struct_name = st.name;
                if let Some(ref methods) = st.methods.get(&it.method_name).map(|r| r.clone()) {
                    for i in 0..methods.len() {
                        let conformity = methods.get(i).unwrap().conforms_to(&mut it.args[..], self) + depth;
                        if conformity > 0 {
                            if conformity < min_conformity {
                                conforming_indices.clear();
                                min_conformity = conformity;
                                conforming_struct = self.structs.get(&struct_name).map(|r| (*r).clone());
                            }
                            conforming_indices.push((struct_name, i));
                        }
                    }
                }
                cur_struct = st.parent.clone();
            }
            depth += 1;
        }

        match conforming_indices.len().cmp(&0) {
            Ordering::Less => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::NoSuchMethod {
                            got: Ty::new(TyKind::Struct(struct_name)), 
                            method_name: it.method_name,
                        },
                        span: it.lhs.span.clone(),
                    }
                );
                Ty::error()
            },
            Ordering::Greater => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::AmbiguousMethodArguments {
                            method_name: it.method_name, 
                            indices: conforming_indices,
                        },
                        span: it.lhs.span.clone(),
                    }
                );
                Ty::error()
            },
            Ordering::Equal => {
                let (struct_name, index) = conforming_indices[0].clone();
                conforming_struct.unwrap().methods[&struct_name][index].return_type.clone()
            },
        }
    }
    fn visit_sizeofexpr(&mut self, it: &mut Ty, span: PosSpan) -> Ty {
        match &it.kind {
            TyKind::Struct(name) => 
                if let Some(s) = self.structs.get(name) {
                    Ty::new(TyKind::I64)
                } else {
                    self.errs.push(
                        TypeError {
                            err: TypeErrorKind::NoSuchStruct { struct_name: *name },
                            span: span,
                        }
                    );
                    Ty::error()
                },
            _ => Ty::new(TyKind::I64),
        }
    }
    fn visit_mulexpr(&mut self, it: &mut MulExpr) -> Ty {
        self.binary_expr_visit("*", it.span.clone(), &mut it.head, &mut it.tail[..])
    }
    fn visit_addexpr(&mut self, it: &mut AddExpr) -> Ty {
        Ty::new(TyKind::I0)
    }
    fn visit_cmpexpr(&mut self, it: &mut CmpExpr) -> Ty{
        Ty::new(TyKind::I0)

    }
    fn visit_eqexpr(&mut self, it: &mut EqExpr) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_inverseexpr(&mut self, it: &mut Expr) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_notexpr(&mut self, it: &mut Expr) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_assignexpr(&mut self, it: &mut AssignExpr) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_leaexpr(&mut self, it: &mut Expr) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_cast(&mut self, it: &mut Cast) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_ident(&mut self, it: &mut Id) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_number(&mut self, it: &mut i64) -> Ty {
        self.numeric_type_hint[self.numeric_type_hint.len() - 1].clone()
    }

    fn visit_function(&mut self, it: &mut Function) -> Ty {
        Ty::new(TyKind::I0)

    }
    fn visit_structure(&mut self, it: &mut Structure) -> Ty {
        Ty::new(TyKind::I0)   
    }
    fn visit_ty(&mut self, it: &mut Ty) -> Ty {
        Ty::new(TyKind::I0)
    }
    fn visit_id(&mut self, it: &mut Id) -> Ty {
        Ty::new(TyKind::I0)
    }
}