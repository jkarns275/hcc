use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;
use super::Visitor;
use ast::PosSpan;

use std::collections::{VecDeque, HashMap};

pub enum TypeErrorKind {
    ExpectedIntegral { got: Ty, },
    WrongType { got: Ty, expected: Ty, },
    NoSuchField { got: Ty, field: Id, },
    NoSuchFunction { fn_name: Id, },
    NoSuchMethod { got: Ty, method: Ty, },
    CannotDeref { got: Ty },
    CannotDerefToVoid,
    InvalidIndex { got: Ty, },
    FunctionNotFound { fn_name: Id, },
    BadFunctionArgs { fn_name: Id },
    AmbiguousFunctionArguments { fn_name: Id, indices: Vec<usize>, },
    AmbiguousMethodArguments { method_name: Id, indices: Vec<(Id, usize)> },
    MethodCallOnPrimitive { ty: Ty, }
}

pub struct TypeError {
    pub err: TypeErrorKind,
    pub span: PosSpan,
}

impl TypeError {
    pub fn new<S: Into<String>>(err_msg: S, span: PosSpan) -> Self {
        TypeError { err_msg: err_msg.into(), span }
    }
}

pub struct TypeChecker<'a> {
    pub var_stack: VecDeque<HashMap<Id, (Ty, PosSpan)>>,
    fns: HashMap<Id, Vec<Rc<Function>>>,
    structs: HashMap<Id, Rc<Structure>>,
    errs: Vec<TypeError>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(structs: HashMap<Id, Rc<Structure>>, fns: HashMap<Id, Vec<Function>>) -> Self {
        TypeChecker {
            structs,
            fns,
            errs: vec![],
            var_stack: VecDeque::with_capacity(8),
        }
    }

    pub fn get_str(&self, id: Id) -> &str {
        self.idbank.get_string(id).unwrap()
    }
}

impl Visitor for TypeChecker {
    fn visit_struct(&mut self, it: &mut Structure) {
        for method in it.methods.iter_mut() {
            self.visit_function(method);
        }
    }
    fn visit_while(&mut self, it: &mut WhileStmt) {
        visit_body(&mut it.body);
        visit_expr(&mut it.cond);
    }
    fn visit_body(&mut self, it: &mut Body) {
        self.var_stack.push_front(HashMap::new());
        for statement in it.stmts.iter_mut() {
            if let Statement::Declaration(ref mut dec) = statement {
                if self.var_stack[0].contains_key(dec.name) {
                    self.errs.push(
                        TypeError::new(format!()),
                    );
                    continue;
                }
            }
        }
        self.var_stack.pop_front();
    }
    fn visit_if(&mut self, it: &mut IfStmt) {
        self.visit_expr(&mut it.cond);
        self.visit_stmt(&mut it.true_body);
        if let Some(ref mut false_body) = it.false_body.as_mut() {
            self.visit_stmt(false_body);
        }

        let ty = it.cond.expr.typeof(self);
        if !ty.is_integral_type() {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::ExpectedIntegral { ty },
                    span: it.cond.span,
                }
            );
        }
        self.visit_body(&mut it.true_body)
        if let Some(ref mut f) = it.false_body.as_mut() {
            self.visit_body(f);
        }
    }
    fn visit_jmp(&mut self, it: &mut JumpStmt) { }
    fn visit_declaration(&mut self, it: &mut Declaration) {
        if let Some(ref mut expr) = it.initializer.as_mut() {
            self.visit_expr(expr);
        }

        let expected = it.ty.clone();
        let span = PosSpan::from_span(it.span);
        if let Some(ref mut expr) = it.initializer.as_mut() {
            let got = expr.typeof(self);
            if got.conforms_to(expected) {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::WrongType { got, expected, },
                        span,
                    }
                );
            }
        }
    }

    fn visit_index(&mut self, it: &mut Index) {
        self.visit_expr(&mut it.base);
        self.visit_expr(&mut it.offset);

        let base_ty = it.base.typeof(self);
        let index_ty = it.offset.typeof(self);

        if base_ty.ptr == 0 {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::CannotDeref { got: base_ty.clone() },
                    span: it.base.span,
                }
            );
        } else if base_ty.kind == TyKind::I0 && base_ty.ptr == 1 {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::CannotDerefToVoid,
                    span: it.base.span,
                }
            );
        }

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
    }
    fn visit_dot(&mut self, it: &mut Dot) {
        self.visit_expr(&mut it.lhs);

        let lhs_ty = it.lhs.typeof(self);
        if !lhs_ty.has_field(it.field_name, self) {
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::NoSuchField { got: lhs_ty, field: it.field_name },
                    span: it.lhs.span.clone()
                }
            );
        }
    }
    fn visit_deref(&mut self, it: &mut Expr) {
        self.visit_expr(&mut it.expr);

        let ty: Ty = it.lhs.typeof(self);
        match (ty.ptr, ty.kind) {
            (0, _) =>
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::CannotDeref { got: ty },
                        span: it.span.clone(),
                    }
                ),
            (1, TyKind::I0) =>
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::CannotDerefToVoid,
                        span: it.span.clone(),
                    }
                ),
            _ => {},
        };
    }
    fn visit_call(&mut self, it: &mut Call) {
        let mut args = vec![];
        for arg in it.args.iter_mut() {
            self.visit_expr(arg);
            args.push(arg.typeof(self));
        }

        let mut min_confirmity = u64::MAX;
        let mut conforming_indices = Vec::with_capacity(0);

        if let Some(ref fns) = self.fns.get(&it.fn_name).clone() {
            for i in 0..fns.len() {
                let conformity = fns[i].conforms_to(&args[..], &*self);
                if conformity > 0 {
                    if conformity < min_confirmity {
                        conforming_indices.clear();
                        min_conformity = confirmity;
                    }
                    conforming_indices.push(i);
                }
            }
        } else {
            // fn not found
            self.errs.push(
                TypeError {
                    err: TypeErrorKind::NoSuchFunction { fn_name: it.fn_name },
                    span: it.span.clone(),
                }
            );
            return
        }

        match conformities.len().cmp(&1) {
            // fn was found but the arguments were a mismatch
            Ordering::Less => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::BadFunctionArgs,
                        span: it.span.clone(),
                    }
                );
                return
            },
            // fn found but there are two or more matches - cant choose which one to use.
            Ordering::Greater => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::AmbiguousArguments { 
                            fn_name: it.fn_name,
                            indices: conforming_indices
                        },
                        span: it.span.clone(),
                    }
                );
                return
            },
            Ordering::Equal => { },
        }
    }
    fn visit_method_call(&mut self, it: &mut MethodCall) {
        self.visit_expr(&mut it.lhs);
        let mut ty = it.lhs.typeof(self);

        let mut args = vec![ty.clone().ptr()];
        for arg in it.args.iter_mut() {
            self.visit_expr(arg);
            args.push(arg.typeof(self));
        }

        let struct_name = match ty.kind {
            TyKind::Struct(id) => id,
            _ @ ty => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::MethodCallOnPrimitive { ty },
                        span: it.lhs.span.clone(),
                    }
                );
                return;
            }
        };

        let mut min_confirmity = u64::MAX;
        let mut conforming_indices = vec![];
        let mut cur_struct = Some(struct_name);
        while let Some(name) = cur_struct.clone() {
            if let Some(ref struct) = self.structs.get_mut(&name).clone() {
                let struct_name = struct.name;
                if let Some(ref methods) = struct.methods.get(&it.method_name).clone() {
                    for i in 0..methods.len() {
                        let conformity = methods[i].conforms_to(&args[..], &*self);
                        if conformity > 0 {
                            if conformity < min_confirmity {
                                conforming_indices.clear();
                                min_conformity = confirmity;
                            }
                            conforming_indices.push((struct_name, i));
                        }
                    }
                }
                cur_struct = struct.parent.clone();
            }
        }

        match conforming_indices.len().cmp(&0) {
            Ordering::Less => {
                self.errs.push(
                    TypeError {
                        err: TypeErrorKind::NoSuchMethod {
                            got: Ty::new(TyKind::Struct(struct_name)), 
                            method: it.method_name,
                        },
                        span: it.lhs.span.clone(),
                    }
                );
                return
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
                return
            },
            Ordering::Equal => { },
        }
    }
    fn visit_sizeofexpr(&mut self, it: &mut Ty) {

    }
    fn visit_mulexpr(&mut self, it: &mut MulExpr) {
        
    }
    fn visit_addexpr(&mut self, it: &mut AddExpr) {
        
    }
    fn visit_cmpexpr(&mut self, it: &mut CmpExpr) {
        
    }
    fn visit_eqexpr(&mut self, it: &mut EqExpr) {
        
    }
    fn visit_inverseexpr(&mut self, it: &mut Expr) {
        
    }
    fn visit_notexpr(&mut self, it: &mut Expr) {
        
    }
    fn visit_assignexpr(&mut self, it: &mut AssignExpr) {
        
    }
    fn visit_leaexpr(&mut self, it: &mut Expr) {
        
    }
    fn visit_cast(&mut self, it: &mut Cast) {
        
    }
    fn visit_ident(&mut self, it: &mut Id) {
        
    }
    fn visit_number(&mut self, it: &mut i64) {
        
    }

    fn visit_function(&mut self, it: &mut Function) {
        
    }
    fn visit_structure(&mut self, it: &mut Structure) {
        
    }
    fn visit_ty(&mut self, it: &mut Ty) {
        
    }
    fn visit_id(&mut self, it: &mut Id) {
        
    }
    fn visit_declaration(&mut self, it: &mut Declaration) {
        
    }
}