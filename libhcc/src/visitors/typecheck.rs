use ast::declaration::*;
use ast::expr::*;
use ast::function::*;
use ast::id::*;
use ast::statement::*;
use ast::structure::*;
use ast::ty::*;
use ast::*;

use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use std::u64;

pub enum TypeErrorKind {
    ExpectedIntegral {
        got: Ty,
    },
    WrongType {
        got: Ty,
        expected: Ty,
    },
    NoSuchField {
        got: Ty,
        field: Id,
    },
    NoSuchFunction {
        fn_name: Id,
    },
    NoSuchMethod {
        got: Ty,
        method_name: Id,
    },
    CannotDeref {
        got: Ty,
    },
    CannotDerefToVoid,
    InvalidIndex {
        got: Ty,
    },
    FunctionNotFound {
        fn_name: Id,
    },
    BadFunctionArgs {
        fn_name: Id,
    },
    AmbiguousFunctionArguments {
        fn_name: Id,
        indices: Vec<usize>,
    },
    AmbiguousMethodArguments {
        method_name: Id,
        ty: Ty,
        indices: Vec<(Id, usize)>,
    },
    MethodCallOnPrimitive {
        ty: Ty,
    },
    NoSuchStruct {
        struct_name: Id,
    },
    IllegalBinaryOperation {
        op: &'static str,
        got: Ty,
        other_ty: Ty,
    },
    IllegalUnaryOperation {
        op: &'static str,
        got: Ty,
    },
    IdNotFound {
        name: Id,
    },
    InvalidCast {
        from: Ty,
        to: Ty,
    },
    NoFunctionDefinition,
    ParentFieldCollision {
        field_name: Id,
        parent_span: PosSpan,
    },
    DuplicateName {
        other: PosSpan,
        field_name: Id,
    },
    DuplicateMethodDefinitions {
        other_span: PosSpan,
        ty: Ty,
        method_name: Id,
    },
    CircularInheritence {
        with: Ty,
    },
    CannotOverloadReturnType {
        super_ty: Ty,
        supplied_ty: Ty,
    },
    DuplicateFieldDefinition {
        parent_def_span: PosSpan,
        ty: Ty,
        parent_ty: Ty,
        field_name: Id,
    },
    DuplicateStructDefinitions {
        other_span: PosSpan,
        name: Id,
    },
    IncompleteType {
        field_name: Id,
    },
    NewZeroSizeType,
}

pub struct TypeError {
    pub err: TypeErrorKind,
    pub span: PosSpan,
    pub ty: Ty,
}

use ast::id::IdStore;
use parser::LineInfo;
use pest::Span;

impl TypeError {
    pub fn to_string(
        &self,
        lines: &[String],
        idstore: &IdStore,
        lineinfo: &LineInfo,
        program: &str,
    ) -> String {
        match &self.err {
            TypeErrorKind::ExpectedIntegral { got, }
                => format!("expected integral type, instead found '{}'", got.to_string(idstore)),
            TypeErrorKind::WrongType { got, expected, }
                => format!("expected type '{}', instead found '{}'", expected.to_string(idstore), got.to_string(idstore)),
            TypeErrorKind::NoSuchField { got, field, }
                => format!("type '{}' has no field named '{}'", got.to_string(idstore), idstore.get_string(*field).unwrap()),
            TypeErrorKind::NoSuchFunction { fn_name, }
                => format!("no function with the name '{}'", idstore.get_string(*fn_name).unwrap()),
            TypeErrorKind::NoSuchMethod { got, method_name, }
                => format!("type '{}' has no method named '{}'", got.to_string(idstore), idstore.get_string(*method_name).unwrap()),
            TypeErrorKind::CannotDeref { got, }
                => format!("cannot dereference type '{}'", got.to_string(idstore)),
            TypeErrorKind::CannotDerefToVoid
                => "cannot dereference type 'i0'".to_string(),
            TypeErrorKind::InvalidIndex { got, }
                => format!("type '{}' is a valid index type", got.to_string(idstore)),
            TypeErrorKind::FunctionNotFound { fn_name, }
                => format!("function '{}' not found.", idstore.get_string(*fn_name).unwrap()),
            TypeErrorKind::BadFunctionArgs { fn_name }
                => format!("no definition of '{}' takes the supplied arguments.", idstore.get_string(*fn_name).unwrap()),
            TypeErrorKind::AmbiguousFunctionArguments { fn_name, indices: _indices, }
                => format!("arguments are ambiguous for function '{}'", idstore.get_string(*fn_name).unwrap()),
            TypeErrorKind::AmbiguousMethodArguments { method_name, ty, indices: _indices }
                => format!("arguments are ambiguous for method '{}::{}'", ty.to_string(idstore), idstore.get_string(*method_name).unwrap()),
            TypeErrorKind::MethodCallOnPrimitive { ty, }
                => format!("cannot call methods on primitive type '{}'", ty.to_string(idstore)),
            TypeErrorKind::NoSuchStruct { struct_name, }
                => format!("there is no struct with the name '{}'", idstore.get_string(*struct_name).unwrap()),
            TypeErrorKind::IllegalBinaryOperation { op, got, other_ty, } => {
                format!("binary operation '{}' cannot be performed on type '{}'\nleft hand side type '{}' is incompatible", 
                op, got.to_string(idstore), other_ty.to_string(idstore))
            },
            TypeErrorKind::IllegalUnaryOperation { op, got, }
                => format!("unary operator '{}' cannot be performed on type '{}'", op, got.to_string(idstore)),
            TypeErrorKind::IdNotFound { name, }
                => format!("variable with the name '{}' was not found", idstore.get_string(*name).unwrap()),
            TypeErrorKind::InvalidCast { from, to, }
                => format!("cannot cast type '{}' to '{}'", from.to_string(idstore), to.to_string(idstore)),
            TypeErrorKind::NoFunctionDefinition
                => format!("function body is never defined"),
            TypeErrorKind::CircularInheritence { with }
                => format!("circular inheritence with parent class '{}'", with.to_string(idstore)),
            TypeErrorKind::ParentFieldCollision { field_name, parent_span } => {
                let (line, col) = Span::new(program, parent_span.start, parent_span.end).unwrap().start_pos().line_col();
                let (filename, linestart) = lineinfo.get_file_and_line_start(line).unwrap();
                let line_n_str = (line - linestart).to_string();
                let indent = " ".repeat(line_n_str.len() + 2);
                format!("both parent struct and define a field with the name '{}'\nfield first defined here\n{}---> {}:{}:{}\n{}|\n {} |  {}\n{}|", 
                    idstore.get_string(*field_name).unwrap(), indent, filename, line_n_str, col, indent, line_n_str, lines[line - 1], indent)
            },
            TypeErrorKind::DuplicateName { other, field_name } => {
                let (line, col) = Span::new(program, other.start, other.end).unwrap().start_pos().line_col();
                let (filename, linestart) = lineinfo.get_file_and_line_start(line).unwrap();
                let line_n_str = (line - linestart).to_string();
                let indent = " ".repeat(line_n_str.len() + 2);
                format!("local variable with name '{}' defined twice\nfield first defined here\n{}---> {}:{}:{}\n{}|\n {} |  {}\n{}|", 
                    idstore.get_string(*field_name).unwrap(), indent, filename, line_n_str, col, indent, line_n_str, lines[line - 1], indent)
            },
            TypeErrorKind::DuplicateMethodDefinitions { other_span, ty, method_name } => {
                let (line, col) = Span::new(program, other_span.start, other_span.end).unwrap().start_pos().line_col();
                let (filename, linestart) = lineinfo.get_file_and_line_start(line).unwrap();
                let line_n_str = (line - linestart).to_string();
                let indent = " ".repeat(line_n_str.len() + 2);
                format!("method '{}::{}(..)' is defined multiple times\nmethod first defined in parent struct here: \n{}---> {}:{}:{}\n{}|\n {} |  {}\n{}|", 
                    ty.to_string(idstore), idstore.get_string(*method_name).unwrap(), indent, filename, line_n_str, col, indent, line_n_str, lines[line - 1], indent)
            },
            TypeErrorKind::CannotOverloadReturnType { super_ty, supplied_ty } =>
                format!("cannot overload return type (type '{}' does not inherit '{}')",
                supplied_ty.to_string(idstore), super_ty.to_string(idstore)),
            TypeErrorKind::DuplicateFieldDefinition { parent_def_span, ty, parent_ty, field_name } => {
                let (line, col) = Span::new(program, parent_def_span.start, parent_def_span.end).unwrap().start_pos().line_col();
                let (filename, linestart) = lineinfo.get_file_and_line_start(line).unwrap();
                let line_n_str = (line - linestart).to_string();
                let indent = " ".repeat(line_n_str.len() + 2);
                format!("field '{}::{}' is defined multiple times\nfield first defined in parent '{}' here: \n{}---> {}:{}:{}\n{}|\n {} |  {}\n{}|", 
                    ty.to_string(idstore), idstore.get_string(*field_name).unwrap(), parent_ty.to_string(idstore), indent, filename, line_n_str, col, indent, line_n_str, lines[line - 1], indent)
            },
            TypeErrorKind::DuplicateStructDefinitions { other_span, name } => {
                let (line, col) = Span::new(program, other_span.start, other_span.end).unwrap().start_pos().line_col();
                let (filename, linestart) = lineinfo.get_file_and_line_start(line).unwrap();
                let line_n_str = (line - linestart).to_string();
                let indent = " ".repeat(line_n_str.len() + 2);
                format!("struct '{}' is defined multiple times\n struct first defined here: \n{}---> {}:{}:{}\n{}|\n {} |  {}\n{}|", 
                    idstore.get_string(*name).unwrap(), indent, filename, line_n_str, col, indent, line_n_str, lines[line - 1], indent)
            },
            TypeErrorKind::IncompleteType { field_name } => {
                format!("field '{}' has an incomplete type\n",
                    idstore.get_string(*field_name).unwrap())
            },
            TypeErrorKind::NewZeroSizeType => {
                format!("cannot allocate a zero sized type")
            }
        }
    }
}

pub enum TypeWarningKind {
    ImplicitCast { from: Ty, to: Ty },
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
    pub expr_span: PosSpan,
    pub this: Id,
    pub ids: IdStore,
}

impl TypeChecker {
    pub fn typecheck<'a>(mut ast: Ast) -> (Result<Ast, (Vec<TypeError>, IdStore)>, Vec<TypeWarning>) {
        let this = ast.idstore.get_id("this");
        let main = ast.idstore.get_id("main");
        let fns = ast
            .functions
            .clone()
            .into_iter()
            .map(|(_, v)| v)
            .collect::<Vec<_>>();
        let structs = ast.structs.clone();
        let mut tc = Self::new(ast.structs, ast.functions, this, ast.idstore);

        for fnlist in fns.into_iter() {
            for f in fnlist.into_iter() {
                let mutref = Rc::into_raw(f) as *mut Function;

                tc.visit_function(unsafe { mutref.as_mut().unwrap() });

                let _rc = unsafe { Rc::from_raw(mutref as *const Function) };
            }
        }

        for (_, st) in structs.into_iter() {
            let mutref = Rc::into_raw(st) as *mut Structure;

            tc.visit_structure(unsafe { mutref.as_mut().unwrap() });

            let _rc = unsafe { Rc::from_raw(mutref as *const Structure) };
        }

        if tc.errs.len() == 0 {
            let mut ast = Ast {
                idstore: tc.ids,
                structs: tc.structs,
                functions: tc.fns,
            };
            if let Some(ref mut a) = ast.functions.get_mut(&main) {
                Rc::get_mut(&mut a[0]).unwrap().intrinsic = true;
            }
            (Ok(ast), tc.wrns)
        } else {
            (Err((tc.errs, tc.ids)), tc.wrns)
        }
    }

    fn new(
        structs: HashMap<Id, Rc<Structure>>,
        fns: HashMap<Id, Vec<Rc<Function>>>,
        this: Id,
        ids: IdStore
    ) -> Self {
        TypeChecker {
            structs,
            this,
            fns,
            errs: vec![],
            wrns: vec![],
            ids,
            var_stack: VecDeque::with_capacity(8),
            numeric_type_hint: vec![Ty::new(TyKind::I64)],
            expr_span: PosSpan { start: 0, end: 0 },
        }
    }

    fn add_to_varstack(&mut self, name: Id, ty: Ty, span: PosSpan) -> bool {
        for vmap in self.var_stack.iter() {
            if let Some((otherty, otherspan)) = vmap.get(&name) {
                self.errs.push(TypeError {
                    err: TypeErrorKind::DuplicateName {
                        field_name: name,
                        other: otherspan.clone(),
                    },
                    span: span.clone(),
                    ty: ty.clone()
                });
            }
        }
        self.var_stack[0].insert(name, (ty.clone(), span.clone())).is_none()
    }

    fn binary_expr_visit<T>(&mut self, head: &mut Expr, tail: &mut [(T, Expr)]) -> Ty
    where
        T: Into<&'static str> + Copy + Clone,
    {
        let mut errs = false;
        let mut tys = vec![self.visit_expr(head)];
        self.numeric_type_hint.push(tys[0].clone());
        for (op, exp) in tail.iter_mut() {
            let ty = self.visit_expr(exp);
            match (ty.kind.clone(), ty.ptr == 0) {
                (TyKind::Struct(_), true) | (TyKind::I0, true) => {
                    errs = true;
                    self.errs.push(TypeError {
                        err: TypeErrorKind::IllegalBinaryOperation {
                            op: op.clone().into(),
                            got: ty.clone(),
                            other_ty: tys[tys.len() - 1].clone(),
                        },
                        span: exp.span.clone(),
                        ty: ty.clone(),
                    });
                    tys.push(Ty::error());
                }
                (_, true) => {
                    self.numeric_type_hint.push(ty.clone());
                    tys.push(ty.clone());
                },
                _ => {
                    tys.push(ty.clone());
                }
            }
        }

        for _ in 0..tail.len() + 1 {
            self.numeric_type_hint.pop();
        }

        if errs {
            return Ty::error();
        }

        let mut largest_type = tys[0].clone();
        for ty in tys[1..].iter() {
            if ty.ptr > 0 {
                largest_type = ty.clone();
                continue;
            }
            match ty.kind.clone() {
                TyKind::Struct(_) | TyKind::I0 => unreachable!(),
                TyKind::I64 => {
                    largest_type = ty.clone();
                }
                _ => {}
            }
        }

        largest_type
    }

    pub fn visit_stmt(&mut self, it: &mut Statement) -> Ty {
        match it {
            Statement::Body(ref mut body) => self.visit_body(body.as_mut()),
            Statement::If(ref mut ifstmt) => self.visit_if(ifstmt.as_mut()),
            Statement::While(ref mut whilestmt) => self.visit_while(whilestmt.as_mut()),
            Statement::Expr(ref mut expr) => self.visit_expr(expr),
            Statement::Jump(ref mut jmpstmt) => self.visit_jmp(jmpstmt),
            Statement::Declaration(ref mut decl) => self.visit_declaration(decl),
        }    
    }

    pub fn visit_while(&mut self, it: &mut WhileStmt) -> Ty {
        self.visit_stmt(&mut it.body);
        self.visit_expr(&mut it.cond);
        Ty::new(TyKind::I0)
    }
    pub fn visit_body(&mut self, it: &mut Body) -> Ty {
        self.var_stack.push_front(HashMap::new());
        for statement in it.stmts.iter_mut() {
            self.visit_stmt(statement);
            if let Statement::Declaration(ref mut dec) = statement {
                self.add_to_varstack(dec.name, dec.ty.clone(), dec.span.clone());
            }
        }
        self.var_stack.pop_front();
        Ty::new(TyKind::I0)
    }
    pub fn visit_if(&mut self, it: &mut IfStmt) -> Ty {
        let cond_ty = self.visit_expr(&mut it.cond);
        self.visit_stmt(&mut it.true_body);
        if let Some(ref mut false_body) = it.false_body.as_mut() {
            self.visit_stmt(false_body);
        }

        if !cond_ty.is_integral_type() && cond_ty.ptr == 0 {
            self.errs.push(TypeError {
                err: TypeErrorKind::ExpectedIntegral {
                    got: cond_ty.clone(),
                },
                span: it.cond.span,
                ty: cond_ty.clone(),
            });
        }
        Ty::new(TyKind::I0)
    }
    pub fn visit_jmp(&mut self, it: &mut JumpStmt) -> Ty {
        match it {
            JumpStmt::Break | JumpStmt::Continue | JumpStmt::Return((None, _)) => {
                Ty::new(TyKind::I0)
            }
            JumpStmt::Return((Some(ref mut expr), _span)) => self.visit_expr(expr),
        }
    }
    pub fn visit_declaration(&mut self, it: &mut Declaration) -> Ty {
        let expected = it.ty.clone();
        match &expected.kind {
            x @ TyKind::I64 | x @ TyKind::I8 => self.numeric_type_hint.push(Ty::new(x.clone())),
            TyKind::Struct(name) => {
                if let Some(s) = self.structs.get(&name) {
                    if s.empty {
                        self.errs.push(TypeError {
                            err: TypeErrorKind::IncompleteType {
                                field_name: it.name,
                            },
                            span: it.span.clone(),
                            ty: it.ty.clone(),
                        });
                    }
                } else {
                    self.errs.push(TypeError {
                        err: TypeErrorKind::NoSuchStruct { struct_name: *name },
                        span: it.span.clone(),
                        ty: it.ty.clone(),
                    });
                }
                self.numeric_type_hint.push(Ty::new(TyKind::I64))
            }
            _ => self.numeric_type_hint.push(Ty::new(TyKind::I64)),
        };
        let span = it.span.clone();
        let mut cast = None;
        let res = if let Some(ref mut expr) = it.initializer.as_mut() {
            let got = self.visit_expr(expr);
            match expected.compatibility_with(&got) {
                TypeCompatibility::Ok => expected.clone(),
                TypeCompatibility::None => {
                    self.errs.push(TypeError {
                        err: TypeErrorKind::WrongType {
                            got: got.clone(),
                            expected,
                        },
                        span,
                        ty: got.clone(),
                    });
                    Ty::new(TyKind::Error)
                }
                TypeCompatibility::CastTo(to) => {
                    // self.wrns.push(TypeWarning {
                    // wrn: TypeWarningKind::ImplicitCast { from: got.clone(), to: to.clone() },
                    // span,
                    // });
                    cast = Some(to);
                    expected.clone()
                }
            }
        } else {
            Ty::new(TyKind::I0)
        };
        if it.initializer.is_some() && cast.is_some() {
            let to = cast.unwrap();
            it.initializer = Some(Expr {
                expr: ExprKind::Cast(box Cast {
                    to: to.clone(),
                    expr: it.initializer.take().unwrap(),
                }),
                span,
                ty: Some(to),
            });
        }
        self.numeric_type_hint.pop();
        res
    }

    pub fn visit_expr(&mut self, it: &mut Expr) -> Ty {
        let span = it.span.clone();
        self.expr_span = span.clone();
        let ty = match &mut it.expr {
            ExprKind::Index(ref mut index) => self.visit_index(index),
            ExprKind::Dot(ref mut dot) => self.visit_dot(dot),
            ExprKind::Deref(ref mut deref) => self.visit_deref(deref),
            ExprKind::Call(ref mut call) => self.visit_call(call),
            ExprKind::MethodCall(ref mut mcall) => self.visit_method_call(mcall),
            ExprKind::SizeOfExpr(ref mut ty) => self.visit_sizeofexpr(ty, span),
            ExprKind::MulExpr(ref mut mulexpr) => self.visit_mulexpr(mulexpr),
            ExprKind::AddExpr(ref mut addexpr) => self.visit_addexpr(addexpr),
            ExprKind::CmpExpr(ref mut cmpexpr) => self.visit_cmpexpr(cmpexpr),
            ExprKind::EqExpr(ref mut eqexpr) => self.visit_eqexpr(eqexpr),
            // Bitwise negation
            ExprKind::InverseExpr(ref mut iexpr) => self.visit_inverseexpr(iexpr),
            // boolean negation
            ExprKind::NotExpr(ref mut expr) => self.visit_notexpr(expr),
            ExprKind::AssignExpr(ref mut assignexpr) => self.visit_assignexpr(assignexpr),
            ExprKind::LeaExpr(ref mut expr) => self.visit_leaexpr(expr),
            ExprKind::Cast(ref mut cast) => self.visit_cast(cast),
            ExprKind::Ident(ref mut id) => self.visit_ident(id),
            ExprKind::Number(ref mut n) => self.visit_number(n),
            ExprKind::New(ref mut ty) => self.visit_new(ty),
            ExprKind::NoOp => Ty::new(TyKind::I0),
        };
        it.ty = Some(ty.clone());
        ty
    }

    pub fn visit_index(&mut self, it: &mut Index) -> Ty {
        let base_ty = self.visit_expr(&mut it.base);
        let index_ty = self.visit_expr(&mut it.offset);

        match index_ty.kind.clone() {
            TyKind::I64 | TyKind::I8 => {}
            _ => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::InvalidIndex {
                        got: index_ty.clone(),
                    },
                    span: it.offset.span.clone(),
                    ty: index_ty.clone(),
                });
            }
        };

        if base_ty.ptr == 0 && base_ty.kind != TyKind::Error {
            self.errs.push(TypeError {
                err: TypeErrorKind::CannotDeref {
                    got: base_ty.clone(),
                },
                span: it.base.span,
                ty: base_ty.clone(),
            });
            Ty::error()
        } else if base_ty.kind == TyKind::I0 && base_ty.ptr == 1 {
            self.errs.push(TypeError {
                err: TypeErrorKind::CannotDerefToVoid,
                span: it.base.span,
                ty: base_ty.clone(),
            });
            Ty::error()
        } else {
            base_ty.derefed()
        }
    }
    pub fn visit_dot(&mut self, it: &mut Dot) -> Ty {
        let lhs_ty = self.visit_expr(&mut it.lhs);
        let lhs_ty = if it.deref { lhs_ty.derefed() } else { lhs_ty };
        if !lhs_ty.has_field(it.field_name, self) {
            self.errs.push(TypeError {
                err: TypeErrorKind::NoSuchField {
                    got: lhs_ty.clone(),
                    field: it.field_name,
                },
                span: it.lhs.span.clone(),
                ty: lhs_ty.clone(),
            });
            return Ty::error()
        }
        match lhs_ty.kind {
            TyKind::Struct(id) => {
                let mut curstruct = Some(id);
                while let Some(st) = curstruct.take() {
                    if let Some(st) = self.structs.get(&st) {
                        if let Some(struct_field) = st.fields.get(&it.field_name) {
                            return struct_field.ty.clone()
                        }
                        curstruct = st.parent.clone();
                    }
                }
                Ty::error()
            }
            _ => Ty::error(),
        }
    }
    pub fn visit_deref(&mut self, it: &mut Expr) -> Ty {
        let ty: Ty = self.visit_expr(it);
        if ty.kind == TyKind::Error {
            return Ty::error();
        }
        match (ty.ptr, ty.kind.clone()) {
            (0, _t) => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::CannotDeref { got: ty.clone() },
                    span: it.span.clone(),
                    ty: ty.clone(),
                });
                Ty::error()
            }
            (1, TyKind::I0) => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::CannotDerefToVoid,
                    span: it.span.clone(),
                    ty: ty.clone(),
                });
                Ty::error()
            }
            _ => {
                let r = ty.derefed();
                r
            },
        }
    }
    pub fn visit_call(&mut self, it: &mut Call) -> Ty {
        let mut args = vec![];
        for arg in it.args.iter_mut() {
            let t = self.visit_expr(arg);
            args.push(t);
        }

        let mut min_conformity = u64::MAX;
        let mut conforming_indices = Vec::with_capacity(0);

        if let Some(ref fns) = self.fns.get(&it.fn_name).map(|r| (*r).clone()) {
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
                    self.errs.push(TypeError {
                        err: TypeErrorKind::BadFunctionArgs {
                            fn_name: it.fn_name,
                        },
                        span: it.span.clone(),
                        ty: Ty::new(TyKind::I0),
                    });
                    Ty::error()
                }
                // fn found but there are two or more matches - cant choose which one to use.
                Ordering::Greater => {
                    self.errs.push(TypeError {
                        err: TypeErrorKind::AmbiguousFunctionArguments {
                            fn_name: it.fn_name,
                            indices: conforming_indices,
                        },
                        span: it.span.clone(),
                        ty: Ty::new(TyKind::I0),
                    });
                    Ty::error()
                }
                Ordering::Equal => {
                    it.f = Some(conforming_indices[0]);
                    fns[conforming_indices[0]].return_type.clone()
                }
            }
        } else {
            // fn not found
            self.errs.push(TypeError {
                err: TypeErrorKind::NoSuchFunction {
                    fn_name: it.fn_name,
                },
                span: it.span.clone(),
                ty: Ty::new(TyKind::I0),
            });
            Ty::error()
        }
    }
    pub fn visit_method_call(&mut self, it: &mut MethodCall) -> Ty {
        let lhs_ty = self.visit_expr(&mut it.lhs);

        let mut args = vec![];
        for arg in it.args.iter_mut() {
            args.push(self.visit_expr(arg));
        }

        if lhs_ty.ptr != 1 {
            self.errs.push(TypeError {
                err: TypeErrorKind::MethodCallOnPrimitive { ty: lhs_ty.clone() },
                span: it.lhs.span.clone(),
                ty: lhs_ty.clone(),
            });
            return Ty::error();
        }

        let struct_name = match lhs_ty.kind.clone() {
            TyKind::Struct(id) => id,
            _ => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::MethodCallOnPrimitive { ty: lhs_ty.clone() },
                    span: it.lhs.span.clone(),
                    ty: lhs_ty.clone(),
                });
                return Ty::error();
            }
        };

        let mut depth = 0;
        let mut min_conformity = u64::MAX;
        let mut conforming_struct = self
            .structs
            .get(&struct_name)
            .map(|r| (*r).clone())
            .unwrap();
        let mut conforming_indices = vec![];
        let mut cur_struct = Some(struct_name);
        while let Some(name) = cur_struct.clone() {
            if let Some(ref st) = self.structs.get_mut(&name).map(|t| t.clone()) {
                let struct_name = st.name;
                if let Some(ref methods) = st.methods.get(&it.method_name).map(|r| r.clone()) {
                    for i in 0..methods.len() {
                        let conformity =
                            methods.get(i).unwrap().conforms_to(&mut it.args[..], self) + depth;
                        if conformity > 0 {
                            if conformity < min_conformity {
                                conforming_indices.clear();
                                min_conformity = conformity;
                                conforming_struct = self
                                    .structs
                                    .get(&struct_name)
                                    .map(|r| (*r).clone())
                                    .unwrap();
                            }
                            conforming_indices.push((it.method_name, i));
                        }
                    }
                }
                cur_struct = st.parent.clone();
            }
            depth += 1;
        }

        match conforming_indices.len().cmp(&1) {
            Ordering::Less => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::NoSuchMethod {
                        got: Ty::new(TyKind::Struct(struct_name)),
                        method_name: it.method_name,
                    },
                    span: it.lhs.span.clone(),
                    ty: lhs_ty.clone(),
                });
                Ty::error()
            }
            Ordering::Greater => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::AmbiguousMethodArguments {
                        method_name: it.method_name,
                        indices: conforming_indices,
                        ty: lhs_ty.clone(),
                    },
                    span: it.lhs.span.clone(),
                    ty: lhs_ty,
                });
                Ty::error()
            }
            Ordering::Equal => {
                let (method_name, index) = conforming_indices[0].clone();
                it.f = Some((conforming_struct.name, index));
                conforming_struct.methods[&method_name][index]
                    .return_type
                    .clone()
            }
        }
    }
    pub fn visit_sizeofexpr(&mut self, it: &mut Ty, span: PosSpan) -> Ty {
        match it.kind.clone() {
            TyKind::Struct(name) => {
                if let Some(_) = self.structs.get(&name) {
                    Ty::new(TyKind::I64)
                } else {
                    self.errs.push(TypeError {
                        err: TypeErrorKind::NoSuchStruct { struct_name: name },
                        span: span,
                        ty: it.clone(),
                    });
                    Ty::error()
                }
            }
            _ => Ty::new(TyKind::I64),
        }
    }
    pub fn visit_mulexpr(&mut self, it: &mut MulExpr) -> Ty {
        self.binary_expr_visit(&mut it.head, &mut it.tail[..])
    }
    pub fn visit_addexpr(&mut self, it: &mut AddExpr) -> Ty {
        self.binary_expr_visit(&mut it.head, &mut it.tail[..])
    }
    pub fn visit_cmpexpr(&mut self, it: &mut CmpExpr) -> Ty {
        self.binary_expr_visit(&mut it.head, &mut it.tail[..]);
        Ty::new(TyKind::I8)
    }
    pub fn visit_eqexpr(&mut self, it: &mut EqExpr) -> Ty {
        self.binary_expr_visit(&mut it.head, &mut it.tail[..]);
        Ty::new(TyKind::I8)
    }
    pub fn visit_inverseexpr(&mut self, it: &mut Expr) -> Ty {
        let ty = self.visit_expr(it);
        match (ty.kind.clone(), ty.ptr == 0) {
            (TyKind::I0, true) | (TyKind::Struct(_), true) => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::IllegalUnaryOperation {
                        op: "~",
                        got: ty.clone(),
                    },
                    span: it.span.clone(),
                    ty: ty.clone(),
                });
                Ty::error()
            }
            _ => ty,
        }
    }
    pub fn visit_notexpr(&mut self, it: &mut Expr) -> Ty {
        let ty = self.visit_expr(it);
        match (ty.kind.clone(), ty.ptr == 0) {
            (TyKind::I64, true) | (TyKind::I8, true) => ty,
            _ => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::IllegalUnaryOperation {
                        op: "!",
                        got: ty.clone(),
                    },
                    span: it.span.clone(),
                    ty: ty.clone(),
                });
                Ty::error()
            }
        }
    }
    pub fn visit_assignexpr(&mut self, it: &mut AssignExpr) -> Ty {
        self.binary_expr_visit(&mut it.head, &mut it.tail[..])
    }
    pub fn visit_leaexpr(&mut self, it: &mut Expr) -> Ty {
        self.visit_expr(it).ptr_to()
    }
    pub fn visit_cast(&mut self, it: &mut Cast) -> Ty {
        let ty = self.visit_expr(&mut it.expr);
        let t2 = ty.clone();
        match it.to.compatibility_with(&ty) {
            TypeCompatibility::None => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::InvalidCast {
                        from: ty.clone(),
                        to: it.to.clone(),
                    },
                    span: self.expr_span.clone(),
                    ty: t2,
                });
                Ty::error()
            }
            _ => it.to.clone(),
        }
    }
    pub fn visit_ident(&mut self, it: &mut Id) -> Ty {
        for scope in self.var_stack.iter() {
            if let Some(t) = scope.get(it) {
                return t.0.clone();
            }
        }
        self.errs.push(TypeError {
            err: TypeErrorKind::IdNotFound { name: *it },
            span: self.expr_span.clone(),
            ty: Ty::new(TyKind::I0),
        });
        Ty::error()
    }

    pub fn visit_number(&mut self, _it: &mut i64) -> Ty {
        let mut a = self.numeric_type_hint.last().cloned().unwrap_or(Ty::new(TyKind::I64));
        a.ptr = 0;
        if a.kind == TyKind::Error {
            Ty::new(TyKind::I64)
        } else {
            a
        }
    }

    pub fn visit_new(&mut self, it: &mut Ty) -> Ty {
        match it.clone().kind {
            TyKind::I0 => {
                self.errs.push(TypeError {
                    err: TypeErrorKind::NewZeroSizeType,
                    span: self.expr_span.clone(),
                    ty: it.clone(),
                });
                Ty::error()
            }
            _ => it.clone(),
        }
    }

    pub fn visit_function(&mut self, it: &mut Function) -> Ty {
        if it.intrinsic || it.body.is_none() {
            return it.return_type.clone();
        }
        if it.body.is_none() {
            self.errs.push(TypeError {
                err: TypeErrorKind::NoFunctionDefinition,
                span: it.span.clone(),
                ty: Ty::new(TyKind::I0),
            });
            return Ty::error();
        }

        let mut vmap = HashMap::with_capacity(it.arg_order.len());
        if it.method.is_some() {
            vmap.insert(
                self.this,
                (
                    Ty::new(TyKind::Struct(it.method.clone().unwrap())).ptr_to(),
                    it.span.clone(),
                ),
            );
        }
        self.var_stack.push_front(vmap);
        for arg in it.arg_order.iter() {
            self.add_to_varstack(*arg, it.args[arg].ty.clone(), it.span.clone());
        }
        let body = it.body.as_mut().unwrap();
        for statement in body.stmts.iter_mut() {
            if let Statement::Declaration(ref mut dec) = statement {
                self.add_to_varstack(dec.name, dec.ty.clone(), dec.span.clone());
            } else if let Statement::Jump(ref mut jmp) = statement {
                match jmp {
                    JumpStmt::Break | JumpStmt::Continue => continue,
                    JumpStmt::Return((Some(ref mut expr), ref pos)) => {
                        let rt = self.visit_expr(expr);
                        if rt != it.return_type
                            && rt.compatibility_with(&it.return_type) == TypeCompatibility::None
                        {
                            self.errs.push(TypeError {
                                err: TypeErrorKind::WrongType {
                                    got: rt.clone(),
                                    expected: it.return_type.clone(),
                                },
                                span: pos.clone(),
                                ty: rt.clone(),
                            });
                        }
                    }
                    JumpStmt::Return((None, ref pos)) => {
                        if it.return_type.kind != TyKind::I0 && it.return_type.kind != TyKind::Error
                        {
                            self.errs.push(TypeError {
                                err: TypeErrorKind::WrongType {
                                    got: Ty::new(TyKind::I0),
                                    expected: it.return_type.clone(),
                                },
                                span: pos.clone(),
                                ty: it.return_type.clone(),
                            });
                        }
                    }
                }
            }
            self.visit_stmt(statement);
        }
        self.var_stack.pop_front();
        it.return_type.clone()
    }
    pub fn visit_structure(&mut self, it: &mut Structure) -> Ty {
        if Ty::new(TyKind::Struct(it.name)).has_circular_inheritence(self) {
            self.errs.push(TypeError {
                err: TypeErrorKind::CircularInheritence {
                    with: Ty::new(TyKind::Struct(it.parent.clone().unwrap())),
                },
                span: it.parent_span.clone().unwrap(),
                ty: Ty::new(TyKind::I0),
            });
            return Ty::error();
        }

        let name = it.name;
        let parent = it.parent.clone();
        for (method_name, methods) in it.methods.iter() {
            for i in 0..methods.len() {
                for j in 0..methods.len() {
                    if j == i {
                        continue;
                    }
                    if methods
                        .get(i)
                        .unwrap()
                        .header_equals(methods.get(j).unwrap())
                    {
                        self.errs.push(TypeError {
                            err: TypeErrorKind::DuplicateMethodDefinitions {
                                ty: Ty::new(TyKind::Struct(name)),
                                other_span: methods.get(j).unwrap().span.clone(),
                                method_name: *method_name,
                            },
                            span: methods.get(i).unwrap().span.clone(),
                            ty: Ty::new(TyKind::I0),
                        });
                    }
                }
            }
            if let Some(ref parent) = parent.as_ref() {
                let mut parent = self.structs[parent].clone();
                for method in methods {
                    if let Some(parent_methods) = parent.clone().methods.get(method_name) {
                        for pmethod in parent_methods {
                            if method.header_equals(pmethod) {
                                match method.return_type.kind.clone() {
                                    TyKind::I0 | TyKind::I64 | TyKind::I8 => {
                                        if method.return_type == pmethod.return_type {
                                            break;
                                        }
                                    }
                                    _ => {
                                        if method.return_type.inherits(&pmethod.return_type, self) {
                                            break;
                                        }
                                    }
                                }
                                self.errs.push(TypeError {
                                    err: TypeErrorKind::CannotOverloadReturnType {
                                        supplied_ty: method.return_type.clone(),
                                        super_ty: pmethod.return_type.clone(),
                                    },
                                    span: method.span.clone(),
                                    ty: Ty::new(TyKind::I0),
                                });
                                break;
                            }
                        }
                    }
                }
            }
        }

        let parent = it.parent.clone();
        for (field_name, field) in it.fields.iter() {
            if let TyKind::Struct(name) = field.ty.kind {
                if !self.structs.contains_key(&name) {
                    self.errs.push(TypeError {
                        err: TypeErrorKind::IncompleteType {
                            field_name: *field_name,
                        },
                        span: field.span.clone(),
                        ty: field.ty.clone(),
                    });
                    return Ty::error();
                }
            }
            if let Some(span) = Ty::new(TyKind::Struct(name)).super_has_field(*field_name, self) {
                self.errs.push(TypeError {
                    err: TypeErrorKind::DuplicateFieldDefinition {
                        parent_def_span: span,
                        ty: Ty::new(TyKind::Struct(name)),
                        parent_ty: Ty::new(TyKind::Struct(parent.clone().unwrap())),
                        field_name: *field_name,
                    },
                    span: field.span.clone(),
                    ty: Ty::new(TyKind::I0),
                });
            }
        }

        for (_name, methodlist) in it.methods.iter_mut() {
            for method in methodlist.iter_mut() {
                self.visit_function(method);
            }
        }

        for (name, struct_field) in it.fields.iter() {
            if let Some(span) = struct_field.ty.super_has_field(*name, self) {
                self.errs.push(TypeError {
                    err: TypeErrorKind::ParentFieldCollision {
                        field_name: *name,
                        parent_span: span,
                    },
                    span: struct_field.span.clone(),
                    ty: struct_field.ty.clone(),
                })
            }
        }

        Ty::new(TyKind::Struct(it.name))
    }

}
