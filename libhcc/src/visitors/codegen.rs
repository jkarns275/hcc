use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;
use ast::*;

use std::fmt::{Formatter, self, Display};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Copy, Clone)]
pub struct Label(usize);

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

impl Label {
    pub fn to_string(self) -> String {
        let mut r = "l".to_string();
        r.push_str(self.0.to_string().as_str());
        r
    }
}

pub struct CG<'a> {
    pub ids: IdStore<'a>,
    pub lbl_counter: usize,
    pub tmp_counter: usize,
    pub output: String,
    pub fns: HashMap<Id, Vec<Rc<Function>>>,
    pub structs: HashMap<Id, Rc<Structure>>,
    pub vtables: HashMap<Id, Vec<Rc<Structure>>>,
    pub varstacks: HashMap<Id, Vec<Id>>,
    pub parents: HashMap<Id, Rc<Structure>>,
    pub current_ty: Ty,
    pub roots: HashMap<Id, Id>,
}


impl<'a> CG<'a> {

    fn get_root_struct(&self, child: Id) -> Id {
        if let Some(key) = self.roots.get(&child) {
            *key
        } else {
            let mut par = Some(child);
            while let Some(parent) = par.clone() {
                if self.structs[parent].parent.is_none() {
                    break
                }
                par = self.structs[parent].parent.clone();
            }
            let parent = par.unwrap();
            self.roots.insert(child, parent);
            parent
        }
    }

    fn emit<S: Into<String>>(&mut self, string: S) { self.output.push_str(string.into().as_str()); }
    fn emit_label(&mut self, label: Label) { self.output.push_str(label.to_string()); }

    fn next_tmp(&mut self) -> Id {
        let next = self.tmp_counter;
        self.tmp_counter += 1;
        self.ids.get_id(format!("t{}", next).as_str())
    }

    fn next_label(&mut self) -> Label {
        let next = self.lbl_counter;
        self.lbl_counter += 1;
        Label(next)
    }

    pub fn visit_struct(&mut self, it: &mut Structure) {
        self.emit(format!("struct _{} {{\n", self.ids.get_string(it.name)).unwrap());

        if it.type_id != -1 {
            let mut par = it.parent.clone();
            let root_parent = self.get_root_struct(it.name);
            {
                let entry = self.vtables.entry(root_parent).or_insert_with(|| vec![]);
                let id = entry.len();
                entry.push(self.structs[&it.name].clone());
                it.type_id = id;
            }
            self.emit(format!("    i8 type_id[4]; // Runtime type information \n"));
        }
        
        for (field_name, field) in it.fields.iter() {
            self.emit(format!("    {} _{};\n", field.ty.to_code(&self.ids), self.ids.get_string(field_name).clone()));
        }

        let mut parent = it.parent.clone();
        while let Some(super_name) = parent.take() {
            let structure = self.structs[super_name].clone();
            self.parents.insert(it.name, structure.clone());
            let mut comment = true;
            for (field_name, field) in structure.fields.iter() {
                if comment {
                    comment = false;
                    self.emit(format!("    // Field from parent type 'struct {}'\n", self.ids.get_string(super_name).unwrap()));
                }
                self.emit(format!("    {} _{};\n", field.ty.to_code(&self.ids), self.ids.get_string(field_name).clone()));
            }
        }

        self.emit("};\n")
    }

    fn visit_stmt(&mut self, it: &mut Statement) -> Label {
        match it {
            Statement::Body(ref mut body)          => self.visit_body(body.as_mut()),
            Statement::If(ref mut ifstmt)          => self.visit_if(ifstmt.as_mut()),
            Statement::While(ref mut whilestmt)    => self.visit_while(whilestmt.as_mut()),
            Statement::Expr(ref mut expr)          => self.visit_expr(expr),
            Statement::Jump(ref mut jmpstmt)       => self.visit_jmp(jmpstmt),
            Statement::Declaration(ref mut decl)   => self.visit_declaration(decl),
        }
    }
    fn visit_while(&mut self, it: &mut WhileStmt) -> Label {
        let loop_start = self.next_label();
        self.emit_label(loop_start);
        let loop_end = self.next_label();
        let cond = self.visit_expr(&mut it.cond);
        self.emit(format!("    if ( {} ) goto {};\n", self.ids.get_str(cond), loop_end));
        self.visit_stmt(&mut it.body);
        self.emit(format!("    goto {};\n", loop_start));
        self.emit_label(loop_end);
        loop_start
    }
    fn visit_body(&mut self, it: &mut Body) -> Label {
        let body_start = self.next_label();
        for stmt in it.stmts.iter_mut() {
            self.visit_stmt(it);
        }
        body_start
    }
    fn visit_if(&mut self, it: &mut IfStmt) -> Label {
        let start = self.next_label();
        self.emit_label(start);
        if it.false_body.is_some() {
            let true_body = self.next_label();
            let false_body = self.next_label();
            let end = self.next_label();
            let goto_end = format!("    goto {}\n;", end);
            let cond = self.visit_expr(&mut it.cond);
            self.emit(format!("    if ( {} ) goto {};\n    else goto {};\n", self.ids.get_str(cond), true_body, false_body));
            self.emit_label(true_body);
            self.visit_stmt(&mut it.true_body);
            self.emit(&goto_end[..]);
            self.emit_label(false_body);
            self.visit_stmt(it.false_body.as_mut().unwrap());
            self.emit(&goto_end[..]);
            self.emit_label(end);
        } else {
            let true_body = self.next_label();
            let end = self.next_label();
            let cond = self.visit_expr(&mut it.cond);
            self.emit(format!("    if ( {} ) goto {}; else goto {};\n", self.ids.get_str(cond), true_body, end));
            self.emit_label(true_body);
            self.visit_stmt(&mut it.true_body);
            self.emit_label(end);
        }
        start
    }
    fn visit_jmp(&mut self, it: &mut JumpStmt) -> Label {
        let start = self.next_label();
        self.emit_label(start);
        match it {
            JumpStmt::Break => self.emit("    break;\n                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "),
            JumpStmt::Continue => self.emit("    continue;\n"),
            JumpStmt::Return((None, _)) => self.emit("    return;\n"),
            JumpStmt::Return((Some(ref mut expr), _)) => {
                let val = self.visit_expr(expr);
                self.emit(format!("    return {};\n", self.ids.get_id(val)));
            },
        }
        start
    }
    fn visit_declaration(&mut self, it: &mut Declaration) -> Label {
        let start = self.next_label();
        self.emit_label(start)
        if let Some(ref mut initializer) = it.initializer.as_mut() {
            let val = self.visit_expr(initializer);
            self.emit(format!("    {} _{} = {};\n", it.ty.to_code(), self.ids.get_str(it.name), self.ids.get_str(val)));
        } else {
            self.emit(format!("    {} _{};\n", it.ty.to_code(), self.ids.get_str(it.name)));
        }
        start
    }

    fn visit_expr(&mut self, it: &mut Expr) -> Id {
        let span = it.span.clone();
        let prev_ty = self.current_ty.clone();
        self.current_ty = it.ty.clone();
        let ret = match &mut it.expr {
            ExprKind::Index(ref mut index)        => self.visit_index(index),
            ExprKind::Dot(ref mut dot)            => self.visit_dot(dot),
            ExprKind::Deref(ref mut deref)        => self.visit_deref(deref),
            ExprKind::Call(ref mut call)          => self.visit_call(call),
            ExprKind::MethodCall(ref mut mcall)   => self.visit_method_call(mcall),
            ExprKind::SizeOfExpr(ref mut ty)      => self.visit_sizeofexpr(ty, span),
            ExprKind::MulExpr(ref mut mulexpr)    => self.visit_mulexpr(mulexpr),
            ExprKind::AddExpr(ref mut addexpr)    => self.visit_addexpr(addexpr),
            ExprKind::CmpExpr(ref mut cmpexpr)    => self.visit_cmpexpr(cmpexpr),
            ExprKind::EqExpr(ref mut eqexpr)      => self.visit_eqexpr(eqexpr),
            // Bitwise negation
            ExprKind::InverseExpr(ref mut iexpr)  => self.visit_inverseexpr(iexpr),
            // boolean negation
            ExprKind::NotExpr(ref mut expr)       => self.visit_notexpr(expr),
            ExprKind::AssignExpr(ref mut assignexpr) 
                                => self.visit_assignexpr(assignexpr),
            ExprKind::LeaExpr(ref mut expr)       => self.visit_leaexpr(expr),
            ExprKind::Cast(ref mut cast)          => self.visit_cast(cast),
            ExprKind::Ident(ref mut id)           => self.visit_ident(id),
            ExprKind::Number(ref mut n)           => self.visit_number(n),
            ExprKind::NoOp                        => 0,
        };
        self.current_ty = prev_ty;
        ret
    }
    fn visit_index(&mut self, it: &mut Index) -> Id {
        let ty = self.current_ty.clone();
        let base = self.visit_expr(&mut it.base);
        let offset = self.visit_expr(&mut it.offset);
        let result = self.next_tmp();
        self.emit(format!("    {} {} = {}[{}];\n", ty.to_code(&self.ids), self.ids.get_str(result), self.ids.get_str(base), self.ids.get_str(offset)));
        result
    }
    fn visit_dot(&mut self, it: &mut Dot) -> Id {
        let result = self.next_tmp();
        let lhs = self.visit_expr(&mut it.lhs);
        self.emit(format!("    {} {} = {}._{};\n", self.current_ty.to_code(&self.ids), self.ids.get_str(result), self.ids.get_str(lhs), self.ids.get_str(it.field_name)));
        result
    }
    fn visit_deref(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        let reference = self.visit_expr(it);
        self.emit(format!("    {} {} = *{};\n", self.current_ty.to_code(&self.ids), self.ids.get_str(result), self.ids.get_str(reference)));
        result
    }
    fn visit_call(&mut self, it: &mut Call) -> Id {
        let result = self.next_tmp();
        let args = it.args.iter_mut().map(|exp| self.visit_expr(exp)).collect::<Vec<Id>>();
        let mut args_string = String::with_capacity(5 * args.len());
        if args.len() != 0 {
            for arg in args {
                args_string.push_str(format!("{}, ", self.ids.get_str(args)).as_str());
            }
            // get rid of trailing comma
            args_string.pop();
            args_string.pop();
        }

        let ind = it.f.clone().unwrap();
        let fn_name =
            if self.fns[ind].len() == 1 {
                format!("{}", self.ids.get_str(it.fn_name))
            } else {
                format!("{}_{:X}", self.ids.get_str(it.fn_name), ind)
            };
        self.emit(format!("    {} {} = _{}( {} );\n", self.current_ty.to_code(&self.ids), self.ids.get_str(result), fn_name, args);
        result
    }
    fn visit_method_call(&mut self, it: &mut MethodCall) -> Id {
        let result = self.next_tmp();
        let args = it.args.iter_mut().map(|exp| self.visit_expr(exp)).collect::<Vec<Id>>();
        let mut args_string = String::with_capacity(5 * args.len());
        if args.len() != 0 {
            for arg in args {
                args_string.push_str(format!("{}, ", self.ids.get_str(args)).as_str());
            }
            // get rid of trailing comma
            args_string.pop();
            args_string.pop();
        }

        let lhs = self.visit_expr(&mut it.lhs);
        let struct_name = match it.lhs.ty.clone().kind {
            TyKind::Struct(name) => name,
            _ => unreachable!(),
        };

        let structure = self.structs[struct_name].clone();
        if structure.type_id != -1 && structure.children.len() != 0 {
            let tid = self.next_tmp();
            self.emit(format!("i8 {} = *((i8*) &{});\n", idstore.get_str(tid), idstore.get_str(lhs)));
            let fn_ptr = self.next_tmp();
            let (conforming_struct, index) = it.f.clone().unwrap();
            let root_parent = self.get_root_struct(conforming_struct);
            let (method_decl, vtable_signature) = {
                let func = &self.structures[conforming_struct].methods[it.method_name][index];
                (func.method_ptr_decl(struct_name, fn_ptr, &self.ids), func.vtable_signature(&self.ids))
            };
            self.emit(format!("    {} = {}_{}[{}];\n", method_decl, self.ids.get_str(root_parent), vtable_signature, self.ids.get_str(tid)));
            self.emit(format!("    {} {} = (*{})({});\n", self.current_ty.to_code(&self.ids), self.ids.get_str(result), self.ids.get_str(fn_ptr), args));
        } else {
            let (conforming_struct, index) = it.f.clone().unwrap();
            let method_signature = self.structures[conforming_struct].methods[it.method_name][index];
            self.emit(format!("    {} {} = {}_{}_{}({});\n", self.current_ty.to_code(&self.ids), self.ids.get_str(result), self.ids.get_str(conforming_struct), 
                                self.ids.get_str(it.method_name), method_signature, args));
        }
        result
    }
    fn visit_sizeofexpr(&mut self, it: &mut Ty, span: PosSpan) -> Id {
        let result = self.next_tmp();
        self.emit(format!("    i64 {} = sizeof ({});\n", self.ids.get_str(result), it.to_code(&self.ids)));
        result
    }
    fn visit_mulexpr(&mut self, it: &mut MulExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it.tail().iter_mut(|x| (self.visit_expr(&mut x.1), x.0.clone())).collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp.push_str(format!("{} ", self.ids.get_str(term.0)));

        for term in terms {
            exp.push_str(format!("{} {} ", term.1.into::<&'static str>(), self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!("    {} {} = {};\n", self.current_ty.clone().to_code(),  self.ids.get_str(result), exp));
        result
    }
    fn visit_addexpr(&mut self, it: &mut AddExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it.tail().iter_mut(|x| (self.visit_expr(&mut x.1), x.0.clone())).collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp.push_str(format!("{} ", self.ids.get_str(term.0)));

        for term in terms {
            exp.push_str(format!("{} {} ", term.1.into::<&'static str>(), self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!("    {} {} = {};\n", self.current_ty.clone().to_code(),  self.ids.get_str(result), exp));
        result
    }
    fn visit_cmpexpr(&mut self, it: &mut CmpExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it.tail().iter_mut(|x| (self.visit_expr(&mut x.1), x.0.clone())).collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp.push_str(format!("{} ", self.ids.get_str(term.0)));

        for term in terms {
            exp.push_str(format!("{} {} ", term.1.into::<&'static str>(), self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!("    {} {} = {};\n", self.current_ty.clone().to_code(),  self.ids.get_str(result), exp));
        result
    }
    fn visit_eqexpr(&mut self, it: &mut EqExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it.tail().iter_mut(|x| (self.visit_expr(&mut x.1), x.0.clone())).collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp.push_str(format!("{} ", self.ids.get_str(term.0)));

        for term in terms {
            exp.push_str(format!("{} {} ", term.1.into::<&'static str>(), self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!("    {} {} = {};\n", self.current_ty.clone().to_code(),  self.ids.get_str(result), exp));
        result
    }
    fn visit_inverseexpr(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        let operand = self.visit_expr(it);
        self.emit(format!("    {} {} = ~{};\n", self.current_ty.to_code(), self.ids(result), self.ids.operand()));
        result
    }
    fn visit_notexpr(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        let operand = self.visit_expr(it);
        self.emit(format!("    {} {} = !{};\n", self.current_ty.to_code(), self.ids(result), self.ids.operand()));
        result
    }
    fn visit_assignexpr(&mut self, it: &mut AssignExpr) -> Id {
        let result = self.next_tmp();
        let operand = self.visit_expr(it);
        self.emit(format!("    {} {} = !{};\n", self.current_ty.to_code(), self.ids(result), self.ids.operand()));
        result
    }
    fn visit_leaexpr(&mut self, it: &mut Expr) -> Id { Label(0) }
    fn visit_cast(&mut self, it: &mut Cast) -> Id { Label(0) }
    fn visit_ident(&mut self, it: &mut Id) -> Id { Label(0) }
    fn visit_number(&mut self, it: &mut i64) -> Id { Label(0) }
    fn visit_new(&mut self, it: &mut Ty) -> Id {
        let binding = self.next_tmp();
        if it.ptr == 0 {
            match it.clone().kind {
                TyKind::I64 => self.emit(format!("    i64* {} = (i64*) malloc(8);\n", self.ids.get_str(binding))),
                TyKind::I8 => self.emit(format!("    i8* {} = (i8*) malloc(1);\n", self.ids.get_str(binding))),
                TyKind::Struct(name) => {
                    self.emit(format!("    struct {0}* {1} = (struct {0}*) malloc ( sizeof(struct {0}) );\n", self.ids.get_str(name), self.ids.get_str(binding)));
                    let type_init_code = {
                        let structure = self.structs[name];
                        if structure.type_id != -1 {
                            Some(format!("    {}->type_id[0] = {};\n", self.ids.get_str(binding), structure.type_id))
                        } else {
                            None
                        }
                    };
                    type_init_code.map(|s| self.emit(s));
                },
                TyKind::I0 => unreachable!(),
            }
        } else {
            self.emit(format!("{0}* {1} = ({0}*) malloc(sizeof (void*));\n", it.ty.to_code(), self.ids.get_str(binding)));
        }
        binding
    }

    fn visit_function(&mut self, it: &mut Function) -> Label { Label(0) }
    fn visit_structure(&mut self, it: &mut Structure) -> Label { Label(0) }
    fn visit_ty(&mut self, it: &mut Ty) -> Label { Label(0) }
    fn visit_id(&mut self, it: &mut Id) -> Label { Label(0) }
}