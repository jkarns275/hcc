use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;
use ast::*;

use std::fmt::{Formatter, self, Display};

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
    pub output: String,
    pub fns: HashMap<Id, Vec<Rc<Function>>>,
    pub structs: HashMap<Id, Rc<Structure>>,
    pub vtables: HashMap<Id, Vec<Rc<Structure>>>,
}


impl<'a> CG<'a> {

    fn emit<S: Into<String>>(&mut self, string: S) { self.output.push_str(string.into().as_str()); }
    fn emit_label(&mut self, label: Label) { self.output.push_str(label.to_string()); }

    pub fn visit_struct(&mut self, it: &mut Structure) -> Label {
        self.emit(format!("struct _{} {{\n", self.ids.get_string(it.name))));
        
        for (field_name, field) in it.fields.iter() {
            self.emit(format!("    {} _{};\n", field.ty.to_string(&self.ids), self.ids.get_string(field_name).clone()));
        }

        let mut parent = it.parent.clone();
        while let Some(super_name) = parent.take() {
            let structure = self.structs[super_name].clone();
            let mut comment = true;
            for (field_name, field) in structure.fields.iter() {
                if comment {
                    comment = false;
                    self.emit(format!("    // Field from parent type 'struct {}'\n", self.ids.get_string(super_name).unwrap()));
                }
                self.emit(format!("    {} _{};\n". field.ty.to_String(&self.ids), self.ids.get_string(field_name).clone()));
            }
        }


        if it.parent.is_some() {
            let mut par = it.parent.clone();
            while let Some(parent) = par.clone() {
                par = self.structs[parent].parent.clone();
            }
            let parent = par.unwrap();
            let entry = self.vtables.entry(parent).or_insert_with(|| vec![]);
            let id = entry.len();
            entry.push(self.structs[it.name].clone());
            self.emit(format!("    i8 type_id;\n"));
            self.emit(format!("}} {}_default = {{ .type_id = {:x} }};", self.ids.get_string(it.name).to_string(), id));
        } else {
            self.emit("};\n")
        }
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
        let label = 
    }
    fn visit_body(&mut self, it: &mut Body) -> Label { Label(0) }
    fn visit_if(&mut self, it: &mut IfStmt) -> Label { Label(0) } 
    fn visit_jmp(&mut self, it: &mut JumpStmt) -> Label { Label(0) }
    fn visit_declaration(&mut self, it: &mut Declaration) -> Label { Label(0) }

    fn visit_expr(&mut self, it: &mut Expr) -> Label {
        let span = it.span.clone();
        match &mut it.expr {
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
            ExprKind::NoOp                        => Label(0),
        }
    }
    fn visit_index(&mut self, it: &mut Index) -> Label { Label(0) }
    fn visit_dot(&mut self, it: &mut Dot) -> Label { Label(0) }
    fn visit_deref(&mut self, it: &mut Expr) -> Label { Label(0) }
    fn visit_call(&mut self, it: &mut Call) -> Label { Label(0) }
    fn visit_method_call(&mut self, it: &mut MethodCall) -> Label { Label(0) }
    fn visit_sizeofexpr(&mut self, it: &mut Ty, span: PosSpan) -> Label { Label(0) }
    fn visit_mulexpr(&mut self, it: &mut MulExpr) -> Label { Label(0) }
    fn visit_addexpr(&mut self, it: &mut AddExpr) -> Label { Label(0) }
    fn visit_cmpexpr(&mut self, it: &mut CmpExpr) -> Label { Label(0) }
    fn visit_eqexpr(&mut self, it: &mut EqExpr) -> Label { Label(0) }
    fn visit_inverseexpr(&mut self, it: &mut Expr) -> Label { Label(0) }
    fn visit_notexpr(&mut self, it: &mut Expr) -> Label { Label(0) }
    fn visit_assignexpr(&mut self, it: &mut AssignExpr) -> Label { Label(0) }
    fn visit_leaexpr(&mut self, it: &mut Expr) -> Label { Label(0) }
    fn visit_cast(&mut self, it: &mut Cast) -> Label { Label(0) }
    fn visit_ident(&mut self, it: &mut Id) -> Label { Label(0) }
    fn visit_number(&mut self, it: &mut i64) -> Label { Label(0) }

    fn visit_function(&mut self, it: &mut Function) -> Label { Label(0) }
    fn visit_structure(&mut self, it: &mut Structure) -> Label { Label(0) }
    fn visit_ty(&mut self, it: &mut Ty) -> Label { Label(0) }
    fn visit_id(&mut self, it: &mut Id) -> Label { Label(0) }
}