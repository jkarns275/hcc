use ast::declaration::*;
use ast::expr::*;
use ast::function::*;
use ast::id::*;
use ast::statement::*;
use ast::structure::*;
use ast::ty::*;
use ast::*;

use std::borrow::*;
use std::cell::*;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Copy, Clone)]
pub struct Label(usize);

impl Display for Label {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "l{}", self.0)
    }
}

impl Label {
    pub fn to_string(self) -> String {
        let mut r = "l".to_string();
        r.push_str(self.0.to_string().as_str());
        r
    }
}

pub struct CG {
    pub ids: IdStore,
    pub lbl_counter: usize,
    pub tmp_counter: usize,
    pub output: String,
    pub fns: HashMap<Id, Vec<Rc<UnsafeCell<Function>>>>,
    pub structs: HashMap<Id, Rc<UnsafeCell<Structure>>>,
    pub vtables: HashMap<Id, Vec<Rc<UnsafeCell<Structure>>>>,
    pub parents: HashMap<Id, Rc<UnsafeCell<Structure>>>,
    pub continue_stack: Vec<Label>,
    pub break_stack: Vec<Label>,
    pub current_ty: Ty,
    pub roots: HashMap<Id, Id>,
}

impl CG {
    pub fn codegen(ast: Ast) -> String {
        //ids: IdStore, fns: HashMap<Id, Vec<Rc<Function>>>, structs: HashMap<Id, Rc<RefCell<Structure>>>) -> String {
        let structs = {
            let mut structs = HashMap::with_capacity(ast.structs.len());
            for (name, structure) in ast.structs {
                structs.insert(
                    name,
                    Rc::new(UnsafeCell::new(Rc::try_unwrap(structure).ok().unwrap())),
                );
            }
            structs
        };
        let fns = {
            let mut fns = HashMap::with_capacity(ast.functions.len());
            for (name, flist) in ast.functions {
                fns.insert(
                    name,
                    flist
                        .into_iter()
                        .map(|x| Rc::new(UnsafeCell::new(Rc::try_unwrap(x).ok().unwrap())))
                        .collect::<Vec<_>>(),
                );
            }
            fns
        };
        let mut cg = CG {
            ids: ast.idstore,
            fns,
            structs,
            vtables: HashMap::new(),
            parents: HashMap::new(),
            current_ty: Ty::error(),
            roots: HashMap::new(),
            output: String::new(),
            continue_stack: vec![],
            break_stack: vec![],
            lbl_counter: 0,
            tmp_counter: 0,
        };

        cg.visit_struct_headers();
        cg.visit_function_headers();

        let structures = cg.structs.clone();
        for structure in structures.values() {
            let mut structure = unsafe { (**structure).get().as_mut() }.unwrap();
            cg.visit_method_headers(&mut *structure);
        }

        for structure in structures.values() {
            let mut structure = unsafe { (**structure).get().as_mut() }.unwrap();
            cg.visit_struct(&mut *structure);
            cg.visit_methods(&mut *structure);
        }

        cg.visit_functions();

        cg.output
    }

    fn get_root_struct(&mut self, child: Id) -> Id {
        if self.roots.contains_key(&child) {
            self.roots[&child]
        } else {
            let mut par = Some(child);
            while let Some(parent) = par {
                if unsafe { (*self.structs[&parent]).get().as_ref() }
                    .unwrap()
                    .parent
                    .is_none()
                {
                    break;
                }
                par = unsafe { (*self.structs[&parent]).get().as_ref() }
                    .unwrap()
                    .parent;
            }
            let parent = par.unwrap();
            self.roots.insert(child, parent);
            parent
        }
    }

    fn emit<S: Into<String>>(&mut self, string: S) {
        self.output.push_str(string.into().as_str());
    }
    fn emit_label(&mut self, label: Label) {
        self.output += format!("{}: ;\n", label.to_string()).as_str();
    }

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

    fn generate_fn_name(&self, it: &Function, i: usize) -> String {
        if it.intrinsic {
            format!("{}", self.ids.get_str(it.name))
        } else {
            format!("{}_{:#x}", self.ids.get_str(it.name), i)
        }
    }

    pub fn generate_method_header(
        &mut self,
        it: &Function,
        conforming_structure: Id,
        structure: Id,
    ) -> String {
        let mut args = format!(
            "{} _this, ",
            Ty::new(TyKind::Struct(structure))
                .ptr_to()
                .to_code(&self.ids)
        );
        for arg in it.arg_order.iter() {
            let decl = &it.args[arg];
            args.push_str(
                format!(
                    "{} _{}, ",
                    decl.ty.to_code(&self.ids),
                    self.ids.get_str(*arg)
                )
                .as_str(),
            )
        }
        args.pop();
        args.pop();

        // self.ids.get_str(it.method_name), method_signature, args
        let method_name = format!(
            "{}_{}_{}_{}",
            self.ids.get_str(conforming_structure),
            self.ids.get_str(structure),
            self.ids.get_str(it.name),
            it.signature(&self.ids)
        );
        format!(
            "{} {}({})",
            it.return_type.to_code(&self.ids),
            method_name,
            args
        )
    }

    pub fn generate_struct_header(&mut self, it: &Structure) -> String {
        format!("struct _{}", self.ids.get_str(it.name))
    }

    pub fn visit_struct_headers(&mut self) {
        let structures = self.structs.values().map(|x| x.clone()).collect::<Vec<_>>();
        for structure in structures {
            let structure = unsafe { (*structure).get().as_ref() }.unwrap();
            self.visit_struct_header(&*structure);
        }
    }

    pub fn visit_function_headers(&mut self) {
        let cloned_fns = self.fns.clone();
        for (_name, fn_list) in cloned_fns {
            for i in 0..fn_list.len() {
                self.visit_function_header(unsafe { fn_list[i].get().as_ref() }.unwrap(), i);
            }
        }
    }

    pub fn visit_functions(&mut self) {
        let cloned_fns = self.fns.clone();
        for (_name, fn_list) in cloned_fns {
            for i in 0..fn_list.len() {
                self.visit_function(unsafe { fn_list[i].get().as_mut() }.unwrap(), i);
            }
        }
    }

    pub fn visit_method_headers(&mut self, it: &mut Structure) {
        let mut method_set = HashSet::<(Id, String)>::new();
        let mut cur_struct = Some(it.name);
        while let Some(struct_name) = cur_struct.take() {
            let structure_rc = self.structs[&struct_name].clone();
            let mut structure = unsafe { (*structure_rc).get().as_ref() }.unwrap();
            for (id, functions) in structure.methods.iter() {
                for f in functions.iter() {
                    let method_signature = (*id, f.signature(&self.ids));
                    if method_set.contains(&method_signature) {
                        continue;
                    }
                    let header = self.generate_method_header(f, struct_name, it.name);
                    self.emit(format!("{};\n", header));
                    method_set.insert(method_signature);
                }
            }
            cur_struct = structure.parent;
        }
    }

    pub fn visit_methods(&mut self, it: &mut Structure) {
        let mut method_set = HashSet::<(Id, String)>::new();
        let mut cur_struct = Some(it.name);
        while let Some(struct_name) = cur_struct.take() {
            let structure_rc = self.structs[&struct_name].clone();
            let mut structure = unsafe { (*structure_rc).get().as_mut() }.unwrap();
            for (id, functions) in structure.methods.iter_mut() {
                for f in functions.iter_mut() {
                    let method_signature = (*id, f.signature(&self.ids));
                    if method_set.contains(&method_signature) {
                        continue;
                    }
                    self.visit_method(f, struct_name, it.name);
                    method_set.insert(method_signature);
                }
            }
            cur_struct = structure.parent;
        }
    }

    pub fn visit_struct_header(&mut self, it: &Structure) {
        let header = self.generate_struct_header(it);
        self.emit(format!("{};", header));
    }

    pub fn visit_struct(&mut self, it: &mut Structure) {
        let header = self.generate_struct_header(it);
        self.emit(format!("{} {{\n", header));

        if it.type_id != -1 {
            let par = it.parent.clone();
            let root_parent = self.get_root_struct(it.name);
            {
                let entry = self.vtables.entry(root_parent).or_insert_with(|| vec![]);
                let id = entry.len();
                entry.push(self.structs[&it.name].clone());
                it.type_id = id as i8;
            }
            self.emit(format!("    i8 type_id[4]; // Runtime type information \n"));
        }

        for (field_name, field) in it.fields.iter() {
            self.emit(format!(
                "    {} _{};\n",
                field.ty.to_code(&self.ids),
                self.ids.get_str(*field_name)
            ));
        }

        let mut parent = it.parent.clone();
        while let Some(super_name) = parent.take() {
            let structure_rc = self.structs[&super_name].clone();
            self.parents.insert(it.name, structure_rc.clone());
            let structure = unsafe { (*structure_rc).get().as_ref() }.unwrap();
            let mut comment = true;
            for (field_name, field) in structure.fields.iter() {
                if comment {
                    comment = false;
                    self.emit(format!(
                        "    // Field from parent type 'struct {}'\n",
                        self.ids.get_string(super_name).unwrap()
                    ));
                }
                self.emit(format!(
                    "    {} _{};\n",
                    field.ty.to_code(&self.ids),
                    self.ids.get_str(*field_name)
                ));
            }
        }

        self.emit("};\n")
    }

    fn visit_stmt(&mut self, it: &mut Statement) -> Label {
        match it {
            Statement::Body(ref mut body) => self.visit_body(body.as_mut()),
            Statement::If(ref mut ifstmt) => self.visit_if(ifstmt.as_mut()),
            Statement::While(ref mut whilestmt) => self.visit_while(whilestmt.as_mut()),
            Statement::Expr(ref mut expr) => {
                self.visit_expr(expr);
                Label(0)
            }
            Statement::Jump(ref mut jmpstmt) => self.visit_jmp(jmpstmt),
            Statement::Declaration(ref mut decl) => self.visit_declaration(decl),
        }
    }
    
    fn visit_while(&mut self, it: &mut WhileStmt) -> Label {
        let loop_start = self.next_label();
        self.continue_stack.push(loop_start);
        self.emit_label(loop_start);
        let loop_end = self.next_label();
        self.break_stack.push(loop_end);
        let cond = self.visit_expr(&mut it.cond);
        self.emit(format!(
            "    if ( {} ) goto {}; // Loop cond check\n",
            self.ids.get_str(cond),
            loop_start
        ));
        self.visit_stmt(&mut it.body);
        self.emit(format!("    goto {}; // Goto start of loop\n", loop_end));
        self.emit_label(loop_end);
        self.continue_stack.pop();
        self.break_stack.pop();
        loop_start
    }
    
    fn visit_body(&mut self, it: &mut Body) -> Label {
        let body_start = self.next_label();
        for stmt in it.stmts.iter_mut() {
            self.visit_stmt(stmt);
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
            let goto_end = format!("    goto {};\n", end);
            let cond = self.visit_expr(&mut it.cond);
            self.emit(format!(
                "    if ( {} ) goto {};\n    else goto {};\n",
                self.ids.get_str(cond),
                true_body,
                false_body
            ));
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
            self.emit(format!(
                "    if ( {} ) goto {}; else goto {};\n",
                self.ids.get_str(cond),
                true_body,
                end
            ));
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
            JumpStmt::Break => self.emit(format!(
                "    goto {};\n",
                self.break_stack.last().cloned().unwrap().to_string()
            )),
            JumpStmt::Continue => self.emit(format!(
                "    goto {};\n",
                self.continue_stack.last().cloned().unwrap().to_string()
            )),
            JumpStmt::Return((None, _)) => self.emit("    return;\n"),
            JumpStmt::Return((Some(ref mut expr), _)) => {
                let val = self.visit_expr(expr);
                self.emit(format!("    return {};\n", self.ids.get_str(val)));
            }
        }
        start
    }

    fn visit_declaration(&mut self, it: &mut Declaration) -> Label {
        let start = self.next_label();
        self.emit_label(start);
        if let Some(ref mut initializer) = it.initializer.as_mut() {
            let val = self.visit_expr(initializer);
            self.emit(format!(
                "    {} _{} = {};\n",
                it.ty.to_code(&self.ids),
                self.ids.get_str(it.name),
                self.ids.get_str(val)
            ));
        } else {
            self.emit(format!(
                "    {} _{};\n",
                it.ty.to_code(&self.ids),
                self.ids.get_str(it.name)
            ));
        }
        Label(0)
    }

    fn visit_expr(&mut self, it: &mut Expr) -> Id {
        let span = it.span.clone();
        let prev_ty = self.current_ty.clone();
        self.current_ty = it.ty.clone().unwrap_or(Ty::error());
        let ret = match &mut it.expr {
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
            ExprKind::NoOp => panic!("a"),
        };
        self.current_ty = prev_ty;
        ret
    }

    fn visit_index(&mut self, it: &mut Index) -> Id {
        let ty = self.current_ty.clone();
        let base = self.visit_expr(&mut it.base);
        let offset = self.visit_expr(&mut it.offset);
        let result = self.next_tmp();
        self.emit(format!(
            "    {} {} = {}[{}];\n",
            ty.to_code(&self.ids),
            self.ids.get_str(result),
            self.ids.get_str(base),
            self.ids.get_str(offset)
        ));
        result
    }

    fn visit_dot(&mut self, it: &mut Dot) -> Id {
        let result = self.next_tmp();
        let lhs = self.visit_expr(&mut it.lhs);
        self.emit(format!(
            "    {} {} = {}._{};\n",
            self.current_ty.to_code(&self.ids),
            self.ids.get_str(result),
            self.ids.get_str(lhs),
            self.ids.get_str(it.field_name)
        ));
        result
    }

    fn visit_deref(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        let reference = self.visit_expr(it);
        self.emit(format!(
            "    {} {} = *{};\n",
            self.current_ty.to_code(&self.ids),
            self.ids.get_str(result),
            self.ids.get_str(reference)
        ));
        result
    }

    fn visit_call(&mut self, it: &mut Call) -> Id {
        let args = it
            .args
            .iter_mut()
            .map(|exp| self.visit_expr(exp))
            .collect::<Vec<Id>>();
        let mut args_string = String::with_capacity(5 * args.len());
        if args.len() != 0 {
            for arg in args {
                args_string.push_str(format!("{}, ", self.ids.get_str(arg)).as_str());
            }
            // get rid of trailing comma
            args_string.pop();
            args_string.pop();
        }

        let ind = it.f.clone().unwrap();
        let function = self.fns[&it.fn_name][ind].clone();
        let fn_name = self.generate_fn_name(unsafe { (*function).get().as_ref() }.unwrap(), ind);
        
        if self.current_ty.kind == TyKind::I0 {
            self.emit(format!("{}( {} );\n", fn_name, args_string));
            0
        } else {
            let result = self.next_tmp();
            self.emit(format!(
                "    {} {} = {}( {} );\n",
                self.current_ty.to_code(&self.ids),
                self.ids.get_str(result),
                fn_name,
                args_string
            ));
            result
        }

    }

    fn visit_method_call(&mut self, it: &mut MethodCall) -> Id {
        let result = self.next_tmp();
        let mut args_string = String::with_capacity(5 * it.args.len());
        let lhs = self.visit_expr(&mut it.lhs);
        args_string += format!("{}, ", self.ids.get_str(lhs)).as_str();
        let args = it
            .args
            .iter_mut()
            .map(|exp| self.visit_expr(exp))
            .collect::<Vec<Id>>();
        let struct_name = match it.lhs.ty.clone().unwrap().kind {
            TyKind::Struct(name) => name,
            _ => unreachable!(),
        };
        if args.len() != 0 {
            for arg in args {
                args_string.push_str(format!("{}, ", self.ids.get_str(arg)).as_str());
            }
            // get rid of trailing comma
            args_string.pop();
            args_string.pop();
        }

        let structure_rc = self.structs[&struct_name].clone();
        let structure = unsafe { (*structure_rc).get().as_ref() }.unwrap();
        if structure.type_id != -1 && structure.children.len() != 0 {
            let tid = self.next_tmp();
            self.emit(format!(
                "i8 {} = *((i8*) {});\n",
                self.ids.get_str(tid),
                self.ids.get_str(lhs)
            ));
            let fn_ptr = self.next_tmp();
            let (conforming_struct, index) = it.f.clone().unwrap();
            let root_parent = self.get_root_struct(conforming_struct);
            let (method_decl, vtable_signature) = {
                let structure_rc = self.structs[&struct_name].clone();
                let mut structure = unsafe { (*structure_rc).get().as_mut() }.unwrap();
                let func = structure.methods.get_mut(&it.method_name).unwrap()[index].borrow_mut();
                (
                    func.method_ptr_decl(struct_name, fn_ptr, &self.ids),
                    func.vtable_signature(&self.ids),
                )
            };
            self.emit(format!(
                "    {} = {}_{}[{}];\n",
                method_decl,
                self.ids.get_str(root_parent),
                vtable_signature,
                self.ids.get_str(tid)
            ));
            self.emit(format!(
                "    {} {} = (*{})({});\n",
                self.current_ty.to_code(&self.ids),
                self.ids.get_str(result),
                self.ids.get_str(fn_ptr),
                args_string
            ));
        } else {
            let (conforming_struct, index) = it.f.clone().unwrap();
            let method_signature = unsafe { (*self.structs[&conforming_struct]).get().as_ref() }
                .unwrap()
                .methods[&it.method_name][index]
                .signature(&self.ids);
            self.emit(format!(
                "    {} {} = {}_{}_{}_{}({});\n",
                self.current_ty.to_code(&self.ids),
                self.ids.get_str(result),
                self.ids.get_str(conforming_struct),
                self.ids.get_str(struct_name),
                self.ids.get_str(it.method_name),
                method_signature,
                args_string
            ));
        }
        result
    }

    fn visit_sizeofexpr(&mut self, it: &mut Ty, _span: PosSpan) -> Id {
        let result = self.next_tmp();
        self.emit(format!(
            "    i64 {} = sizeof ({});\n",
            self.ids.get_str(result),
            it.to_code(&self.ids)
        ));
        result
    }

    fn visit_mulexpr(&mut self, it: &mut MulExpr) -> Id {
        let result = self.next_tmp();
        let terms = it
            .tail
            .iter_mut()
            .map(|x| (self.visit_expr(&mut x.1), x.0.clone()))
            .collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp += format!("{} ", self.ids.get_str(first)).as_str();

        for term in terms {
            let op: &'static str = term.1.into();
            exp.push_str(format!("{} {} ", op, self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!(
            "    {} {} = {};\n",
            self.current_ty.clone().to_code(&self.ids),
            self.ids.get_str(result),
            exp
        ));
        result
    }

    fn visit_addexpr(&mut self, it: &mut AddExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it
            .tail
            .iter_mut()
            .map(|x| (self.visit_expr(&mut x.1), x.0.clone()))
            .collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp += format!("{} ", self.ids.get_str(first)).as_str();

        for term in terms {
            let op: &'static str = term.1.into();
            exp.push_str(format!("{} {} ", op, self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!(
            "    {} {} = {};\n",
            self.current_ty.clone().to_code(&self.ids),
            self.ids.get_str(result),
            exp
        ));
        result
    }

    fn visit_cmpexpr(&mut self, it: &mut CmpExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it
            .tail
            .iter_mut()
            .map(|x| (self.visit_expr(&mut x.1), x.0.clone()))
            .collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp += format!("{} ", self.ids.get_str(first)).as_str();

        for term in terms {
            let op: &'static str = term.1.into();
            exp.push_str(format!("{} {} ", op, self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!(
            "    {} {} = {};\n",
            self.current_ty.clone().to_code(&self.ids),
            self.ids.get_str(result),
            exp
        ));
        result
    }

    fn visit_eqexpr(&mut self, it: &mut EqExpr) -> Id {
        let result = self.next_tmp();
        let mut terms = it
            .tail
            .iter_mut()
            .map(|x| (self.visit_expr(&mut x.1), x.0.clone()))
            .collect::<Vec<_>>();
        let mut exp = "".to_owned();

        let first = self.visit_expr(&mut it.head);
        exp += format!("{} ", self.ids.get_str(first)).as_str();

        for term in terms {
            let op: &'static str = term.1.into();
            exp.push_str(format!("{} {} ", op, self.ids.get_str(term.0)).as_str());
        }

        exp.pop();

        self.emit(format!(
            "    {} {} = {};\n",
            self.current_ty.clone().to_code(&self.ids),
            self.ids.get_str(result),
            exp
        ));
        result
    }

    fn visit_inverseexpr(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        let operand = self.visit_expr(it);
        self.emit(format!(
            "    {} {} = ~{};\n",
            self.current_ty.to_code(&self.ids),
            self.ids.get_str(result),
            self.ids.get_str(operand)
        ));
        result
    }

    fn visit_notexpr(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        let operand = self.visit_expr(it);
        self.emit(format!(
            "    {} {} = !{};\n",
            self.current_ty.to_code(&self.ids),
            self.ids.get_str(result),
            self.ids.get_str(operand)
        ));
        result
    }

    fn visit_assignexpr(&mut self, it: &mut AssignExpr) -> Id {
        for i in (0..it.tail.len() - 1).rev() {
            let op: &'static str = it.tail[i + 1].0.into();
            let value = self.visit_expr(&mut it.tail[i + 1].1);
            match &mut it.tail[i].1.expr {
                ExprKind::Dot(ref mut exp) => {
                    let lhs = self.visit_expr(&mut exp.lhs);
                    self.emit(format!(
                        "    {}.{} {} {};\n",
                        self.ids.get_str(lhs),
                        self.ids.get_str(exp.field_name),
                        op,
                        self.ids.get_str(value)
                    ));
                }
                ExprKind::Index(ref mut exp) => {
                    let index = self.visit_expr(&mut exp.offset);
                    let base = self.visit_expr(&mut exp.base);
                    self.emit(format!(
                        "    {}[{}] {} {};\n",
                        self.ids.get_str(base),
                        self.ids.get_str(index),
                        op,
                        self.ids.get_str(value)
                    ));
                }
                ExprKind::Deref(ref mut exp) => {
                    let ptr = self.visit_expr(exp);
                    self.emit(format!(
                        "    *{} {} {};\n",
                        self.ids.get_str(ptr),
                        op,
                        self.ids.get_str(value)
                    ));
                }
                ExprKind::Ident(ref mut id) => {
                    let name = format!("_{}", self.ids.get_str(*id));
                    self.emit(format!(
                        "    {} {} {};\n",
                        name,
                        op,
                        self.ids.get_str(value)
                    ));
                }
                _ => panic!("Fix the type checker for assignments dumbo."),
            };
        }
        let value = self.visit_expr(&mut it.tail[0].1);
        let op: &'static str = it.tail[0].0.into();
        match &mut it.head.expr {
            ExprKind::Dot(ref mut exp) => {
                let lhs = self.visit_expr(&mut exp.lhs);
                self.emit(format!(
                    "    {}.{} {} {};\n",
                    self.ids.get_str(lhs),
                    self.ids.get_str(exp.field_name),
                    op,
                    self.ids.get_str(value)
                ));
            }
            ExprKind::Index(ref mut exp) => {
                let index = self.visit_expr(&mut exp.offset);
                let base = self.visit_expr(&mut exp.base);
                self.emit(format!(
                    "    {}[{}] {} {};\n",
                    self.ids.get_str(base),
                    self.ids.get_str(index),
                    op,
                    self.ids.get_str(value)
                ));
            }
            ExprKind::Deref(ref mut exp) => {
                let ptr = self.visit_expr(exp);
                self.emit(format!(
                    "    *{} {} {};\n",
                    self.ids.get_str(ptr),
                    op,
                    self.ids.get_str(value)
                ));
            }
            ExprKind::Ident(ref mut id) => {
                let name = format!("_{}", self.ids.get_str(*id));
                self.emit(format!(
                    "    {} {} {};\n",
                    name,
                    op,
                    self.ids.get_str(value)
                ));
            }
            _ => panic!("Fix the type checker for assignments dumbo."),
        };
        value
    }

    fn visit_leaexpr(&mut self, it: &mut Expr) -> Id {
        let result = self.next_tmp();
        match &mut it.expr {
            ExprKind::Dot(ref mut exp) => {
                let lhs = self.visit_expr(&mut exp.lhs);
                self.emit(format!(
                    "    {} {} = &({}.{});\n",
                    self.current_ty.to_code(&self.ids),
                    self.ids.get_str(result),
                    self.ids.get_str(lhs),
                    self.ids.get_str(exp.field_name)
                ));
            }
            ExprKind::Index(ref mut exp) => {
                let index = self.visit_expr(&mut exp.offset);
                let base = self.visit_expr(&mut exp.base);
                self.emit(format!(
                    "    {} {} = {}[{}];\n",
                    self.current_ty.to_code(&self.ids),
                    self.ids.get_str(result),
                    self.ids.get_str(base),
                    self.ids.get_str(index)
                ));
            }
            ExprKind::Deref(ref mut exp) => {
                let ptr = self.visit_expr(exp);
                self.emit(format!(
                    "    {} {} = {};\n",
                    self.current_ty.to_code(&self.ids),
                    self.ids.get_str(result),
                    self.ids.get_str(ptr)
                ));
            }
            ExprKind::Ident(ref mut id) => {
                let name = format!("_{}", self.ids.get_str(*id));
                self.emit(format!(
                    "    {} {} = &{};\n",
                    self.current_ty.to_code(&self.ids),
                    self.ids.get_str(result),
                    name
                ));
            }
            _ => panic!("Fix the type checker for lea dumbo."),
        };
        result
    }

    fn visit_cast(&mut self, it: &mut Cast) -> Id {
        let result = self.next_tmp();
        let value = self.visit_expr(&mut it.expr);
        self.emit(format!(
            "    {} {} = ({}) {};\n",
            self.current_ty.to_code(&self.ids),
            self.ids.get_str(result),
            it.to.to_code(&self.ids),
            self.ids.get_str(value)
        ));
        result
    }

    fn visit_ident(&mut self, it: &mut Id) -> Id {
        self.ids.get_id(format!("_{}", self.ids.get_str(*it)))
    }

    fn visit_number(&mut self, it: &mut i64) -> Id {
        let result = self.next_tmp();
        self.emit(format!(
            "    {} {} = {:#x};\n",
            self.current_ty.to_code(&self.ids),
            self.ids.get_str(result),
            *it
        ));
        result
    }

    fn visit_new(&mut self, it: &mut Ty) -> Id {
        let binding = self.next_tmp();
        if it.ptr == 0 {
            match it.clone().kind {
                TyKind::I64 => self.emit(format!(
                    "    i64* {} = (i64*) malloc(8);\n",
                    self.ids.get_str(binding)
                )),
                TyKind::I8 => self.emit(format!(
                    "    i8* {} = (i8*) malloc(1);\n",
                    self.ids.get_str(binding)
                )),
                TyKind::Struct(name) => {
                    self.emit(format!(
                        "    struct {0}* {1} = (struct {0}*) malloc ( sizeof(struct {0}) );\n",
                        self.ids.get_str(name),
                        self.ids.get_str(binding)
                    ));
                    let type_init_code = {
                        let structure_rc = self.structs[&name].clone();
                        let structure = unsafe { (*structure_rc).get().as_ref() }.unwrap();
                        if structure.type_id != -1 {
                            Some(format!(
                                "    {}->type_id[0] = {};\n",
                                self.ids.get_str(binding),
                                structure.type_id
                            ))
                        } else {
                            None
                        }
                    };
                    type_init_code.map(|s| self.emit(s));
                }
                _ => unreachable!(),
            }
        } else {
            self.emit(format!(
                "{0}* {1} = ({0}*) malloc(sizeof (void*));\n",
                it.to_code(&self.ids),
                self.ids.get_str(binding)
            ));
        }
        binding
    }

    fn visit_method(
        &mut self,
        it: &mut Function,
        conforming_structure: Id,
        structure: Id,
    ) -> Label {
        let result = self.next_tmp();

        let method_header = self.generate_method_header(it, conforming_structure, structure);

        self.emit(format!("{} {{\n", method_header));

        {
            let body = it.body.as_mut().unwrap();
            self.visit_body(body);
        }

        self.emit("}\n");

        Label(0)
    }

    fn visit_function_header(&mut self, it: &Function, i: usize) {
        let mut args = String::new();
        for arg in it.arg_order.iter() {
            let decl = it.args.get(arg).unwrap();
            args.push_str(
                format!(
                    "{} _{}, ",
                    decl.ty.to_code(&self.ids),
                    self.ids.get_str(*arg)
                )
                .as_str(),
            );
        }
        args.pop();
        args.pop();

        let fn_name = self.generate_fn_name(it, i);

        let ret_type = it.return_type.to_code(&self.ids);

        self.emit(format!("{} {}({});\n", ret_type, fn_name, args));
    }

    fn visit_function(&mut self, it: &mut Function, i: usize) -> Label {
        if it.body.is_none() {
            return Label(0);
        }
        // format!("{}_{:X}", self.ids.get_str(it.fn_name), ind)
        let mut args = "  ".to_string();
        for arg in it.arg_order.iter() {
            let decl = it.args.get(arg).unwrap();
            args.push_str(
                format!(
                    "{} _{}, ",
                    decl.ty.to_code(&self.ids),
                    self.ids.get_str(*arg)
                )
                .as_str(),
            );
        }
        args.pop();
        args.pop();

        let fn_name = self.generate_fn_name(it, i);

        let ret_type = it.return_type.to_code(&self.ids);

        self.emit(format!("{} {}({}) {{\n", ret_type, fn_name, args));

        {
            let body = it.body.as_mut().unwrap();
            self.visit_body(body);
        }

        self.emit("}\n");

        Label(0)
    }
}
