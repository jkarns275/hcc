use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;

pub mod typecheck;

trait Visitor {

    fn visit_struct(&mut self, it: &mut Structure) -> Ty;

    fn visit_stmt(&mut self, it: &mut Statement) -> Ty {
        match it {
            Statement::Body(ref mut body)          => self.visit_body(body.as_mut()),
            Statement::If(ref mut ifstmt)          => self.visit_if(ifstmt.as_mut()),
            Statement::While(ref mut whilestmt)    => self.visit_while(whilestmt.as_mut()),
            Statement::Expr(ref mut expr)          => self.visit_expr(expr),
            Statement::Jump(ref mut jmpstmt)       => self.visit_jmp(jmpstmt),
            Statement::Declaration(ref mut decl)   => self.visit_declaration(decl),
        }
    }
    fn visit_while(&mut self, it: &mut WhileStmt) -> Ty;
    fn visit_body(&mut self, it: &mut Body) -> Ty;
    fn visit_if(&mut self, it: &mut IfStmt) -> Ty;
    fn visit_jmp(&mut self, it: &mut JumpStmt) -> Ty;
    fn visit_declaration(&mut self, it: &mut Declaration) -> Ty;

    fn visit_expr(&mut self, it: &mut Expr) -> Ty {
        let span = it.span.clone();
        let ty = match &mut it.expr {
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
            /// Bitwise negation
            ExprKind::InverseExpr(ref mut iexpr)  => self.visit_inverseexpr(iexpr),
            /// boolean negation
            ExprKind::NotExpr(ref mut expr)       => self.visit_notexpr(expr),
            ExprKind::AssignExpr(ref mut assignexpr) 
                                => self.visit_assignexpr(assignexpr),
            ExprKind::LeaExpr(ref mut expr)       => self.visit_leaexpr(expr),
            ExprKind::Cast(ref mut cast)          => self.visit_cast(cast),
            ExprKind::Ident(ref mut id)           => self.visit_ident(id),
            ExprKind::Number(ref mut n)           => self.visit_number(n),
            ExprKind::NoOp                        => Ty::new(TyKind::I0),
        };
        it.ty = Some(ty.clone());
        ty
    }
    fn visit_index(&mut self, it: &mut Index) -> Ty;
    fn visit_dot(&mut self, it: &mut Dot) -> Ty;
    fn visit_deref(&mut self, it: &mut Expr) -> Ty;
    fn visit_call(&mut self, it: &mut Call) -> Ty;
    fn visit_method_call(&mut self, it: &mut MethodCall) -> Ty;
    fn visit_sizeofexpr(&mut self, it: &mut Ty, span: PosSpan) -> Ty;
    fn visit_mulexpr(&mut self, it: &mut MulExpr) -> Ty;
    fn visit_addexpr(&mut self, it: &mut AddExpr) -> Ty;
    fn visit_cmpexpr(&mut self, it: &mut CmpExpr) -> Ty;
    fn visit_eqexpr(&mut self, it: &mut EqExpr) -> Ty;
    fn visit_inverseexpr(&mut self, it: &mut Expr) -> Ty;
    fn visit_notexpr(&mut self, it: &mut Expr) -> Ty;
    fn visit_assignexpr(&mut self, it: &mut AssignExpr) -> Ty;
    fn visit_leaexpr(&mut self, it: &mut Expr) -> Ty;
    fn visit_cast(&mut self, it: &mut Cast) -> Ty;
    fn visit_ident(&mut self, it: &mut Id) -> Ty;
    fn visit_number(&mut self, it: &mut i64) -> Ty;

    fn visit_function(&mut self, it: &mut Function) -> Ty;
    fn visit_structure(&mut self, it: &mut Structure) -> Ty;
    fn visit_ty(&mut self, it: &mut Ty) -> Ty;
    fn visit_id(&mut self, it: &mut Id) -> Ty;
    fn visit_declaration(&mut self, it: &mut Declaration) -> Ty;
}

trait Visitable {
    fn accept<V: Visitor>(&mut self, visitor: &mut V);
}