use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;

pub mod typecheck;

trait Visitor {

    fn visit_struct(&mut self, it: &mut Structure);

    fn visit_stmt(&mut self, it: &mut Statement) {
        match it {
            Statement::Body(ref mut body)          => self.visit_body(body.as_mut()),
            Statement::If(ref mut ifstmt)          => self.visit_if(ifstmt.as_mut()),
            Statement::While(ref mut whilestmt)    => self.visit_while(whilestmt.as_mut()),
            Statement::Expr(ref mut expr)          => self.visit_expr(expr),
            Statement::Jump(ref mut jmpstmt)       => self.visit_jmp(jmpstmt),
            Statement::Declaration(ref mut decl)   => self.visit_declaration(decl),
        }
    }
    fn visit_while(&mut self, it: &mut WhileStmt);
    fn visit_body(&mut self, it: &mut Body);
    fn visit_if(&mut self, it: &mut IfStmt);
    fn visit_jmp(&mut self, it: &mut JumpStmt);
    fn visit_declaration(&mut self, it: &mut Declaration);

    fn visit_expr(&mut self, it: &mut Expr) {
        match &mut it.expr {
            ExprKind::Index(ref mut index)        => self.visit_index(index),
            ExprKind::Dot(ref mut dot)            => self.visit_dot(dot),
            ExprKind::Deref(ref mut deref)        => self.visit_deref(deref),
            ExprKind::Call(ref mut call)          => self.visit_call(call),
            ExprKind::MethodCall(ref mut mcall)   => self.visit_method_call(mcall),
            ExprKind::SizeOfExpr(ref mut ty)      => self.visit_sizeofexpr(ty),
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
            ExprKind::NoOp                => {},
        }
    }
    fn visit_index(&mut self, it: &mut Index);
    fn visit_dot(&mut self, it: &mut Dot);
    fn visit_deref(&mut self, it: &mut Expr);
    fn visit_call(&mut self, it: &mut Call);
    fn visit_method_call(&mut self, it: &mut MethodCall);
    fn visit_sizeofexpr(&mut self, it: &mut Ty);
    fn visit_mulexpr(&mut self, it: &mut MulExpr);
    fn visit_addexpr(&mut self, it: &mut AddExpr);
    fn visit_cmpexpr(&mut self, it: &mut CmpExpr);
    fn visit_eqexpr(&mut self, it: &mut EqExpr);
    fn visit_inverseexpr(&mut self, it: &mut Expr);
    fn visit_notexpr(&mut self, it: &mut Expr);
    fn visit_assignexpr(&mut self, it: &mut AssignExpr);
    fn visit_leaexpr(&mut self, it: &mut Expr);
    fn visit_cast(&mut self, it: &mut Cast);
    fn visit_ident(&mut self, it: &mut Id);
    fn visit_number(&mut self, it: &mut i64);

    fn visit_function(&mut self, it: &mut Function);
    fn visit_structure(&mut self, it: &mut Structure);
    fn visit_ty(&mut self, it: &mut Ty);
    fn visit_id(&mut self, it: &mut Id);
    fn visit_declaration(&mut self, it: &mut Declaration);

}

trait Visitable {
    fn accept<V: Visitor>(&mut self, visitor: &mut V);
}