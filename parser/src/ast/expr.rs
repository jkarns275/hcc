use ast::ty::Ty;
use ast::id::Id;

pub struct FnCallExpr {
    name: Id,
    args: Vec<Expr>,
}

macro_rules! arithmetic_expr {
    ( $name:ident, $op_ty_name:ident ) => {
        pub struct $name {
            pub head: Expr,
            pub tail: Vec<($op_ty_name, Expr)>,
        }
    };
    ( $name:ident ) => {
        pub struct $name {
            pub items: Vec<Expr>
        }
    }
}

arithmetic_expr!( MulExpr, MulOp );
arithmetic_expr!( AddExpr, AddOp );
arithmetic_expr!( ShiftExpr, ShiftOp );
arithmetic_expr!( CmpExpr, CmpOp );
arithmetic_expr!( EqExpr, bool );
arithmetic_expr!( AndExpr );
arithmetic_expr!( XorExpr );
arithmetic_expr!( OrExpr );

pub enum MulOp { Mul, Div }
pub enum AddOp { Sub, Add }
pub enum ShiftOp { Lsh, Rsh }
pub enum CmpOp { Lt, Lte, Gt, Gte, }
pub enum AssignOp {
    Eq,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Lsh,
    Rsh,
    And,
    Or,
    Xor,
}

pub struct AssignExpr {
    pub lhs: Expr,
    pub op: AssignOp,
    pub rhs: Expr,
}

pub struct MethodCallExpr {
    pub name: Id,
    pub deref_call: bool,
    pub access: Expr
}

pub enum Expr {
    Deref(Box<Expr>),
    FnCall(Box<FnCallExpr>),
    MethodCall(Box<MethodCallExpr>),
    SizeOfExpr(Ty),
    MulExpr(Box<MulExpr>),
    AddExpr(Box<AddExpr>),
    ShiftExpr(Box<ShiftExpr>),
    CmpExpr(Box<CmpExpr>),
    EqExpr(Box<EqExpr>),
    AndExpr(Box<AndExpr>),
    XorExpr(Box<XorExpr>),
    OrExpr(Box<OrExpr>),
    AssignExpr(Box<AssignExpr>)
}