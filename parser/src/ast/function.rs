use ast::ty::Ty;
use ast::id::Id;
use std::collections::HashMap;
use ast::statement::Body;

pub struct Function {
    pub name: Id,
    pub return_type: Ty,
    pub args: HashMap<Id, Ty>,
    pub body: Body,
}