use std::rc::Rc;
use ast::structure::Structure;

#[derive(PartialEq, Eq)]
pub enum Ty {
    U0,
    U8,
    U64,
    Ptr(Box<Ty>),
    Struct(Rc<Structure>)
}