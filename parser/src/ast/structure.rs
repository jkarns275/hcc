use std::collections::HashMap;
use std::rc::Rc;
use ast::id::Id;
use ast::function::Function;
use ast::ty::Ty;

pub struct Structure {
    pub methods: HashMap<Id, Function>,
    pub fields: HashMap<Id, Ty>,
    pub parent: Option<Rc<Structure>>,
    pub name: Id
}

impl PartialEq for Structure {

    fn eq(&self, other: &Structure) -> bool {
        // TODO: Ensure that this is an okay way to check!
        self.name == other.name
    }
}

impl Eq for Structure {}