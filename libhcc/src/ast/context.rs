use ast::function::Function;
use ast::id::Id;
use ast::id::IdStore;
use ast::structure::Structure;
use ast::AstError;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Context {
    pub structs: HashMap<Id, Rc<Structure>>,
    pub idstore: IdStore,
    pub functions: HashMap<Id, Vec<Rc<Function>>>,
    pub errors: Vec<AstError>,
}

impl Context {
    pub fn new() -> Self {
        Context {
            structs: HashMap::new(),
            idstore: IdStore::new(),
            functions: HashMap::new(),
            errors: Vec::new(),
        }
    }
}
