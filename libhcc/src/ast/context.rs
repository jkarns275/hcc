use std::collections::HashMap;
use ast::structure::Structure;
use ast::id::Id;
use ast::id::IdStore;
use ast::function::Function;
use std::rc::Rc;
use ast::AstError;

pub struct Context<'r> {
    pub structs: HashMap<Id, Rc<Structure>>,
    pub idstore: IdStore<'r>,
    pub functions: HashMap<Id, Vec<Rc<Function>>>,
    pub errors: Vec<AstError>
}

impl<'r> Context<'r> {
    pub fn new() -> Self {
        Context {
            structs: HashMap::new(),
            idstore: IdStore::new(),
            functions: HashMap::new(),
            errors: Vec::new(),
        }
    }
}