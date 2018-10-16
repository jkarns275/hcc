use std::collections::HashMap;
use ast::structure::Structure;
use ast::id::Id;
use ast::id::IdStore;
use ast::function::Function;
use std::rc::Rc;

pub struct Context<'r> {
    pub structs: HashMap<Id, Rc<Structure>>,
    pub idstore: IdStore<'r>,
    pub functions: HashMap<Id, Vec<Function>>,
    pub errors: Vec<(usize, String)>
}