use ast::expr::*;
use ast::statement::*;
use ast::structure::*;
use ast::id::*;
use ast::declaration::*;
use ast::function::*;
use ast::ty::*;

use std::collections::{VecDeque, HashMap};


pub struct TypeChecker {
    pub var_stack: VecDeque<HashMap<Id, Ty>>,
    fns: Vec<Function>,
    structs: Vec<Structure>,
}

impl TypeChecker {
    pub fn new(structs: Vec<Structure>, fns: Vec<Function>) -> Self {
        TypeChecker {
            var_stack: VecDeque::with_capacity(8),
            structs,
            fns,
        }
    }
}