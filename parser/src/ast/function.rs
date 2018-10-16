use ast::ty::Ty;
use ast::id::Id;
use std::collections::HashMap;
use ast::statement::Body;
use parser::Rule;
use pest::iterators::Pair;
use ast::context::Context;
use ast::AstError;

pub struct Function {
    pub name: Id,
    pub return_type: Ty,
    pub args: HashMap<Id, Ty>,
    pub body: Option<Body>,
}

impl Function {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Function, AstError> {
        debug_assert!(pair.as_rule() == Rule::function_definition);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ret_type = expect!(pairs, Rule::declaration_specifiers, "declaration specifiers", span);
        let return_type = Function::return_type_from_pair(ret_type)?;
        let fn_declarator = expect!(pairs, Rule::function_declarator, "function declarator", span);
        let (name, args) = Function::name_and_args_from_pair(fn_declarator)?;

        if let Some(body) = pairs.next() {
            let body = Some(Body::from_pair(t)?);
            Ok(Function {
                name,
                args,
                return_type,
                body,
            })
        } else {
            // Just a function header
            Ok(Function {
                name,
                args,
                return_type,
                body: None
            })
        }
    }
}