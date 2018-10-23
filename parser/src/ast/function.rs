use ast::ty::Ty;
use ast::id::Id;
use std::collections::HashMap;
use ast::statement::Body;
use parser::Rule;
use pest::iterators::Pair;
use ast::context::Context;
use ast::AstError;
use ast::declaration::Declaration;

pub struct Function {
    pub name: Id,
    pub return_type: Ty,
    pub args: HashMap<Id, Declaration>,
    pub body: Option<Body>,
    /// Id of the Struct
    pub method: Option<Id>
}

impl Function {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Function, AstError> {
        debug_assert!(pair.as_rule() == Rule::function_definition);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ret_type = expect!(pairs, Rule::declaration_specifiers, "declaration specifiers", span);
        let return_type = Function::return_type_from_pair(ret_type, context)?;
        let fn_declarator = expect!(pairs, Rule::function_declarator, "function declarator", span);
        let (name, method, args) = Function::name_and_args_from_pair(fn_declarator, context)?;

        if let Some(body) = pairs.next() {
            let body = Some(Body::from_pair(body, context)?);
            Ok(Function {
                name,
                args,
                return_type,
                method,
                body,
            })
        } else {
            // Just a function header
            Ok(Function {
                name,
                args,
                return_type,
                body: None,
                method,
            })
        }
    }

    fn name_and_args_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<(Id, Option<Id>, HashMap<Id, Declaration>), AstError> {
        debug_assert!(pair.as_rule() == Rule::function_declarator);
        let span = pair.as_span();

        let mut pairs = pair.into_inner();
        let fn_direct_declarator = expect!(pairs, Rule::function_direct_declarator, "function_direct_declarator", span);

        let mut pairs = fn_direct_declarator.into_inner();
        let mut name = ident!(pairs, context.idstore, span);
        let mut method = None;
        if let Some(mut next) = pairs.next() {
            if next.as_rule() == Rule::ident {
                let id = context.idstore.get_id(next.as_str());
                method = Some(name);
                name = id;
                next = pairs.next().expect("Unexpected end of tokens.");
            }
            let declarator_call: Pair<'r, Rule> = next;
            let span = declarator_call.as_span();

            let mut pairs = declarator_call.into_inner();
            let parameter_list: Pair<'r, Rule> = expect!(pairs, Rule::parameter_list, "parameter_list", span);
            let span = parameter_list.as_span();

            let mut pairs = parameter_list.into_inner();
            let mut params = HashMap::new();

            for pair in pairs {
                let declaration = Declaration::parameter_declaration_from_pair(pair, context)?;
                params.insert(declaration.name, declaration);
            }

            Ok((name, method, params))
        } else {
            Err(AstError::new("Unexpected end of tokens.", span))
        }
    }

    fn return_type_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Ty, AstError> {
        debug_assert!(pair.as_rule() == Rule::declaration_specifiers);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let type_specifier = expect!(pairs, Rule::declaration_specifiers, "declaration specifier", span);
        Ok(Ty::from_pair(type_specifier, context)?)
    }
}