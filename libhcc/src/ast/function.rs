use ast::context::Context;
use ast::declaration::Declaration;
use ast::expr::*;
use ast::id::{Id, IdStore};
use ast::statement::Body;
use ast::ty::{Ty, TyKind};
use ast::AstError;
use ast::PosSpan;
use parser::Rule;
use pest::iterators::Pair;
use std::cell::RefCell;
use std::collections::HashMap;
use visitors::typecheck::*;

pub struct Function {
    pub name: Id,
    pub return_type: Ty,
    pub intrinsic: bool,
    pub args: HashMap<Id, Declaration>,
    pub arg_order: Vec<Id>,
    pub body: Option<Body>,
    /// Id of the Struct
    pub method: Option<Id>,
    pub span: PosSpan,
    pub vtable_signature: RefCell<Option<String>>,
}

impl Function {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Function, AstError> {
        debug_assert!(
            pair.as_rule() == Rule::function_definition || pair.as_rule() == Rule::function_header
        );
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ret_type = expect!(
            pairs,
            Rule::declaration_specifiers,
            "declaration specifiers",
            span
        );
        let mut return_type = Function::return_type_from_pair(ret_type, context)?;
        let fn_declarator = expect!(
            pairs,
            Rule::function_declarator,
            "function declarator",
            span
        );
        let (name, method, args, arg_order, ptr) =
            Function::name_and_args_from_pair(fn_declarator, context)?;
        return_type.ptr = ptr;
        if let Some(body) = pairs.next() {
            let body = Some(Body::from_pair(body, context)?);
            Ok(Function {
                name,
                intrinsic: false,
                args,
                arg_order,
                return_type,
                method,
                body,
                span: PosSpan::from_span(span),
                vtable_signature: RefCell::new(None),
            })
        } else {
            // Just a function header
            Ok(Function {
                name,
                args,
                intrinsic: false,
                arg_order,
                return_type,
                body: None,
                method,
                span: PosSpan::from_span(span),
                vtable_signature: RefCell::new(None),
            })
        }
    }

    fn name_and_args_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<(Id, Option<Id>, HashMap<Id, Declaration>, Vec<Id>, usize), AstError> {
        debug_assert!(pair.as_rule() == Rule::function_declarator);
        let span = pair.as_span();

        let mut ptr = 0;
        let mut pairs = pair.into_inner().peekable();

        if let Some(ref p) = pairs.peek() {
            if p.as_rule() == Rule::pointer {
                ptr = p.as_str().matches('*').count();
            }
        }
        if ptr != 0 {
            let _ = pairs.next().unwrap();
        }

        let fn_direct_declarator = expect!(
            pairs,
            Rule::function_direct_declarator,
            "function_direct_declarator",
            span
        );

        let mut pairs = fn_direct_declarator.into_inner();
        let mut name = ident!(pairs, context.idstore, span);
        let mut method = None;
        if let Some(mut next) = pairs.next() {
            if next.as_rule() == Rule::ident {
                let id = context.idstore.get_id(next.as_str());
                method = Some(name);
                name = id;
                next = pairs
                    .next()
                    .expect("Unexpected end of tokens in name_and_args_from_pair");
            }
            let declarator_call: Pair<'r, Rule> = next;

            let mut pairs = declarator_call.into_inner();
            if let Some(parameter_list) = pairs.next() {
                let span = parameter_list.as_span();

                let pairs = parameter_list.into_inner();
                let mut params = HashMap::new();
                let mut order = vec![];

                for pair in pairs {
                    let declaration = Declaration::parameter_declaration_from_pair(pair, context)?;
                    order.push(declaration.name);
                    if params.contains_key(&declaration.name) {
                        return Err(AstError::new(
                            format!(
                                "Name '{}' is repeated.",
                                context.idstore.get_string(declaration.name).unwrap()
                            ),
                            span,
                        ));
                    }
                    params.insert(declaration.name, declaration);
                }

                Ok((name, method, params, order, ptr))
            } else {
                Ok((name, method, HashMap::new(), vec![], ptr))
            }
        } else {
            Err(AstError::new(
                "Unexpected end of tokens in name_and_args_from_pair",
                span,
            ))
        }
    }

    fn return_type_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Ty, AstError> {
        debug_assert!(pair.as_rule() == Rule::declaration_specifiers);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let type_specifier = expect!(pairs, Rule::type_specifier, "type specifier", span);
        Ok(Ty::from_pair(type_specifier, context)?)
    }

    /// Returns 0 on no match, and returns a number that corresponds to the closeness
    /// of the match if there is a match. The lower the number, the closer the match.
    pub fn conforms_to(&self, args: &mut [Expr], tc: &mut TypeChecker) -> u64 {
        if self.method.is_none() && self.arg_order.len() != args.len() {
            return 0;
        }
        let mut conformity = 1;
        for (argid, supplied) in self.arg_order.iter().zip(args.iter_mut()) {
            let arg_dec = self.args.get(argid).unwrap();
            let arg_ty = arg_dec.ty.clone();

            let sup_ty = tc.visit_expr(supplied);

            let arg_conformity = sup_ty.conforms_to_mag(arg_ty, tc);
            if arg_conformity == 0 {
                return 0;
            }
            conformity += arg_conformity;
        }
        conformity
    }

    pub fn header_equals(&self, other: &Function) -> bool {
        if self.arg_order.len() != other.arg_order.len() {
            return false;
        }
        for (a, b) in self
            .arg_order
            .as_slice()
            .iter()
            .zip(other.arg_order.as_slice().iter())
        {
            if self.args[a].ty != other.args[b].ty {
                return false;
            }
        }
        true
    }

    pub fn fn_ptr_decl(&self, fn_name: Id, idstore: &IdStore) -> String {
        let name_and_return_type = format!(
            "{} (*{})",
            self.return_type.to_code(idstore),
            idstore.get_str(fn_name)
        );
        let args = {
            let s = String::new();
            if self.arg_order.len() == 0 {
                s
            } else {
                let mut s = String::new();
                for arg in self.arg_order.iter() {
                    s.push_str(format!("{}, ", self.args[arg].ty.to_code(idstore)).as_str());
                }
                s.pop();
                s.pop();
                s
            }
        };
        format!("{}({})", name_and_return_type, args)
    }

    pub fn method_ptr_decl(&self, struct_name: Id, method_name: Id, idstore: &IdStore) -> String {
        let name_and_return_type = format!(
            "{} (*{})",
            self.return_type.to_code(idstore),
            idstore.get_str(method_name)
        );
        let args = {
            let mut s = Ty::new(TyKind::Struct(struct_name))
                .ptr_to()
                .to_code(idstore);
            if self.arg_order.len() == 0 {
                s
            } else {
                s.push_str(", ");
                let mut s = String::new();
                for arg in self.arg_order.iter() {
                    s.push_str(format!("{}, ", self.args[arg].ty.to_code(idstore)).as_str());
                }
                s.pop();
                s.pop();
                s
            }
        };
        format!("{}({})", name_and_return_type, args)
    }

    pub fn signature(&self, idstore: &IdStore) -> String {
        let mut s = String::new();
        for arg in self.arg_order.iter() {
            let ty = self.args[arg].ty.clone();
            s.push_str(ty.ptr.to_string().as_str());
            s.push_str(match self.args[arg].ty.clone().kind {
                TyKind::I0 => "i0",
                TyKind::I8 => "i8",
                TyKind::I64 => "i64",
                TyKind::Struct(name) => idstore.get_str(name),
                _ => panic!("Stop"),
            });
        }
        s
    }

    pub fn vtable_signature(&self, idstore: &IdStore) -> String {
        if let Some(s) = self.vtable_signature.replace(None) {
            self.vtable_signature.replace(Some(s.clone()));
            return s;
        }
        let s = "vtable_".to_owned() + self.signature(idstore).as_str();
        self.vtable_signature.replace(Some(s.clone()));
        s
    }
}
