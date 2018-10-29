use std::collections::HashMap;
use std::rc::Rc;
use ast::id::Id;
use ast::function::Function;
use ast::ty::*;
use pest::iterators::Pair;
use parser::Rule;
use ast::id::IdStore;
use ast::AstError;

#[macro_use]
use ast;
use ast::declarator::Declarator;
use ast::context::Context;
use pest::Span;
use pest::Position;
use ast::PosSpan;
use ast::declaration::Declaration;

pub struct StructField {
    pub ty: Ty,
    pub span: PosSpan,
}

impl StructField {
    pub fn new(ty: Ty, span: PosSpan) -> Self {
        StructField { ty, span }
    }
}

pub struct Structure {
    pub methods: HashMap<Id, Vec<Function>>,
    pub fields: HashMap<Id, StructField>,
    pub parent: Option<Id>,
    pub name: Id
}

impl Structure {

    pub fn field_from_pair<'r>(pair : Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Vec<(Id, StructField)>, AstError> {
        debug_assert!(pair.as_rule() == Rule::field_declaration);
        let span = pair.as_span();
        let mut pairs = pair.into_inner().peekable();
        let ty =
            if pairs.peek().is_some() {
                Ty::from_pair(expect!(pairs, Rule::type_specifier, "type specifier", span), context)?
            } else {
                return Err(AstError::new("Unexpected end of tokens.", span))
            };
        let declarator_list = expect!(pairs, Rule::struct_declarator_list, "struct declarator list", span);
        let declarators =
            Declarator::struct_declarator_list_from_pair(declarator_list, context)?;
        let mut fields = vec![];

        for d in declarators.into_iter() {
            fields.push((d.name, StructField::new(ty.clone().ptr_n_to(d.ptrs), d.span)));
        }

        Ok(fields)
    }

    pub fn fields_and_methods_from_pairs<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<(HashMap<Id, StructField>, HashMap<Id, Vec<Function>>), AstError> {
        debug_assert!(pair.as_rule() == Rule::struct_declaration_list);
        let mut pairs = pair.into_inner();
        let mut fields = HashMap::new();
        let mut methods = HashMap::new();

        while let Some(pair) = pairs.next() {
            match pair.as_rule() {
                Rule::field_declaration => {
                    let f = Structure::field_from_pair(pair, context)?;
                    for (id, field) in f {
                        fields.insert(id, field);
                    }
                },
                Rule::function_header | Rule::function_definition => {
                    let function = Function::from_pair(pair, context)?;
                    methods.entry(function.name).or_insert_with(|| vec![]).push(function);
                },
                _ => {}
            }
        }
        // TODO: Make sure there are no duplicate field names, or conflicting function types.

        Ok((fields, methods))
    }
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>) -> Result<Self, AstError> {
        debug_assert!(pair.as_rule() == Rule::struct_or_union_spec);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let _ = expect!(pairs, Rule::struct_kw, "struct keyword", span);
        let name = ident!(pairs, context.idstore, span);

        Ok(if let Some(r) = pairs.next() {
            let name = ident!(pairs, context.idstore, span);
            let mut next = pairs.next();
            let parent = if let Some(next_ref) = next.as_ref() {
                match next_ref.as_rule() {
                    Rule::struct_or_union => {
                        let parent_name = ident!(pairs, context.idstore, span);
                        Some(parent_name)
                    },
                    _ => None,
                }
            } else { panic!("This shouldnt happen. ") };
            if parent.is_some() {
                next = pairs.next();
            }
            let declarations = next.expect("Unexpected end of tokens while parsing struct.");
            let (fields, mut methods)
                = Structure::fields_and_methods_from_pairs(declarations, context)?;
            for (_name, method_list) in methods.iter_mut() {
                for method in method_list.iter_mut() {
                    method.arg_order.insert(0, context.idstore.get_id("this"));
                    method.args.insert(context.idstore.get_id("this"), Declaration {
                        name: context.idstore.get_id("this"),
                        ty: Ty { kind: TyKind::Struct(name), ptr: 1 },
                        initializer: None,
                    });
                }
            }
            Structure { methods, fields, parent, name }
        } else {
            Structure {
                methods: HashMap::new(),
                fields: HashMap::new(),
                parent: None,
                name,
            }
        })
    }
}

impl PartialEq for Structure {

    fn eq(&self, other: &Structure) -> bool {
        // TODO: Ensure that this is an okay way to check!
        self.name == other.name
    }
}

impl Eq for Structure {}