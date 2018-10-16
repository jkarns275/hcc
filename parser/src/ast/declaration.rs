use ast::id::Id;
use ast::ty::Ty;
use pest::iterators::Pair;
use ast::context::Context;
use ast::AstError;
use parser::Rule;
use ast::declarator::Declarator;
use ast::expr::Expr;

pub struct Declaration {
    pub name: Id,
    pub ty: Ty,
    pub initializer: Option<Expr>,
}

impl Declaration {
    pub fn from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Vec<Declaration>, AstError> {
        debug_assert!(pair.as_rule() == Rule::declaration);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ty = Declaration::type_from_pair(
            expect!(pairs, Rule::declaration_specifiers, "declaration specifiers", span),
            context
        )?;
        let mut declarations = vec![];
        while let Some(pair) = pairs.next() {
            if pair.as_rule() != Rule::declarator {
                return Err(AstError::new("Expected declarator", pair.as_span()))
            }
            let declarator = Declarator::from_pair(pair, context)?;
            declarator.push(Declaration {
                name: declarator.name,
                ty: ty.ptr_n_to(declarator.ptrs),
                initializer: declarator.initializer,
            });
        }
        let declarator = Declarator::from_pair(
            expect!(pairs, Rule::declarator, "declarator", span),
            context
        )?;

        Ok(declarations)
    }

    pub fn parameter_declaration_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Declaration, AstError> {
        debug_assert!(pair.as_rule() == Rule::parameter_declaration);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ty = Declaration::type_from_pair(
            expect!(pairs, Rule::declaration_specifiers, "declaration specifiers", span),
            context
        )?;
        let declarator = Declarator::from_pair(
            expect!(pairs, Rule::declarator, "declarator", span),
            context
        )?;

        Ok(Declaration {
            name: declarator.name,
            ty: ty.ptr_n_to(declarator.ptrs),
            initializer: None,
        })
    }

    fn type_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Ty, AstError> {
        debug_assert!(pair.as_rule() == Rule::declaration_specifiers);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        Ty::from_pair(expect!(pairs, Rule::type_specifier, "type specifier", span), context)
    }
}