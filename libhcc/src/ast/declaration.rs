use ast::context::Context;
use ast::declarator::Declarator;
use ast::expr::*;
use ast::id::Id;
use ast::ty::Ty;
use ast::AstError;
use ast::PosSpan;
use parser::Rule;
use pest::iterators::Pair;

pub struct Declaration {
    pub name: Id,
    pub ty: Ty,
    pub initializer: Option<Expr>,
    pub span: PosSpan,
}

impl Declaration {
    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Vec<Declaration>, AstError> {
        debug_assert!(pair.as_rule() == Rule::declaration);
        let span = pair.as_span();
        let dec_span = PosSpan::from_span(pair.as_span());
        let mut pairs = pair.into_inner();
        let ty = Declaration::type_from_pair(
            expect!(
                pairs,
                Rule::declaration_specifiers,
                "declaration specifiers",
                span
            ),
            context,
        )?;
        let init_declarator_list = expect!(
            pairs,
            Rule::init_declarator_list,
            "init declarator list",
            span
        );
        let mut pairs = init_declarator_list.into_inner();
        let mut declarations = vec![];
        while let Some(pair) = pairs.next() {
            if pair.as_rule() != Rule::init_declarator {
                return Err(AstError::new("Expected init_declarator", pair.as_span()));
            }
            let mut pairs = pair.into_inner();
            let declarator = Declarator::from_pair(
                expect!(pairs, Rule::declarator, "declarator", span),
                context,
            )?;
            let initializer = if let Some(initializer) = pairs.next() {
                Some(AssignExpr::from_pair(initializer, context)?)
            } else {
                None
            };
            declarations.push(Declaration {
                name: declarator.name,
                ty: ty.clone().ptr_n_to(declarator.ptrs),
                initializer,
                span: dec_span,
            });
        }

        Ok(declarations)
    }

    pub fn parameter_declaration_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Declaration, AstError> {
        debug_assert!(pair.as_rule() == Rule::parameter_declaration);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let ty = Declaration::type_from_pair(
            expect!(
                pairs,
                Rule::declaration_specifiers,
                "declaration specifiers",
                span
            ),
            context,
        )?;
        let declarator = Declarator::from_pair(
            expect!(pairs, Rule::declarator, "declarator", span),
            context,
        )?;

        Ok(Declaration {
            name: declarator.name,
            ty: ty.ptr_n_to(declarator.ptrs),
            initializer: None,
            span: PosSpan::from_span(span),
        })
    }

    fn type_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context) -> Result<Ty, AstError> {
        debug_assert!(pair.as_rule() == Rule::declaration_specifiers);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        Ty::from_pair(
            expect!(pairs, Rule::type_specifier, "type specifier", span),
            context,
        )
    }
}
