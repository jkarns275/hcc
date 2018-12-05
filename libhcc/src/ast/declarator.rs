use ast::context::Context;
use ast::id::Id;
use ast::AstError;
use ast::PosSpan;
use parser::Rule;
use pest::iterators::Pair;

pub struct Declarator {
    pub name: Id,
    pub ptrs: usize,
    pub span: PosSpan,
}

impl Declarator {
    pub fn struct_declarator_list_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Vec<Declarator>, AstError> {
        debug_assert!(pair.as_rule() == Rule::struct_declarator_list);
        let pairs = pair.into_inner();
        let mut decls = vec![];
        for result in pairs.map(|pair| Declarator::struct_declarator_from_pair(pair, context)) {
            decls.push(result?);
        }

        Ok(decls)
    }

    pub fn from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Declarator, AstError> {
        debug_assert!(pair.as_rule() == Rule::declarator);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let pair = pairs.next();
        if let Some(pair) = pair {
            let ptrs = match pair.as_rule() {
                Rule::pointer => pair.as_str().matches('*').count(),
                _ => 0,
            };
            let direct_declarator = if ptrs != 0 {
                match pairs.next() {
                    Some(t) => t,
                    _ => return Err(AstError::new("", span)),
                }
            } else {
                pair
            };
            let mut pairs = direct_declarator.into_inner();
            let name = ident!(pairs, context.idstore, span);

            Ok(Declarator {
                name,
                ptrs,
                span: PosSpan::from_span(span),
            })
        } else {
            Err(AstError::new("", span))
        }
    }

    fn struct_declarator_from_pair<'r>(
        pair: Pair<'r, Rule>,
        context: &mut Context,
    ) -> Result<Declarator, AstError> {
        debug_assert!(pair.as_rule() == Rule::struct_declarator);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let declarator: Pair<'r, Rule> = expect!(pairs, Rule::declarator, "declarator", span);
        Declarator::from_pair(declarator, context)
    }
}
