use ast::id::Id;
use ast::expr::Expr;
use ast::AstError;
use ast::context::Context;
use parser::Rule;
use pest::iterators::Pair;
use ast::PosSpan;

pub struct Declarator {
    pub id: Id,
    pub ptrs: usize,
    pub span: PosSpan,
}

impl Declarator {
    pub fn struct_declarator_list_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Vec<Declarator>, AstError> {
        debug_assert!(pair.as_rule() == Rule::struct_declarator_list);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let mut decls = vec![];
        for result in pairs
            .map(|pair| Declarator::struct_declarator_from_pair(pair, context)) {
            decls.push(result?);
        }

        Ok(decls)
    }

    pub fn struct_declarator_from_pair<'r>(pair: Pair<'r, Rule>, context: &mut Context<'r>)
        -> Result<Declarator, AstError> {
        debug_assert!(pair.as_rule() == Rule::struct_declarator);
        let span = pair.as_span();
        let mut pairs = pair.into_inner();
        let declarator: Pair<'r, Rule> = expect!(pairs, Rule::declarator, "declarator", span);
        let mut pairs = declarator.into_inner();
        let pair = pairs.next();
        if let Some(pair) = pair {
            let ptrs = match pair.as_rule() {
                Rule::pointer => pair.as_str().matches('*').count(),
                _ => 0
            };
            let pair =
                if ptrs != 0 {
                    match pairs.next() {
                        Some(t) => t,
                        _ => return Err(AstError::new("", span))
                    }
                } else {
                    pair
                };

            let direct_declarator = expect!(pairs, Rule::direct_declarator, "direct declarator", span);
            let mut pairs = direct_declarator.into_inner();
            let id = ident!(pairs, context.idstore, span);

            Ok(Declarator {
                id,
                ptrs,
                span: PosSpan::from_span(span)
            })

        } else {
            Err(AstError::new("", span))
        }
    }
}