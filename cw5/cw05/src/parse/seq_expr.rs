use super::error::Error as InternalParseError;
use crate::{
    ast::*,
    display_option_tok, input,
    parse::{error::Expected, *},
};
use nom::IResult;

impl Parser {
    /// Parse a sequence expression.. this
    pub fn parse_seq_expr<'b>(
        toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, Expr<'b>, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_seq_expr()");
        let (mut toks, _lbrace) = tokt(&Tokt::LBrace)(toks)?;

        let mut exprs: Vec<Expr> = Vec::new();
        let toks = loop {
            // we do want to parse a whole expression, since syntax like this
            // is valid
            // ```
            // def fn(): Void = {
            //     { do_nothing() };
            //     do_something();
            // }
            // ```
            let (newtoks, expr) = Self::parse_expr(toks)?;
            exprs.push(expr);

            // alt semicolon or rbrace, they cannot both work
            let semicolon_result = tokt(&Tokt::Semicolon)(newtoks);
            let rbrace_result = tokt(&Tokt::RBrace)(newtoks);
            if let Ok((newtoks, repr)) = semicolon_result {
                if let Ok((newtoks, _rbrace)) = tokt(&Tokt::RBrace)(newtoks) {
                    // this is actualyl slightly sneak and not really a good
                    // idea, but it should be fine??
                    // TODO: make sure this is fine
                    exprs.push(Expr::Literal(Literal { repr }));
                    break newtoks;
                }
                // this is the only time that we continue the loop, so it's the
                // only spot we have to update toks
                toks = newtoks;
                continue;
            } else if let Ok((newtoks, _rbrace)) = rbrace_result {
                break newtoks;
            // both semicolon and rbrace parsers failed, so we have some sort
            // of invalid token which we need to highlight
            } else {
                return Err(nom::Err::Failure(InternalParseError {
                    tok: Some(tokt(&Tokt::Any)(toks).expect("Tokt::Any matches all").1),
                    expected: Expected::Tokt(Tokt::Semicolon),
                    reason: Some("why are you using expressions with side effects anyway?"),
                }));
            }
            // unreachable!()
        };

        if exprs.len() == 1 {
            Ok((toks, exprs.remove(0)))
        } else {
            Ok((toks, Expr::Seq(exprs)))
        }
    }
}
