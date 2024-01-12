use crate::parse::error::Error as InternalParseError;
use crate::{ast::*, lex::*, parse::*};
use crate::{display_option_tok, input};
use nom::branch::alt;
use nom::IResult;

impl Parser {
    /// parse args up to a closing brace
    fn parse_args<'b>(
        mut toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, Vec<Expr<'b>>, InternalParseError> {
        if let Ok((toks, _rparen)) = tokt(&Tokt::RParen)(toks) {
            return Ok((toks, vec![]));
        }

        // we must have parameters
        let mut params = Vec::new();
        loop {
            let (newtoks, param) = Self::parse_expr(toks)?;
            params.push(param);

            if let Ok((newtoks, _comma)) = tokt(&Tokt::Comma)(newtoks) {
                toks = newtoks;
            } else {
                // we have to fail here if there is no closing brace
                let (newtoks, _rparen) = tokt(&Tokt::RParen)(newtoks)?;
                toks = newtoks;
                break;
            }
        }
        Ok((toks, params))
    }

    /// Parse an explicit call that is not an operator, for example
    ///
    /// ```plaintext
    /// foo -> Ident
    /// bar() -> ExplicitCall
    /// baz(arg, arg) -> ExplicitCall
    /// ```
    pub fn parse_explicit_call_expr<'b>(
        toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, Expr<'b>, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_explicit_call_expr()");

        let (toks, fn_name) = alt((
            tokt(&Tokt::Ident),
            tokv(&Tokv::Keyword(KeywordToken::Write)),
        ))(toks)?;

        // if we have a paren, get args
        if let Ok((toks, _lparen)) = tokt(&Tokt::LParen)(toks) {
            let (toks, params) = Self::parse_args(toks)?;
            // disambiguate `write` from user-defined functions
            let callable_id = if fn_name == &Tokv::Keyword(KeywordToken::Write) {
                CallableId::Write
            } else {
                CallableId::Ident
            };
            Ok((
                toks,
                Expr::ExplicitCall {
                    callable: Callable {
                        id: callable_id,
                        repr: fn_name,
                    },
                    params,
                },
            ))
        } else {
            if fn_name == &Tokv::Keyword(KeywordToken::Write) {
                Ok((
                    toks,
                    Expr::ExplicitCall {
                        callable: Callable {
                            id: CallableId::Write,
                            repr: fn_name,
                        },
                        params: vec![],
                    },
                ))
            } else {
                Ok((toks, Expr::Ident(Ident { repr: fn_name })))
            }
        }
    }
}
