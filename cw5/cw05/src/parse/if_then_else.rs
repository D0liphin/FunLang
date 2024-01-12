use super::{error::Error as InternalParseError, Parser, TokenStream};
use crate::{ast::Expr, display_option_tok, input, lex::*, parse::*};
use nom::IResult;

impl Parser {
    /// Parse a function definition, that is: the signature *and* the body.
    pub fn parse_if_then_else<'b>(
        toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, Expr<'b>, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_if_then_else()");
        let (toks, condition_repr) = tokv(&Tokv::Keyword(KeywordToken::If))(toks)?;
        let (toks, condition) = Self::parse_expr(toks)?;
        let (toks, then_repr) = tokv(&Tokv::Keyword(KeywordToken::Then))(toks)?;
        let (toks, then_expr) = Self::parse_expr(toks)?;
        let (toks, else_repr) = tokv(&Tokv::Keyword(KeywordToken::Else))(toks)?;
        let (toks, else_expr) = Self::parse_expr(toks)?;

        Ok((
            toks,
            Expr::IfThenElse {
                condition_repr,
                then_repr,
                else_repr,
                condition: Box::new(condition),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            },
        ))
    }
}
