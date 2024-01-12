//! Nom parsers for all the base case expressions, as well as simple wrapper
//! expressions, such as { <expr> } and ( <expr> )

use nom::sequence::tuple;
use nom::{branch::alt, IResult};

use crate::parse::error::Error as InternalParseError;
use crate::{ast::*, display_option_tok, input, parse::*};

impl Parser {
    /// Parse a base case expression, at the moment this is just a literal or
    /// an ident
    pub fn parse_base_expr<'a>(
        toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, Expr, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_base_expr()");
        let (toks, tok) = alt((tokt(&Tokt::Literal), tokt(&Tokt::Ident)))(toks)?;
        // We can construct both literals and idents with this method
        let Ok(expr) = Expr::try_from(tok) else {
            unreachable!();
        };
        Ok((toks, expr))
    }

    /// Parse an expression in braces
    pub fn parse_braces_expr<'a>(
        toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, Expr, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_braces{{}}_expr()");
        let (toks, (_opening_brace, expr, _closing_brace)) =
            tuple((tokt(&Tokt::LBrace), Self::parse_expr, tokt(&Tokt::RBrace)))(toks)?;

        Ok((toks, expr))
    }
}
