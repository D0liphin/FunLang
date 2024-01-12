use super::{error::Error as InternalParseError, Parser, TokenStream};
use crate::{ast::*, lex::*, parse::*};
use crate::{display_option_tok, input};
use nom::{
    combinator::opt,
    sequence::{pair, tuple},
    IResult,
};

impl Parser {
    /// Parse a typed parameter declaration **EXCLUDING TRAILING PUNCTUATION**.
    ///
    /// ```plaintext
    /// def x_iter(x: Double, y: Double)
    ///            ^^^^^^^^^
    /// ```
    pub fn parse_typed_param<'b>(
        toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, (Ident<'b>, Type<'b>), InternalParseError> {
        let (toks, (ident, _colon, ty)) =
            tuple((tokt(&Tokt::Ident), tokt(&Tokt::Colon), tokt(&Tokt::Ty)))(toks)?;
        Ok((toks, (Ident { repr: ident }, Type { repr: ty })))
    }

    /// Parse a function definition, that is: the signature *and* the body.
    pub fn parse_def<'b>(
        toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, AstNode<'b>, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_def");
        let (toks, _def) = tokv(&Tokv::Keyword(KeywordToken::Def))(toks)?;
        let (mut toks, (ident, _lparen)) = tuple((tokt(&Tokt::Ident), tokt(&Tokt::LParen)))(toks)?;

        let params = if let Ok((newtoks, _rparen)) = tokt(&Tokt::RParen)(toks) {
            toks = newtoks;
            vec![]
        } else {
            // match all params
            let mut params = Vec::with_capacity(8);
            loop {
                let (newtoks, (ident, ty)) = Self::parse_typed_param(toks)?;
                toks = newtoks;
                params.push(Param { ident, ty });

                if let Ok((newtoks, _comma)) = tokt(&Tokt::Comma)(toks) {
                    toks = newtoks;
                } else {
                    // we have to fail here if there is no closing brace
                    let (newtoks, _rparen) = tokt(&Tokt::RParen)(toks)?;
                    toks = newtoks;
                    break;
                }
            }
            params
        };

        let (toks, (_colon, retrepr)) = pair(tokt(&Tokt::Colon), tokt(&Tokt::Ty))(toks)?;
        let (toks, _eq) = tokv(&Tokv::Op(OpToken::Eq))(toks)?;

        let signature = FnSignature {
            ident: Ident { repr: ident },
            params,
            ret: Type { repr: retrepr },
        };

        let (toks, body) = Self::parse_expr(toks)?;
        let (toks, _semicolon) = opt(tokt(&Tokt::Semicolon))(toks)?;

        Ok((toks, AstNode::Def { signature, body }))
    }
}
