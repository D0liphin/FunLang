use super::{Parser, TokenStream};
use crate::parse::error::Error as InternalParseError;
use crate::{ast::*, lex::*, parse::*};
use crate::{display_option_tok, input};
use nom::{sequence::tuple, IResult};

impl Parser {
    /// Construct a new `AstNode::Const` from some tokens
    ///
    /// # Panics
    /// If any of the provided tokens cannot be cast to the coorect type
    fn new_const<'a>(ident: &'a Token, ty: &'a Token, literal: &'a Token) -> AstNode<'a> {
        let Tokv::Literal(..) = &literal.variant else {
            panic!("literal must be a Tokv::Literal");
        };

        AstNode::Const {
            ident: Ident { repr: ident },
            ty: Type { repr: ty },
            value: Literal { repr: literal },
        }
    }

    /// nom const_decl parser
    ///
    /// # Returns
    /// an `AstNode::Const`
    pub fn parse_const_decl<'a>(
        toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, AstNode<'a>, InternalParseError<'a, 'a>> {
        display_option_tok!(toks.0.first());
        input!("parse_const_decl");

        let (toks, _val) = tokv(&Tokv::Keyword(KeywordToken::Val))(toks)?;
        let (toks, (ident, _colon, ty)) =
            tuple((tokt(&Tokt::Ident), tokt(&Tokt::Colon), tokt(&Tokt::Ty)))(toks)?;
        let (toks, _eq) = tokv(&Tokv::Op(OpToken::Eq))(toks)?;
        let (toks, (literal, _semicolon)) =
            tuple((tokt(&Tokt::Literal), tokt(&Tokt::Semicolon)))(toks)?;

        Ok((toks, Self::new_const(ident, ty, literal)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_const_decl() {
        let ident = String::from("MyVal");
        let ty = String::from("Int");
        let lit = LiteralToken::Int(CharInt::Int(5));
        let plaintext = format!("val {ident}: {ty} = {lit};");

        let toks = Lexer::new(&plaintext)
            .into_token_stream(true)
            .expect("valid syntax");

        let (_, node) = Parser::parse_const_decl(TokenStream(&toks)).expect("valid const decl");

        assert_eq!(
            &node,
            &AstNode::Const {
                ident: Ident {
                    repr: &Token {
                        variant: Tokv::Ident(ident),
                        index: 4,
                        len: 5,
                    },
                },
                ty: Type {
                    repr: &Token {
                        variant: Tokv::Ty(ty),
                        index: 11,
                        len: 3,
                    },
                },
                value: Literal {
                    repr: &Token {
                        variant: Tokv::Literal(lit),
                        index: 17,
                        len: 1,
                    },
                },
            }
        );
    }
}
