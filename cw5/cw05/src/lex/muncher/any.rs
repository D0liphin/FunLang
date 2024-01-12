use super::Muncher;
use crate::lex::{error::LexingError, Token, Tokv};

/// Munches any `char`
pub(crate) struct AnyMuncher;

impl Muncher for AnyMuncher {
    type Output = Token;

    fn munch(
        &mut self,
        lexer: &mut crate::lex::Lexer<'_>,
    ) -> Result<Self::Output, crate::lex::error::LexingError> {
        let Some(ch) = lexer.all_upcoming_str().chars().next() else {
            return Err(LexingError {
                index: lexer.index(),
                expected: either::Either::Right(Box::new([])),
                reason: None,
            });
        };
        let tok = Ok(Token::new(Tokv::Unknown(ch), lexer.index(), ch.len_utf8()));
        lexer.index_add_mut(ch.len_utf8());
        tok
    }
}
