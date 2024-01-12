use crate::lex::*;

/// Munches any comment, that is the, the double-forward slash style comment
pub(crate) struct CommentMuncher;

impl Muncher for CommentMuncher {
    type Output = Token;

    fn munch(
        &mut self,
        lexer: &mut crate::lex::Lexer<'_>,
    ) -> Result<Self::Output, crate::lex::error::LexingError> {
        let bytes = lexer.all_upcoming_bytes();
        if bytes.starts_with(b"//") {
            for (i, &b) in lexer.all_upcoming_bytes()[2..].iter().enumerate() {
                if b == b'\n' {
                    let tok = Ok(Token::new(
                        Tokv::Ignore(lexer.index()),
                        lexer.index(),
                        i + 3,
                    ));
                    lexer.index_add_mut(i + 3); // /, /, \n (3)
                    return tok;
                }
            }
            lexer.index_add_mut(bytes.len());
            return Ok(Token::new(
                Tokv::Ignore(bytes.len()),
                lexer.index(),
                bytes.len(),
            ));
        }
        Err(LexingError::new(lexer.index(), TokenTy::Comment))
    }
}
