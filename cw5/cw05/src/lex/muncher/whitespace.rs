use crate::lex::*;

/// [`Muncher`] ZST for munching whitespace, outputs the amount of whitespace
/// it matched. It must match at least `.0` whitespace
pub struct WhitespaceMuncher(pub(crate) usize);

impl Muncher for WhitespaceMuncher {
    type Output = Token;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        let mut len = lexer.all_upcoming_bytes().len();
        for (i, &b) in lexer.all_upcoming_bytes().iter().enumerate() {
            if !b.is_ascii_whitespace() {
                len = i;
                break;
            }
        }

        if len >= self.0 {
            let index = lexer.index();
            lexer.index_add_mut(len);
            Ok(Token::new(Tokv::Ignore(len), index, len))
        } else {
            Err(LexingError::new(lexer.index(), TokenTy::Whitespace))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lex::{
        muncher::{Muncher, WhitespaceMuncher},
        Lexer, Tokv,
    };

    #[test]
    fn lexes_whitespace() {
        let plaintext = "?    ?";
        assert_eq!(plaintext.len(), 6);
        let mut lexer = Lexer::new(&plaintext);
        // _ = ByteMuncher(b'?').munch(&mut lexer);
        let result = WhitespaceMuncher(1).munch(&mut lexer).unwrap();
        assert_eq!(result, Tokv::Ignore(0));
        assert_eq!(lexer.index(), 0);
    }
}
