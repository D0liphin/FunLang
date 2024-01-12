use crate::lex::*;

#[derive(Debug)]
pub struct ByteMuncher(pub(crate) u8);

impl Muncher for ByteMuncher {
    type Output = u8;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        match lexer.all_upcoming_bytes().first() {
            Some(&b) if b == self.0 => {
                lexer.index_add_mut(1);
                Ok(b)
            }
            _ => Err(LexingError::new(
                lexer.index(),
                TokenTy::Char(self.0 as char),
            )),
        }
    }
}
