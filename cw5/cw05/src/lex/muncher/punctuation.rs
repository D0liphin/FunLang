use crate::{lex::*, starts_with};

macro_rules! punctuation_muncher {
    ($Muncher:ident, $needle:expr => $Tokv:ident, $TokenTy:ident) => {
        pub struct $Muncher;

        impl Muncher for $Muncher {
            type Output = Token;

            fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
                let bytes = lexer.all_upcoming_bytes();
                let (tok, head) = starts_with!(bytes, {
                    $needle => Token::new(Tokv::$Tokv, lexer.index(), $needle.len()),
                    _ => {
                        return Err(LexingError::new(lexer.index(), TokenTy::$TokenTy))
                    },
                });
                lexer.index_add_mut(head.len());
                Ok(tok)
            }
        }
    };
}

punctuation_muncher!(SemicolonMuncher, b";" => Semicolon, Semicolon);
punctuation_muncher!(ColonMuncher, b":" => Colon, Colon);
punctuation_muncher!(CommaMuncher, b"," => Comma, Comma);
punctuation_muncher!(LParenMuncher, b"(" => LParen, LParen);
punctuation_muncher!(RParenMuncher, b")" => RParen, RParen);
punctuation_muncher!(LBraceMuncher, b"{" => LBrace, LBrace);
punctuation_muncher!(RBraceMuncher, b"}" => RBrace, RBrace);
