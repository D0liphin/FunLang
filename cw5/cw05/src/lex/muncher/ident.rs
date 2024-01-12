use crate::lex::*;

/// Munches an `Ident`, I actually allow utf8 identifiers. For example, the
/// below would be valid if strings could be assigned:
///
/// ```plaintext
/// val 挨拶 = "こんにちは世界";
/// ```
pub struct IdentMuncher;

fn as_ascii(ch: char) -> Option<u8> {
    if ch.is_ascii() {
        Some(ch as u8)
    } else {
        None
    }
}

fn invalid_bchar(b: u8) -> bool {
    b.is_ascii_whitespace() || b";{}!@#$%^&*()-=+\\|'\"/?:>.<,`".contains(&b)
}

fn invalid_char(ch: char) -> bool {
    let Some(b) = as_ascii(ch) else {
        return false;
    };
    invalid_bchar(b)
}

impl Muncher for IdentMuncher {
    type Output = Token;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        let mut chars = lexer.all_upcoming_str().char_indices();
        // Make sure this starts with a valid char
        if let Some((_, c)) = chars.next() {
            if invalid_char(c) {
                return Err(LexingError::new(lexer.index(), TokenTy::Ident));
            }
            if c.is_ascii_digit() {
                return Err(LexingError::new(lexer.index(), TokenTy::Ident)
                    .with_reason("identifiers cannot start with a digit"));
            }
        } else {
            return Err(LexingError::new(lexer.index(), TokenTy::Ident));
        }

        // parse ALL utf8 chars until an ascii whitespace
        let mut end_offset = lexer.all_upcoming_bytes().len();
        for (i, &c) in lexer.all_upcoming_bytes().iter().enumerate() {
            if invalid_bchar(c) {
                end_offset = i;
                break;
            }
        }
        let tok = Ok(Token::new(
            Tokv::Ident(String::from(lexer.upcoming_str(end_offset))),
            lexer.index(),
            end_offset,
        ));
        lexer.index_add_mut(end_offset);
        return tok;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_idents() {
        let mut lexer = Lexer::new("挨拶");
        let tok = IdentMuncher.munch(&mut lexer);
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok, Token::new(Tokv::Ident("挨拶".into()), 0, "挨拶".len()));
        assert_eq!(lexer.this_bchar(), None);

        let mut lexer = Lexer::new("1挨拶");
        let tok = IdentMuncher.munch(&mut lexer);
        assert!(tok.is_err());
        assert_eq!(lexer.this_bchar(), Some('1'));

        let mut lexer = Lexer::new("挨拶{");
        let tok = IdentMuncher.munch(&mut lexer);
        assert!(tok.is_ok());
        let tok = tok.unwrap();
        assert_eq!(tok, Token::new(Tokv::Ident("挨拶".into()), 0, "挨拶".len()));
        assert_eq!(lexer.this_bchar(), Some('{'));
    }
}
