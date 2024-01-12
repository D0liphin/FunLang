use std::mem;

use crate::{lex::*, starts_with, try_all_munchers};

/// Munches a single function item
pub(crate) struct LiteralMuncher;

pub(crate) const KEYWORD_TRUE: &'static [u8] = b"true";
pub(crate) const KEYWORD_FALSE: &'static [u8] = b"false";

impl LiteralMuncher {
    /// Munch a string literal, starting at one after the first quotation
    fn munch_string(lexer: &mut Lexer<'_>) -> Result<<Self as Muncher>::Output, LexingError> {
        let mut escaped = false;
        let mut result = vec![];
        ByteMuncher(b'"').munch(lexer)?;
        for (len, &ch) in lexer.all_upcoming_bytes().iter().enumerate() {
            if escaped {
                match ch {
                    b'\\' => result.push(b'\\'),
                    b'n' => result.push(b'\n'),
                    b'r' => result.push(b'\r'),
                    b'"' => result.push(b'"'),
                    _ => {
                        return Err(LexingError::new(lexer.index() + len + 1, TokenTy::Literal)
                            .with_reason("invalid escape sequence"))
                    }
                }
                escaped = false;
                continue;
            } else if ch == b'\\' {
                escaped = true;
                continue;
            } else if ch == b'"' {
                let tok = LiteralToken::String(String::from_utf8(result).expect("valid utf8"));
                let tok = Ok(Token::new(tok, lexer.index() - 1, len + 2));
                lexer.index_add_mut(len + 1);
                return tok;
            }
            result.push(ch);
        }

        lexer.index_sub_mut(1);
        Err(LexingError::new(lexer.index(), TokenTy::Literal)
            .with_reason("unterminated string literal"))
    }

    /// Munch a number literal, either a `Double` or an `Int`
    fn munch_number(lexer: &mut Lexer<'_>) -> Result<<Self as Muncher>::Output, LexingError> {
        let is_negative = ByteMuncher(b'-').munch(lexer).is_ok();
        let mut is_double = false;
        let mut len = lexer.all_upcoming_bytes().len();
        for (i, &b) in lexer.all_upcoming_bytes().iter().enumerate() {
            match b {
                d if d.is_ascii_digit() || d == b'_' => {}
                b'.' => {
                    if is_double {
                        return Err(LexingError::new(lexer.index() + i, TokenTy::Literal)
                            .with_reason("repeated decimal point in number literal"));
                    }
                    is_double = true;
                }
                _ => {
                    len = i;
                    break;
                }
            }
        }

        if len == 0 {
            return Err(LexingError::new(lexer.index(), TokenTy::Literal));
        }

        // we need to include the - case
        if is_negative {
            lexer.index_sub_mut(1);
            len += 1;
        }
        let s = String::from_utf8(
            lexer
                .upcoming_bytes(len)
                .iter()
                .filter_map(|&b| if b != b'_' { Some(b) } else { None })
                .collect::<Vec<u8>>(),
        )
        .expect("valid utf8");

        let tok = if is_double {
            let Ok(n) = s.parse::<f64>() else {
                return Err(
                    LexingError::new(lexer.index(), TokenTy::Literal).with_reason(concat![
                        "not a valid floating point literal -- might be overflowing"
                    ]),
                );
            };
            LiteralToken::Double(n)
        } else {
            let Ok(n) = s.parse::<i64>() else {
                return Err(
                    LexingError::new(lexer.index(), TokenTy::Literal).with_reason(concat![
                        "not a valid integer literal -- might be overflowing"
                    ]),
                );
            };
            LiteralToken::Int(CharInt::Int(n))
        };
        let tok = Ok(Token::new(tok, lexer.index(), len));
        lexer.index_add_mut(len);
        tok
    }

    /// Muncha boolean literal, either a `true` or a `false`
    fn munch_bool(lexer: &mut Lexer<'_>) -> Result<<Self as Muncher>::Output, LexingError> {
        let bytes = lexer.all_upcoming_bytes();
        let (tok, s) = starts_with!(bytes, {
            KEYWORD_TRUE => LiteralToken::Bool(true),
            KEYWORD_FALSE => LiteralToken::Bool(false),
            _ => {
                return Err(LexingError::new(lexer.index(), TokenTy::Literal))
            }
        });
        let tok = Ok(Token::new(tok, lexer.index(), s.len()));
        lexer.index_add_mut(s.len());
        tok
    }

    /// Munch a char literal
    fn munch_char(lexer: &mut Lexer<'_>) -> Result<<Self as Muncher>::Output, LexingError> {
        ByteMuncher(b'\'').munch(lexer)?;

        // first match the special cases '\' and '\\'. We don't care about
        // utf8 sequences or something... that would be annoying!
        let escaped = match lexer.upcoming_bytes(2) {
            br#"\n"# => Some('\n'),
            br#"\\"# => Some('\\'),
            br#"\'"# => Some('\''),
            _ => None,
        };
        if let Some(c) = escaped {
            let tok = Ok(Token::new(
                LiteralToken::Int(CharInt::Char(c)),
                lexer.index() - 1,
                4,
            ));
            lexer.index_add_mut(3); // \, ?, ' (3 total)
            return tok;
        }

        // grab bytes until we hit '. Then, convert the byte slice into a str.
        // this should always be a valid str... but might contain lots of small
        // characters.
        for (count, &b) in lexer
            .upcoming_bytes(mem::size_of::<char>())
            .iter()
            .enumerate()
        {
            if b == b'\'' {
                if count == 0 {
                    return Err(LexingError::new(lexer.index() + 1, TokenTy::Literal)
                        .with_reason("empty chars are invalid... maybe use a string?"));
                }

                let charbuf = lexer.upcoming_bytes(count);
                lexer.index_sub_mut(1);
                let mut chars = std::str::from_utf8(charbuf)
                    .map_err(|_| LexingError::new(lexer.index(), TokenTy::Literal))?
                    .chars();
                let c = chars.next().expect("valid str where len >= 1");

                // we still need to invalidate any char that is a string, we
                // could just take the first one, but I think that's
                // uninintuitive
                if chars.next().is_some() {
                    return Err(LexingError::new(lexer.index(), TokenTy::Literal)
                        .with_reason("expected one utf8 character, found several"));
                }

                let tok = Ok(Token::new(
                    LiteralToken::Int(CharInt::Char(c)),
                    lexer.index(),
                    count + 2,
                ));
                lexer.index_add_mut(count + 2); // for both quotes
                return tok;
            }
        }

        lexer.index_sub_mut(1);
        // we consumed 5 bytes without hitting a '
        Err(LexingError::new(lexer.index(), TokenTy::Literal)
            .with_reason("unterminated or string-like char literal... maybe use a string?"))
    }

    /// Munch a void literal
    fn munch_void(lexer: &mut Lexer<'_>) -> Result<<Self as Muncher>::Output, LexingError> {
        if lexer.upcoming_bytes(3) == b"..." {
            let tok = Ok(Token::new(LiteralToken::Void, lexer.index(), 3));
            lexer.index_add_mut(3);
            tok
        } else {
            Err(LexingError::new(lexer.index(), TokenTy::Literal))
        }
    }
}

impl Muncher for LiteralMuncher {
    type Output = Token;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        try_all_munchers!(
            lexer,
            [
                Self::munch_string,
                Self::munch_bool,
                Self::munch_char,
                Self::munch_number,
                Self::munch_void,
            ]
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn munches_ints() {
        let mut lexer = Lexer::new("100");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::Int(CharInt::Int(100)))
        );

        let mut lexer = Lexer::new("1");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(n.variant, Tokv::Literal(LiteralToken::Int(CharInt::Int(1))));

        let mut lexer = Lexer::new("100_000)");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::Int(CharInt::Int(100_000)))
        );

        let mut lexer = Lexer::new("-100");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::Int(CharInt::Int(-100)))
        );
    }

    #[test]
    fn munches_doubles() {
        let mut lexer = Lexer::new("100.");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(n.variant, Tokv::Literal(LiteralToken::Double(100.)));

        let mut lexer = Lexer::new("1.5");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(n.variant, Tokv::Literal(LiteralToken::Double(1.5)));

        let mut lexer = Lexer::new("123_456.789)");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(n.variant, Tokv::Literal(LiteralToken::Double(123_456.789)));
    }

    #[test]
    fn munches_strings() {
        let mut lexer = Lexer::new(r#""valid string""#);
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::String("valid string".into()))
        );

        let mut lexer = Lexer::new(r#""\"my \\ quote\"")"#);
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::String("\"my \\ quote\"".into()))
        );
    }

    #[test]
    fn munches_bools() {
        let mut lexer = Lexer::new("true)");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(n.variant, Tokv::Literal(LiteralToken::Bool(true)));

        let mut lexer = Lexer::new("false");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(n.variant, Tokv::Literal(LiteralToken::Bool(false)));
    }

    #[test]
    fn munches_chars() {
        let mut lexer = Lexer::new("'A'");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::Int(CharInt::Char('A' as _)))
        );

        let mut lexer = Lexer::new("'あ'");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_ok());
        let n = n.unwrap();
        assert_eq!(
            n.variant,
            Tokv::Literal(LiteralToken::Int(CharInt::Char('あ' as _)))
        );

        let mut lexer = Lexer::new("'あa'");
        let n = LiteralMuncher.munch(&mut lexer);
        assert!(n.is_err());
    }
}
