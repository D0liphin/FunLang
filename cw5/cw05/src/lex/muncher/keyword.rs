use crate::{lex::*, starts_with};

/// Munches a single [`KeywordToken`]`
pub(crate) struct KeywordMuncher;

pub(crate) const KEYWORD_DEF: &'static [u8] = b"def";
pub(crate) const KEYWORD_VAL: &'static [u8] = b"val";
pub(crate) const KEYWORD_IF: &'static [u8] = b"if";
pub(crate) const KEYWORD_THEN: &'static [u8] = b"then";
pub(crate) const KEYWORD_ELSE: &'static [u8] = b"else";
pub(crate) const KEYWORD_WRITE: &'static [u8] = b"write";

impl Muncher for KeywordMuncher {
    type Output = Token;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        let bytes = lexer.all_upcoming_bytes();
        let (tok, tokstr) = starts_with!(bytes, {
            KEYWORD_DEF => KeywordToken::Def,
            KEYWORD_VAL => KeywordToken::Val,
            KEYWORD_IF => KeywordToken::If,
            KEYWORD_THEN => KeywordToken::Then,
            KEYWORD_ELSE => KeywordToken::Else,
            KEYWORD_WRITE => KeywordToken::Write,
            _ => {
                return Err(LexingError::new(lexer.index(), TokenTy::Keyword))
            },
        });

        let result = Ok(Token::new(tok, lexer.index(), tokstr.len()));
        lexer.index_add_mut(tokstr.len());

        match tok {
            KeywordToken::Write => {
                let index = lexer.index();
                // lookahead for lparen
                _ = Hungry(LParenMuncher).munch(lexer)?;
                lexer.index.set(index);
            }
            // now we need at least one whitespace after every keyword, so that
            // we don't clash with identifiers
            _ => match WhitespaceMuncher(1).munch(lexer) {
                Ok(..) => {}
                Err(e) => {
                    lexer.index_sub_mut(tokstr.len());
                    return Err(e);
                }
            },
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn munches_keywords() {
        let mut lexer = Lexer::new("if");
        let kw = KeywordMuncher.munch(&mut lexer);
        assert!(kw.is_ok());
        let kw = kw.unwrap();
        assert_eq!(kw.variant, Tokv::Keyword(KeywordToken::If));
    }
}
