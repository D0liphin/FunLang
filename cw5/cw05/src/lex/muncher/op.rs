use crate::{lex::*, starts_with};

/// Match any operator
pub(crate) struct OpMuncher;

pub(crate) const OP_DBLEQ: &'static [u8] = b"==";
pub(crate) const OP_EQ: &'static [u8] = b"=";
pub(crate) const OP_NEQ: &'static [u8] = b"!=";
pub(crate) const OP_LT: &'static [u8] = b"<";
pub(crate) const OP_GT: &'static [u8] = b">";
pub(crate) const OP_LEQ: &'static [u8] = b"<=";
pub(crate) const OP_GEQ: &'static [u8] = b">=";
pub(crate) const OP_ADD: &'static [u8] = b"+";
pub(crate) const OP_SUB: &'static [u8] = b"-";
pub(crate) const OP_MUL: &'static [u8] = b"*";
pub(crate) const OP_DIV: &'static [u8] = b"/";
pub(crate) const OP_MOD: &'static [u8] = b"%";

impl Muncher for OpMuncher {
    type Output = Token;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        let bytes = lexer.all_upcoming_bytes();
        let (tok, tokstr) = starts_with!(bytes, {
            OP_DBLEQ => OpToken::DblEq,
            OP_EQ => OpToken::Eq,
            OP_NEQ => OpToken::Neq,
            // these must come before...
            OP_LEQ => OpToken::Leq,
            OP_GEQ => OpToken::Geq,
            // these
            OP_LT => OpToken::Lt,
            OP_GT => OpToken::Gt,
            OP_ADD => OpToken::Add,
            OP_SUB => OpToken::Sub,
            OP_MOD => OpToken::Mod,
            OP_MUL => OpToken::Mul,
            OP_DIV => OpToken::Div,
            _ => {
                return Err(LexingError::new(lexer.index(), TokenTy::Op))
            },
        });
        let tok = Ok(Token::new(tok, lexer.index(), tokstr.len()));
        lexer.index_add_mut(tokstr.len());

        tok
    }
}
