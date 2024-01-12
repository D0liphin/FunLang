use either::Either;

use crate::lex::*;

/// Munches a single type, currently this is exactly the same as an
/// `IdentMuncher` and relies on internal implementation details of
/// `IdentMuncher`.
///
/// # TODO
/// Separate this from `IdentMuncher`
pub(crate) struct TyMuncher;

impl Muncher for TyMuncher {
    type Output = Token;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        let mut ident = IdentMuncher.munch(lexer).map_err(|mut e| {
            e.expected = Either::Left(TokenTy::Ty);
            if e.reason.is_some() {
                e.with_reason("invalid type name")
            } else {
                e
            }
        })?;
        if let Tokv::Ident(s) = ident.variant {
            ident.variant = Tokv::Ty(s);
        };
        Ok(ident)
    }
}
