use super::{error::LexingError, Lexer};

mod keyword;
pub(crate) use keyword::*;
mod literal;
pub(crate) use literal::*;
mod byte;
pub(crate) use byte::*;
mod ident;
pub(crate) use ident::*;
mod comb;
pub(crate) use comb::*;
mod whitespace;
pub(crate) use whitespace::*;
mod punctuation;
pub(crate) use punctuation::*;
mod op;
pub(crate) use op::*;
pub mod ty;
pub(crate) use ty::*;
pub mod comment;
pub(crate) use comment::*;
pub mod any;
pub(crate) use any::*;

/// Indicates that this struct is able to consume something that can be parsed
/// as an `Output`.
///
/// [`Muncher`]s should be ZSTs and rely on information stored in `lexer`.
/// `&mut self` is taken *only* for forwards compatibility, in the rare event
/// that I *do* need a muncher with state.
///
/// [`Muncher`]s cannot backtrack, that is: a failing muncher called n times,
/// mut fail n times. This is a lexing pass, so we should be able to determine
/// valid tokens only from the previous token. E.g. Token `A` can be followed by
/// the unambiguous token seq `{ B, C }`, but not a token set that contains
/// ambiguity.
pub(crate) trait Muncher {
    type Output;

    /// Munch some bytes from `lexer`.
    ///
    /// # Side Effects and Returns
    /// - On success, return a `Self::Output` and leave `lexer` in a state that
    ///   indicates that a `Self::Output` has been munched.
    /// - On failure, return an appropriate `LexingError`. **`lexer` mut be left
    ///   in the exact same state as it was passed t othis function**
    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError>;
}
