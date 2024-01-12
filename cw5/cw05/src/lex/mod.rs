pub mod error;
use std::cell::Cell;

use error::*;
mod token;
pub use token::*;
mod muncher;
use muncher::*;
mod macros;

use crate::any_muncher;

pub struct Lexer<'a> {
    /// The plaintext we are lexing
    plaintext: &'a str,
    /// The current index our lexer is at
    index: Cell<usize>,
}

impl<'a> Lexer<'a> {
    /// Construct a new [`Lexer`]
    pub fn new(plaintext: &'a str) -> Self {
        Self {
            plaintext: plaintext.trim_end(),
            index: Cell::new(0),
        }
    }

    /// Returns `true` if the `Lexer` has consumed the entire `plaintext`.
    ///
    /// # Panics
    /// If `index` is greater than `self.plaintext.len()`
    pub fn is_exhausted(&self) -> bool {
        if self.index() > self.plaintext.len() {
            panic!("invalid index, check munchers!");
        }
        self.index() == self.plaintext.len()
    }

    /// Consume this [`Lexer`], producing a [`Vec<Token>`]
    pub fn into_token_stream(mut self, strict: bool) -> Result<Vec<Token>, LexingError> {
        let mut tokens: Vec<Token> = Vec::with_capacity(256);

        while !self.is_exhausted() {
            // println!(
            //     "{}{:?}",
            //     tokens.len(),
            //     tokens
            //         .get(tokens.len().checked_sub(5).unwrap_or(0)..)
            //         .unwrap_or(&tokens)
            //         .iter()
            //         .map(|token| &token.variant)
            //         .collect::<Vec<_>>()
            // );
            match tokens.last() {
                None => any_muncher!(
                    &mut self,
                    tokens,
                    strict,
                    [
                        Hungry(LBraceMuncher),
                        Hungry(RBraceMuncher),
                        Hungry(LParenMuncher),
                        Hungry(RParenMuncher),
                        Hungry(CommentMuncher),
                        Hungry(KeywordMuncher),
                        Hungry(IdentMuncher),
                        Hungry(LBraceMuncher),
                        WhitespaceMuncher(1),
                    ],
                ),
                Some(tok) => match tok.variant {
                    Tokv::Ignore(_) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(LBraceMuncher),
                                Hungry(RBraceMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(IdentMuncher),
                                Hungry(CommentMuncher),
                                Hungry(SemicolonMuncher),
                                Hungry(LiteralMuncher),
                            ]
                        )
                    }
                    Tokv::Semicolon | Tokv::LBrace => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(LParenMuncher),
                                Hungry(LBraceMuncher),
                                Hungry(RBraceMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(IdentMuncher),
                                Hungry(CommentMuncher),
                                Hungry(LiteralMuncher),
                            ]
                        )
                    }
                    // after a `write` we only allow (
                    Tokv::Keyword(KeywordToken::Write) => {
                        any_muncher!(&mut self, tokens, strict, [Hungry(LParenMuncher)])
                    }
                    Tokv::Keyword(_) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(KeywordMuncher),
                                Hungry(LParenMuncher),
                                Hungry(RParenMuncher),
                                Hungry(LBraceMuncher),
                                Hungry(LiteralMuncher),
                                Hungry(CommentMuncher),
                                Hungry(LParenMuncher),
                                Hungry(IdentMuncher),
                            ]
                        )
                    }
                    Tokv::Ident(_) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(RBraceMuncher),
                                Hungry(ColonMuncher),
                                Hungry(CommentMuncher),
                                Hungry(OpMuncher),
                                Hungry(LParenMuncher),
                                Hungry(RParenMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(CommaMuncher),
                                Hungry(SemicolonMuncher),
                            ]
                        )
                    }
                    Tokv::Colon => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(TyMuncher),
                                WhitespaceMuncher(1),
                                Hungry(CommentMuncher),
                            ]
                        )
                    }
                    Tokv::Ty(_) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(OpMuncher),
                                Hungry(CommaMuncher),
                                Hungry(RParenMuncher),
                                Hungry(CommentMuncher),
                            ]
                        )
                    }
                    Tokv::Op(OpToken::Eq) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(LBraceMuncher),
                                Hungry(LParenMuncher),
                                Hungry(LiteralMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(IdentMuncher),
                                Hungry(CommentMuncher),
                                WhitespaceMuncher(1),
                            ]
                        )
                    }
                    Tokv::Op(_) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(LParenMuncher),
                                Hungry(OpMuncher),
                                Hungry(LiteralMuncher),
                                Hungry(IdentMuncher),
                                Hungry(CommentMuncher),
                            ]
                        )
                    }
                    Tokv::Literal(_) => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(KeywordMuncher),
                                Hungry(SemicolonMuncher),
                                Hungry(RParenMuncher),
                                Hungry(OpMuncher),
                                Hungry(CommaMuncher),
                                Hungry(CommentMuncher),
                                Hungry(RBraceMuncher),
                            ]
                        )
                    }
                    Tokv::LParen => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(KeywordMuncher),
                                Hungry(LBraceMuncher),
                                Hungry(LiteralMuncher),
                                Hungry(IdentMuncher),
                                Hungry(RParenMuncher),
                                Hungry(LParenMuncher),
                                Hungry(CommentMuncher),
                            ]
                        )
                    }
                    Tokv::RParen => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(OpMuncher),
                                Hungry(ColonMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(LBraceMuncher),
                                Hungry(RBraceMuncher),
                                Hungry(RParenMuncher),
                                Hungry(SemicolonMuncher),
                                Hungry(CommentMuncher),
                                Hungry(CommaMuncher),
                            ]
                        )
                    }
                    Tokv::Comma => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(LiteralMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(IdentMuncher),
                                Hungry(CommentMuncher),
                            ]
                        )
                    }
                    Tokv::RBrace => {
                        any_muncher!(
                            &mut self,
                            tokens,
                            strict,
                            [
                                Hungry(LBraceMuncher),
                                Hungry(RParenMuncher),
                                Hungry(KeywordMuncher),
                                Hungry(RBraceMuncher),
                                Hungry(SemicolonMuncher),
                                Hungry(CommentMuncher),
                                Hungry(IdentMuncher),
                            ]
                        )
                    }
                    Tokv::Unknown(_) => {
                        // fall back to default, this is not a valid tokstream
                        // anyway
                        any_muncher!(&mut self, tokens, strict, [WhitespaceMuncher(0)])
                    }
                },
            }?;
        }
        tokens.retain(|tok| !matches!(tok.variant, Tokv::Ignore(_)));
        Ok(tokens)
    }

    /// Return the upcoming bytes starting at `self.index`
    pub(crate) fn all_upcoming_bytes(&self) -> &[u8] {
        &self.plaintext.as_bytes()[self.index()..]
    }

    /// Returns an upcoming byte slice starting at `index` of length `len`
    pub(crate) fn upcoming_bytes(&self, len: usize) -> &[u8] {
        let upcoming = self.all_upcoming_bytes();
        if upcoming.len() < len {
            upcoming
        } else {
            &upcoming[..len]
        }
    }

    /// Returns an upcoming `&str`, all the way to then end of the plaintext
    ///
    /// # Panics
    /// If there is no valid utf8 string starting at `self.index`
    pub(crate) fn all_upcoming_str(&self) -> &str {
        std::str::from_utf8(self.all_upcoming_bytes()).expect("valid utf8")
    }

    /// Returns an upcoming `&str` of size `len`
    ///
    /// # Panics
    /// If there is no valid utf8 string starting at `self.index`, with size
    /// `len`.
    pub(crate) fn upcoming_str(&self, len: usize) -> &str {
        std::str::from_utf8(self.upcoming_bytes(len)).expect("valid utf8")
    }

    pub(crate) fn index_add_mut(&self, offset: usize) {
        self.index.set(self.index() + offset)
    }

    pub(crate) fn index_sub_mut(&self, offset: usize) {
        self.index.set(self.index() - offset)
    }

    pub(crate) fn index(&self) -> usize {
        self.index.get()
    }

    /// peek the current byte, without munching anything
    #[allow(unused)]
    pub(crate) fn this_bchar(&self) -> Option<char> {
        self.all_upcoming_bytes().first().map(|b| *b as char)
    }
}
