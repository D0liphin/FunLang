//! We need to define a way of using `nom` on tokens, this is where
//! [`TokenStream`] and [`TokvStream`] come in! We can compare [`TokenStream`]s
//! to [`TokvStream`]!

use std::fmt;

use nom::{Compare, CompareResult, InputIter, InputLength, InputTake};

use crate::lex::*;

macro_rules! nom_slice {
    ($Stream:ident<$T:ty>) => {
        impl InputTake for $Stream<'_> {
            fn take(&self, count: usize) -> Self {
                Self(&self.0[..count])
            }

            fn take_split(&self, count: usize) -> (Self, Self) {
                let (head, tail) = self.0.split_at(count);
                ($Stream(head), $Stream(tail))
            }
        }

        impl InputLength for $Stream<'_> {
            fn input_len(&self) -> usize {
                self.0.len()
            }
        }

        impl<'a> InputIter for $Stream<'a> {
            type Item = &'a $T;
            type Iter = std::iter::Enumerate<std::slice::Iter<'a, $T>>;
            type IterElem = std::slice::Iter<'a, $T>;

            fn iter_elements(&self) -> Self::IterElem {
                self.0.iter()
            }

            fn iter_indices(&self) -> Self::Iter {
                self.0.iter().enumerate()
            }

            fn position<P>(&self, predicate: P) -> Option<usize>
            where
                P: Fn(Self::Item) -> bool,
            {
                self.iter_elements().position(predicate)
            }

            fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
                if self.0.len() >= count {
                    Ok(count)
                } else {
                    Err(nom::Needed::new(count - self.0.len()))
                }
            }
        }
    };
}

macro_rules! cmp_tokstream {
    ($Stream:ident) => {
        impl<'a> Compare<$Stream<'a>> for TokenStream<'a> {
            fn compare(&self, tokvs: $Stream<'a>) -> CompareResult {
                // this code is largely the same as the internal implemntation for
                // byte slices in nom... I wonder why they didn't just make this auto
                // implement!
                let pos = self.0.iter().zip(tokvs.0.iter()).position(|(a, b)| a != b);

                match pos {
                    Some(_) => CompareResult::Error,
                    None => {
                        if self.0.len() >= tokvs.0.len() {
                            CompareResult::Ok
                        } else {
                            // [head, ..] is apparently an `Incomplete`. Whatever that
                            // means
                            CompareResult::Incomplete
                        }
                    }
                }
            }

            fn compare_no_case(&self, _: $Stream<'a>) -> CompareResult {
                unimplemented!("this makes no sense to implement for tokens")
            }
        }
    };
}

#[derive(Clone, Copy, Debug)]
pub struct TokenStream<'a>(pub &'a [Token]);
nom_slice!(TokenStream<Token>);

#[derive(Clone, Copy)]
pub struct TokvStream<'a>(pub &'a [Tokv]);
nom_slice!(TokvStream<Tokv>);
cmp_tokstream!(TokvStream);

#[derive(Clone, Copy)]
pub struct ToktStream<'a>(pub &'a [Tokt]);
nom_slice!(ToktStream<Tokt>);
cmp_tokstream!(ToktStream);

/// Match any token, only based on its tag
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Tokt {
    Semicolon,
    Literal,
    Op,
    Ty,
    Ident,
    Colon,
    Comma,
    LBrace,
    RBrace,
    LParen,
    RParen,
    /// Match any token type
    Any,
    #[allow(unused)]
    Ignore,
    #[allow(unused)]
    Keyword,
    #[allow(unused)]
    Unknown,
}

impl fmt::Display for Tokt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Semicolon => ";",
                Self::Keyword => "keyword",
                Self::Literal => "literal",
                Self::Op => "operator",
                Self::Ty => "type",
                Self::Ident => "identifier",
                Self::Colon => ":",
                Self::Comma => "comma",
                Self::LBrace => "{",
                Self::RBrace => "}",
                Self::LParen => "(",
                Self::RParen => ")",
                Self::Ignore => "ignoreable",
                Self::Unknown => "{unknown}",
                Self::Any => "any",
            }
        )
    }
}

impl PartialEq<Tokt> for Token {
    fn eq(&self, other: &Tokt) -> bool {
        match (&self.variant, other) {
            (Tokv::Semicolon, Tokt::Semicolon)
            | (Tokv::Keyword(..), Tokt::Keyword)
            | (Tokv::Literal(..), Tokt::Literal)
            | (Tokv::Op(..), Tokt::Op)
            | (Tokv::Ty(..), Tokt::Ty)
            | (Tokv::Ident(..), Tokt::Ident)
            | (Tokv::Colon, Tokt::Colon)
            | (Tokv::Comma, Tokt::Comma)
            | (Tokv::LBrace, Tokt::LBrace)
            | (Tokv::RBrace, Tokt::RBrace)
            | (Tokv::LParen, Tokt::LParen)
            | (Tokv::RParen, Tokt::RParen)
            | (Tokv::Ignore(..), Tokt::Ignore)
            | (Tokv::Unknown(..), Tokt::Unknown) => true,
            (_, Tokt::Any) => true,
            _ => false,
        }
    }
}
