use colored::Colorize;
use nom::error::{ErrorKind, ParseError};

use super::{TokenStream, Tokt};
use crate::{fmt::display_line, lex::*};
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Expected<'a> {
    Tokv(&'a Tokv),
    Tokt(Tokt),
    Expr,
    Any,
}

impl<'a> fmt::Display for Expected<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expected::Any => write!(f, "any"),
            Expected::Tokt(tokt) => write!(f, "{}", tokt),
            Expected::Tokv(tokv) => write!(f, "exact token '{}'", tokv),
            Expected::Expr => write!(f, "expression"),
        }
    }
}

/// A parsing error
#[derive(Debug, PartialEq, Clone)]
pub struct Error<'a, 'b> {
    /// `None` if EOF, otherwise `Some` and the token
    pub tok: Option<&'a Token>,
    pub expected: Expected<'b>,
    pub reason: Option<&'static str>,
}

impl<'a, 'b> Error<'a, 'b> {
    pub fn reached_eof() -> Self {
        Self {
            tok: None,
            expected: Expected::Any,
            reason: Some("encountered bug, please submit bug report"),
        }
    }

    pub fn display_err(&self, plaintext: &str) -> String {
        let mut err = String::new();
        let Some(tok) = self.tok else {
            panic!("cannot display error");
        };

        err += &format!("{}", "error".red().bold());
        let error_reason = format!(": expected {}", self.expected).bold();
        err += &format!("{error_reason}");
        err += "\n";

        err += &display_line(tok.index, plaintext, |offset| {
            format!(
                "{}",
                format!(
                    "{}{} {}",
                    " ".repeat(offset),
                    "^".repeat(tok.len),
                    self.reason.unwrap_or("at this token")
                )
                .red()
                .bold()
            )
            .into()
        });

        err
    }
}

impl<'a, 'b> From<(&'a Token, &'b Tokv)> for Error<'a, 'b> {
    fn from(value: (&'a Token, &'b Tokv)) -> Self {
        Self {
            tok: Some(value.0),
            expected: Expected::Tokv(value.1),
            reason: None,
        }
    }
}

impl<'a, 'b> From<(&'a Token, &'b Tokt)> for Error<'a, 'b> {
    fn from(value: (&'a Token, &'b Tokt)) -> Self {
        Self {
            tok: Some(value.0),
            expected: Expected::Tokt(*value.1),
            reason: None,
        }
    }
}

impl ParseError<TokenStream<'_>> for Error<'_, '_> {
    fn append(_: TokenStream<'_>, _: ErrorKind, other: Self) -> Self {
        other
    }

    fn from_error_kind(_: TokenStream<'_>, _: ErrorKind) -> Self {
        panic!("don't see how this method is relevant")
    }
}