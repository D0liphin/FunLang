use crate::fmt::display_line;
use crate::lex::token::TokenTy;
use colored::Colorize;
use either::Either;

#[derive(Debug, Clone)]
pub struct LexingError {
    /// The index where we encountered this lexing error
    pub(crate) index: usize,
    /// The tokens we expected to find at this index
    pub(crate) expected: Either<TokenTy, Box<[TokenTy]>>,
    /// Additional information about why we could not parse the token, for
    /// displaying this error.
    pub(crate) reason: Option<&'static str>,
}

impl LexingError {
    pub fn new(index: usize, ty: TokenTy) -> Self {
        Self {
            index,
            expected: Either::Left(ty),
            reason: None,
        }
    }

    /// Gets the only expected token
    ///
    /// # Panics
    /// if multiple tokens are expected
    pub fn expected_single(&self) -> TokenTy {
        let Either::Left(single) = self.expected else {
            panic!("multiple tokens expected")
        };
        single
    }

    /// Swap this error if `e` is more descriptive
    ///
    /// This should be used for Munchers that could fail deeper for a different
    /// reason. For these lexers, the muncher that gets the furthest is
    /// probably the most descriptive muncher.
    pub fn swap_if_better(&mut self, e: LexingError) {
        if self.reason.is_some() {
            if e.reason.is_some() && self.index < e.index {
                *self = e;
            }
        } else if e.reason.is_some() {
            *self = e;
        }
    }

    pub fn with_reason(mut self, reason: &'static str) -> Self {
        self.reason = Some(reason.into());
        self
    }

    /// format the various token types that might have been expected here
    pub fn display_expected(&self) -> String {
        match &self.expected {
            Either::Left(ty) => format!("{ty}").into(),
            Either::Right(tys) => {
                let tys: &[TokenTy] = &tys;
                match tys {
                    [ty] => format!("{ty}").into(),
                    [tys @ ..] => {
                        let mut result = String::new();
                        for ty in &tys[..tys.len() - 2] {
                            result += &format!("{ty}, ");
                        }
                        result += &format!("{}", tys[tys.len() - 2]);
                        result += &format!(" or {}", tys[tys.len() - 1]);
                        result
                    }
                }
            }
        }
    }

    /// format this error in a human-readable way
    ///
    /// # Panics
    /// The plaintext is not the same as the one this [`LexingError`] was
    /// generated from
    pub fn display_err(&self, plaintext: &str) -> String {
        let mut err = String::with_capacity(256);

        // something like "error: lexing error" will be printed
        err += &format!("{}", "error".red().bold());
        let error_reason = format!(": expected {}", self.display_expected()).bold();
        err += &format!("{error_reason}");
        err += "\n";

        err += &display_line(self.index, plaintext, |caret_offset| {
            let mut s = String::with_capacity(64);
            s += &" ".repeat(caret_offset);
            s += &format!(
                "{}",
                format!(
                    "^ {}",
                    self.reason
                        .as_ref()
                        .map(|r| -> &str { &r })
                        .unwrap_or("lexing error")
                )
                .red()
                .bold()
            );
            s
        });

        err
    }
}