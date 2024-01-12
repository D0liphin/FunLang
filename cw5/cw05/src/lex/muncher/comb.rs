use crate::lex::*;

/// Create a muncher that consumes and ignores all leading whitespace before
/// it starts. The muncher should not consume whitespace already.
///
/// It is called [`Hungry`] because the muncher is not jut satisfied with
/// eating ascii whitespace, it wants to eat something yummy!
pub(crate) struct Hungry<M>(pub(crate) M)
where
    M: Muncher;

impl<M> Muncher for Hungry<M>
where
    M: Muncher,
{
    type Output = <M as Muncher>::Output;

    fn munch(&mut self, lexer: &mut Lexer<'_>) -> Result<Self::Output, LexingError> {
        let start_index = lexer.index();
        let _ = WhitespaceMuncher(0).munch(lexer)?;
        match self.0.munch(lexer) {
            Ok(tok) => Ok(tok),
            Err(e) => {
                lexer.index.set(start_index);
                Err(e)
            }
        }
    }
}

// pub(crate) struct MuncherSeq<'a, 'b> {
//     start_index: usize,
//     lexer: &'a mut Lexer<'b>,
//     output: Vec<Token>,
//     err: Option<LexingError>,
// }

// impl<'a, 'b> MuncherSeq<'a, 'b> {
//     /// Creates a new muncher combinator with the expected output `size`. For
//     /// example, if we want to munch 5 tokens in sequence, you should set the
//     /// [`MuncherSeq`] size to `5` (probably).
//     pub(crate) fn new(lexer: &'a mut Lexer<'b>, size: usize) -> Self {
//         Self {
//             start_index: lexer.index(),
//             lexer,
//             output: Vec::with_capacity(size),
//             err: None,
//         }
//     }

//     /// Consume this [`MuncherSeq`] and spit out the sequence of tokens
//     /// it matched, if any.
//     pub(crate) fn collect(self) -> Result<Vec<Token>, LexingError> {
//         match self.err {
//             Some(e) => {
//                 // failed, so bring back to the beginning
//                 self.lexer.index.set(self.start_index);
//                 Err(e)
//             }
//             None => Ok(self.output),
//         }
//     }

//     // pub(crate) fn then_munch_pred()

//     /// Munch using another [`Muncher`].
//     pub(crate) fn then_munch<M>(mut self, mut m: M) -> Self
//     where
//         M: Muncher<Output = Token>,
//     {
//         if self.err.is_some() {
//             return self;
//         }

//         match m.munch(self.lexer) {
//             Ok(tok) => {
//                 let tok = tok.into();
//                 self.output.push(tok);
//             }
//             Err(e) => {
//                 self.err = Some(e);
//                 self.lexer.index.set(self.start_index);
//             }
//         }

//         self
//     }

//     /// Munch using another [`Muncher`]. But do not add its successful output
//     /// to the internal token list.
//     pub(crate) fn then<M>(mut self, mut m: M) -> Self
//     where
//         M: Muncher,
//     {
//         if self.err.is_some() {
//             return self;
//         }

//         match m.munch(self.lexer) {
//             Ok(_) => {}
//             Err(e) => {
//                 self.err = Some(e);
//                 self.lexer.index.set(self.start_index);
//             }
//         }

//         self
//     }
// }

// #[cfg(test)]
// mod tests {
//     use crate::lex::*;

//     #[test]
//     fn munches_sequences() {
//         let mut lexer = Lexer::new("if true then 42.5 else 96_000");
//         let toks = MuncherSeq::new(&mut lexer, 4)
//             .then_munch(KeywordMuncher)
//             .then(WhitespaceMuncher(1))
//             .then_munch(LiteralMuncher)
//             .then(WhitespaceMuncher(1))
//             .then_munch(KeywordMuncher)
//             .then(WhitespaceMuncher(1))
//             .then_munch(LiteralMuncher)
//             .then(WhitespaceMuncher(1))
//             .then_munch(KeywordMuncher)
//             .then(WhitespaceMuncher(1))
//             .then_munch(LiteralMuncher)
//             .collect()
//             .unwrap();

//         assert_eq!(
//             toks,
//             vec![
//                 Token::new(KeywordToken::If, 0),
//                 Token::new(LiteralToken::Bool(true), 3),
//                 Token::new(KeywordToken::Then, 8),
//                 Token::new(LiteralToken::Double(42.5), 13),
//                 Token::new(KeywordToken::Else, 18),
//                 Token::new(LiteralToken::Int(96_000), 23),
//             ]
//         )
//     }
// }
