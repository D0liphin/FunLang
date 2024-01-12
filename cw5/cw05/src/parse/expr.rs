use super::{error::Error as InternalParseError, Parser, TokenStream};
use crate::ast::Expr;
use nom::{branch::alt, IResult};

impl Parser {
    /// Parse any expression
    pub fn parse_expr<'b>(
        toks: TokenStream<'b>,
    ) -> IResult<TokenStream<'b>, Expr<'b>, InternalParseError> {
        // the ordering of these matters
        alt((
            Self::parse_if_then_else,
            Self::parse_op_expr,
            Self::parse_explicit_call_expr,
            Self::parse_seq_expr,
            Self::parse_base_expr,
            // this is not needed technically, as parse_seq_expr has a special
            // case for this anyway, but I want to keep it...
            Self::parse_braces_expr,
        ))(toks)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::display_ast;
    use crate::fmt::highlight_syntax;
    use crate::lex::Lexer;
    use crate::parse::{Parser, TokenStream};

    const PLAINTEXT: &'static str = include_str!("../../../mand2.fun");
    
    #[test]
    fn parse_def() {
        let result = Lexer::new(PLAINTEXT).into_token_stream(true);
        let toks = match result {
            Err(e) => {
                panic!("{}", e.display_err(PLAINTEXT))
            }
            Ok(toks) => toks,
        };

        println!("{}", highlight_syntax(PLAINTEXT, &toks, true));
        let (_, ast) = &Parser::parse_ast(TokenStream(&toks)).unwrap();
        println!("{}", display_ast(ast));
    }
}
