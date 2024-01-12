use nom::{branch::alt, IResult};
use std::fmt;

use crate::parse::error::{Error as InternalParseError, Expected};
use crate::{ast::*, display_option_tok, input, lex::*, parse::*};

// #[derive(Debug)]
enum Nullable<'b> {
    Operator(&'b Token),
    Operand(Expr<'b>),
}

impl fmt::Debug for Nullable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Operator(op) => {
                let Tokv::Op(op) = op.variant else {
                    unreachable!()
                };
                write!(f, "{op}")
            }
            Self::Operand(opd) => {
                write!(
                    f,
                    "{}",
                    match opd {
                        Expr::Ident(ident) => ident.repr.as_exact_str(),
                        // Expr::Literal(literal) => literal.repr.as_exact_str(),
                        _ => "{expr}",
                    }
                )
            }
        }
    }
}

fn parse_expr_unambiguous_with_operand<'a>(
    toks: TokenStream<'a>,
) -> IResult<TokenStream<'a>, Expr, InternalParseError> {
    alt((
        Parser::parse_explicit_call_expr,
        Parser::parse_base_expr,
        Parser::parse_braces_expr,
        Parser::parse_if_then_else,
        Parser::parse_op_expr_force_parens,
    ))(toks)
}

impl Parser {
    /// Parses an op expr, but forces parens, this is to make it unambiguous
    /// from other operator expressions
    ///
    /// ```plaintext
    /// 5 * -5 + five
    /// ```
    ///
    /// would be read as the following, without this disambiguation.
    ///
    /// ```plaintext
    /// 5 * (-5 + five)
    /// ```
    pub fn parse_op_expr_force_parens<'a>(
        toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, Expr, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_op_expr_force_parens()");
        let (toks, _lparen) = tokt(&Tokt::LParen)(toks)?;
        let (toks, expr) = Parser::parse_op_expr_no_parens(toks)?;
        // if we had an opening brace, we must close it here!
        let (toks, _rparen) = tokt(&Tokt::RParen)(toks)?;
        Ok((toks, expr))
    }

    /// Parses an op expr, but without parens
    pub fn parse_op_expr_no_parens<'a>(
        mut toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, Expr, InternalParseError> {
        // expr, op pairs (must start and end with an expr and be at least
        // 3 long to be valid)
        let mut seq: Vec<Nullable<'a>> = Vec::with_capacity(8);
        loop {
            let (newtoks, exprtok) = parse_expr_unambiguous_with_operand(toks)?;
            toks = newtoks;
            seq.push(Nullable::Operand(exprtok));

            let Ok((
                newtoks,
                optok @ Token {
                    variant: Tokv::Op(..),
                    ..
                },
            )) = tokt(&Tokt::Op)(toks)
            else {
                break;
            };
            toks = newtoks;
            seq.push(Nullable::Operator(optok));
        }

        // if sequence is less than three long, we've basically just parsed
        // an expression, so we can return this as an expression
        if seq.len() == 1 {
            let Nullable::Operand(expr) = seq.swap_remove(0) else {
                unreachable!();
            };
            return Ok((toks, expr));
        }

        // we have to start and end with an expression, otherwise we're missing
        // an operator
        if seq.len() % 2 == 0 {
            return Err(nom::Err::Failure(InternalParseError {
                expected: Expected::Expr,
                tok: toks.0.first(),
                reason: Some("missing operand"),
            }));
        }

        // shunting yard
        let mut rpolish = Vec::new();
        let mut opstack = Vec::new();
        for nullable in seq.into_iter() {
            match nullable {
                Nullable::Operator(Token {
                    variant: Tokv::Op(op),
                    ..
                }) => {
                    while {
                        // there is some operator with an equal or greater
                        // precedence on the stack...
                        if let Some(Nullable::Operator(Token {
                            variant: Tokv::Op(top),
                            ..
                        })) = opstack.last()
                        {
                            top >= op
                        } else {
                            false
                        }
                    } {
                        rpolish.push(opstack.pop().expect("just checked this is Some"));
                    }
                    opstack.push(nullable);
                }
                Nullable::Operand(..) => {
                    rpolish.push(nullable);
                }
                _ => unreachable!(),
            }
        }
        rpolish.extend(opstack.into_iter().rev());

        // obviously, there is no need for this to be `Nullable` still, but I
        // can't be bothered to changed it!
        let mut stack = Vec::with_capacity(rpolish.len() / 3 + 2); // ab*cd*+
        for nullable in rpolish {
            match nullable {
                Nullable::Operand(expr) => {
                    stack.push(Nullable::Operand(expr));
                }
                Nullable::Operator(
                    tok @ Token {
                        variant: Tokv::Op(op),
                        ..
                    },
                ) => {
                    // we need to pop these 'backwards'
                    let (Some(Nullable::Operand(expr2)), Some(Nullable::Operand(expr1))) =
                        (stack.pop(), stack.pop())
                    else {
                        unreachable!()
                    };

                    let expr = Expr::ExplicitCall {
                        callable: Callable {
                            id: CallableId::Op(*op),
                            repr: tok,
                        },
                        params: vec![expr1, expr2],
                    };
                    stack.push(Nullable::Operand(expr));
                }
                _ => unreachable!(),
            }
        }

        debug_assert_eq!(stack.len(), 1);
        let Some(Nullable::Operand(expr)) = stack.pop() else {
            unreachable!()
        };

        Ok((toks, expr))
    }

    /// Parse an expression that involves operators
    pub fn parse_op_expr<'a>(
        toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, Expr, InternalParseError> {
        display_option_tok!(toks.0.first());
        input!("parse_op_expr()");

        // we let operator expressions include regualr parentheses
        let (toks, expr) = alt((
            Parser::parse_op_expr_no_parens,
            Parser::parse_op_expr_force_parens,
        ))(toks)?;

        Ok((toks, expr))
    }
}

#[cfg(test)]
mod tests {
    use crate::{lex::*, parse::*};

    #[test]
    fn op_precedence_works() {
        let plaintext = r#"if "something" * 'c' / 'c' - 2 * 6"#;
        let toks = Lexer::new(&plaintext).into_token_stream(true).unwrap();
        let (_, _expr) =
            Parser::parse_op_expr(crate::parse::TokenStream(&toks[1..])).expect("valid op expr");
    }

    #[test]
    fn leq_op_works() {
        let plaintext = r#"if a <= b * c + d * e"#;
        // SOLUTION: sort lowest to highest instead, and do a recursive search
        //                           ^   ^   ^
        // a a * b b * +
        // 1 * 2 + 3 * 4 + 5 * 6
        //       ^
        //
        //
        let toks = Lexer::new(&plaintext).into_token_stream(true).unwrap();
        let (_, expr) =
            Parser::parse_op_expr(crate::parse::TokenStream(&toks[1..])).expect("valid op expr");
        dbg!(expr);
    }
}

// lt(add(6, 6), add(5, 5))
