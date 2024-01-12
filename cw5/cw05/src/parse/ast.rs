//! module for parsing asts and astnodes

use nom::IResult;
use nom::branch::alt;

use crate::ast::*;
use crate::parse::error::Error as InternalParseError;
use crate::parse::*;

impl Parser {
    fn parse_expr_as_ast_node<'a>(
        toks: TokenStream<'a>,
    ) -> IResult<TokenStream<'a>, AstNode<'a>, InternalParseError> {
        let (toks, expr) = Parser::parse_expr(toks)?;
        Ok((toks, AstNode::Expr(expr)))
    }

    /// I really should have named this "item" or something, but it's too late
    /// now isn't it.
    ///
    /// Parses any [`AstNode`]
    pub fn parse_ast_node(toks: TokenStream) -> IResult<TokenStream, AstNode, InternalParseError> {
        alt((
            Parser::parse_expr_as_ast_node,
            Parser::parse_def,
            Parser::parse_const_decl,
        ))(toks)
    }

    /// Parse an entire program, generating an abstract syntax tree
    pub fn parse_ast(mut toks: TokenStream) -> IResult<TokenStream, Ast, InternalParseError> {
        let mut ast = Ast::new();
        while toks.0.len() != 0 {
            let (newtoks, node) = Self::parse_ast_node(toks)?;
            ast.push(node);
            toks = newtoks;
        }
        Ok((toks, ast))
    }
}
