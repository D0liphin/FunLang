//! Contains definitions required for a an untyped AST (hence the name '`weak'`)
//! For a typed version, see [`crate::typecheck::tast`]

use crate::lex::*;
use colored::Colorize;

/// A reference to a type token, with an additional ID
#[derive(Debug, Clone, PartialEq)]
pub struct Type<'a> {
    pub repr: &'a Token,
}

/// a reference to an ident token, with an additional ID
#[derive(Debug, Clone, PartialEq)]
pub struct Ident<'a> {
    pub repr: &'a Token,
}

/// A callable can either be an op or an identifier. I'm not sure if this is a
/// good way of representing this, but hey-ho. Since fun has no generic typing,
/// ops are the only functions that support overloading and we handle that
/// here
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CallableId {
    Ident,
    Op(OpToken),
    Write,
}

/// A [`CallableId`] and a debug repr
#[derive(Debug, Clone, PartialEq)]
pub struct Callable<'a> {
    pub id: CallableId,
    pub repr: &'a Token,
}

/// A literal [`Token`] and a type_id, which should always be resolvable
/// at the mmoment.
#[derive(Debug, Clone, PartialEq)]
pub struct Literal<'a> {
    pub repr: &'a Token,
}

/// A function signature, when evaluated, this should add a new type to the
/// type map
#[derive(Debug, Clone, PartialEq)]
pub struct FnSignature<'a> {
    pub ident: Ident<'a>,
    pub params: Vec<Param<'a>>,
    pub ret: Type<'a>,
}

/// A parameter to a function, has token lookups for both the identifier and the
/// type
#[derive(Debug, Clone, PartialEq)]
pub struct Param<'a> {
    pub ident: Ident<'a>,
    pub ty: Type<'a>,
}

/// Almost everything in fun is an expression... or at least that's the idea.
/// In fact in the future we could have something like
///
/// ```plaintext
/// val MyVoid: Void = val MyInt: Int = 42;
/// ```
///
/// which would assign both `MyVoid` and `MyInt` a valid value
///
/// However, at the moment, `val` decls are `const` and global and so are `def`
/// decls, which is quite useful because then we allow them all to be reordered
/// any way we like
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    IfThenElse {
        condition_repr: &'a Token,
        then_repr: &'a Token,
        else_repr: &'a Token,
        condition: Box<Expr<'a>>,
        then_expr: Box<Expr<'a>>,
        else_expr: Box<Expr<'a>>,
    },
    Literal(Literal<'a>),
    /// This is just to show that this is not a const operation, it might
    /// evaluate by calling a function. This is all dependent on the type of
    /// `Ident`
    Ident(Ident<'a>),
    /// Operators are also converted to `ExplicitCall`s.
    ExplicitCall {
        callable: Callable<'a>,
        params: Vec<Expr<'a>>,
    },
    /// Only the last expr is actually type checked
    Seq(Vec<Expr<'a>>),
}

impl<'a> TryFrom<&'a Token> for Expr<'a> {
    type Error = ();

    fn try_from(value: &'a Token) -> Result<Self, Self::Error> {
        match &value.variant {
            Tokv::Ident(..) => Ok(Expr::Ident(Ident { repr: value })),
            Tokv::Literal(..) => Ok(Expr::Literal(Literal { repr: value })),
            _ => Err(()),
        }
    }
}

/// All the possible nodes in an AST
#[derive(Debug, Clone, PartialEq)]
pub enum AstNode<'a> {
    Const {
        ident: Ident<'a>,
        ty: Type<'a>,
        // TODO: allow assignment from the result of functions
        value: Literal<'a>,
    },
    Def {
        signature: FnSignature<'a>,
        body: Expr<'a>,
    },
    Expr(Expr<'a>),
}

impl PartialOrd for AstNode<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        fn as_u32(node: &AstNode) -> u32 {
            match node {
                AstNode::Const { .. } => 0,
                AstNode::Def { .. } => 1,
                AstNode::Expr { .. } => 2,
            }
        }
        as_u32(&self).partial_cmp(&as_u32(&other))
    }
}

pub type Ast<'a> = Vec<AstNode<'a>>;

/// A clone implementation that should panic if we try and clone something
/// beyond a depth of 1. Aka, this should only clone leaves.
pub trait CloneShallow
where
    Self: Sized,
{
    fn try_clone_shallow(&self) -> Option<Self>;

    /// A clone implementation that should panic if we try and clone something
    /// beyond a depth of 1. Aka, this should only clone leaves.
    ///
    /// # Panics
    /// If this is no a leaf node
    fn clone_shallow(&self) -> Self {
        if let Some(val) = self.try_clone_shallow() {
            val
        } else {
            panic!("illegal recursive clone")
        }
    }
}

impl CloneShallow for Expr<'_> {
    fn try_clone_shallow(&self) -> Option<Self> {
        match self {
            Self::Literal { .. } | Self::Ident { .. } => Some(self.clone()),
            _ => None,
        }
    }
}

// suite of naive display functions -- not intended to be fast

fn display_ast_padding(depth: usize) -> String {
    " ".repeat(depth)
}

fn display_ast_op(op: OpToken) -> &'static str {
    match op {
        OpToken::Add => "ADD",
        OpToken::DblEq => "DBLEQ",
        OpToken::Geq => "GEQ",
        OpToken::Div => "DIV",
        OpToken::Gt => "GT",
        OpToken::Leq => "LEQ",
        OpToken::Lt => "LT",
        OpToken::Mod => "MOD",
        OpToken::Mul => "MUL",
        OpToken::Sub => "SUB",
        OpToken::Neq => "NEQ",
        // should never really be generated
        OpToken::Eq => "EQ",
    }
}

fn display_ast_expr_inline(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Literal(literal) => Some(display_ast_literal(literal)),
        Expr::Ident(ident) => Some(display_ast_ident(ident)),
        _ => None,
    }
}

fn display_ast_args(args: &[Expr], depth: usize) -> String {
    let inline_args = args
        .iter()
        .filter_map(|arg| display_ast_expr_inline(arg))
        .collect::<Vec<_>>();
    let inline_args_str = format!("({})", inline_args.join(" "));

    if inline_args.len() == args.len() && inline_args_str.len() < 100 {
        inline_args_str
    } else {
        format!(
            "(\n{}{padding})",
            args.iter()
                .map(|arg| format!("{}", display_ast_expr(arg, depth + 1)))
                .collect::<Vec<_>>()
                .join(""),
            padding = display_ast_padding(depth)
        )
    }
}

fn display_ast_callable(callable: &Callable) -> String {
    match callable.id {
        CallableId::Ident => format!(
            "{}{}={}",
            "IDENT".red(),
            display_ast_token(callable.repr),
            callable.repr.variant
        ),
        CallableId::Write => format!("#builtin{}", "WRITE".bright_red()),
        CallableId::Op(op) => format!("#builtin{}", display_ast_op(op).bright_red()),
    }
}

pub fn display_ast_expr(body: &Expr, depth: usize) -> String {
    let padding = display_ast_padding(depth);
    match body {
        Expr::ExplicitCall { callable, params } => {
            format!(
                "{}{} {}{}\n",
                padding,
                "EXPLICITCALL".magenta().bold(),
                display_ast_callable(callable),
                display_ast_args(&params, depth)
            )
        }
        Expr::IfThenElse {
            condition,
            then_expr,
            else_expr,
            ..
        } => {
            format!(
                "{padding}{iff}\n{condition}{padding}{then}\n{then_expr}{padding}{elsee}\n{else_expr}",
                iff = "IF:".red().bold(),
                condition = display_ast_expr(condition, depth + 1),
                then = "THEN:".red().bold(),
                then_expr = display_ast_expr(then_expr, depth + 1),
                elsee = "ELSE:".red().bold(),
                else_expr = display_ast_expr(else_expr, depth + 1),
            )
        }
        Expr::Literal(literal) => format!("{padding}{}\n", display_ast_literal(literal)),
        Expr::Ident(ident) => format!("{padding}{}\n", display_ast_ident(ident)),
        Expr::Seq(seq) => {
            let mut s = String::new();
            for expr in seq.iter() {
                // s += &format!("{}{}{}:\n", padding, "SEQEXPR".magenta().bold(), i);
                s += &display_ast_expr(expr, depth + 1);
            }
            s
        }
    }
}

fn display_ast_range(start: usize, end: usize) -> String {
    if end - start == 1 {
        format!("@{}", format!("{}", start).cyan(),)
    } else {
        format!(
            "@{}..{}",
            format!("{}", start).cyan(),
            format!("{}", end).cyan()
        )
    }
}

fn display_ast_token(token: &Token) -> String {
    display_ast_range(token.index, token.index + token.len)
}

fn display_ast_param(param: &Param) -> String {
    format!(
        "{}{}={}:{}",
        "PARAM".red(),
        display_ast_range(
            param.ident.repr.index,
            param.ty.repr.index + param.ty.repr.len
        ),
        param.ident.repr.variant,
        format!("{}", param.ty.repr.variant).yellow(),
    )
}

fn display_ast_fn_signature(fn_signature: &FnSignature) -> String {
    format!(
        "{}({})",
        display_ast_ident(&fn_signature.ident),
        fn_signature
            .params
            .iter()
            .map(|p| format!("{}", display_ast_param(p)))
            .collect::<Vec<_>>()
            .join(" ")
    )
}

fn display_ast_ident(ident: &Ident) -> String {
    format!(
        "{}{}={}",
        "IDENT".red(),
        display_ast_token(ident.repr),
        ident.repr.variant
    )
}

fn display_ast_ty(ty: &Type) -> String {
    format!(
        "{}{}={}",
        "TYPE".red(),
        display_ast_token(ty.repr),
        format!("{}", ty.repr.variant).yellow()
    )
}

fn display_ast_literal(literal: &Literal) -> String {
    format!(
        "{}{}={}",
        "LITERAL".red(),
        display_ast_token(literal.repr),
        format!("{}", literal.repr.variant).green(),
    )
}

pub fn display_ast_node(ast_node: &AstNode, depth: usize) -> String {
    let mut s = String::new();
    let padding = display_ast_padding(depth);
    s += &match ast_node {
        AstNode::Const { ident, ty, value } => {
            format!(
                "{padding}{name} {ident}: {ty} = {value}\n",
                name = "CONSTDECL".bold().magenta(),
                ident = display_ast_ident(ident),
                ty = display_ast_ty(ty),
                value = display_ast_literal(value)
            )
        }
        AstNode::Def { signature, body } => {
            format!(
                "\n{padding}{name} {signature}:\n{body}",
                name = "DEF".bold().magenta(),
                signature = display_ast_fn_signature(signature),
                body = display_ast_expr(body, depth + 1),
            )
        }
        AstNode::Expr(expr) => display_ast_expr(expr, depth),
    };
    s
}

pub fn display_ast(ast: &Ast) -> String {
    let mut s = String::new();
    for node in ast {
        s += &display_ast_node(node, 0);
    }
    s
}
