use colored::Colorize;
use crate::{fmt::display_line, lex::muncher::*, typecheck::*};
use std::fmt::{self};

macro_rules! impl_into_token {
    ($Tokv:ident($SomeToken:ident)) => {
        impl Into<Tokv> for $SomeToken {
            fn into(self) -> Tokv {
                Tokv::$Tokv(self)
            }
        }
    };
}

impl_into_token!(Keyword(KeywordToken));
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordToken {
    Def,
    Val,
    If,
    Then,
    Else,
    Write,
}

impl fmt::Display for KeywordToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Def => KEYWORD_DEF,
            Self::Val => KEYWORD_VAL,
            Self::Else => KEYWORD_ELSE,
            Self::Then => KEYWORD_THEN,
            Self::Write => KEYWORD_WRITE,
            Self::If => KEYWORD_IF,
        };
        write!(f, "{}", std::str::from_utf8(s).expect("is valid utf8"))
    }
}

/// Two variant enum indicating whether an `Int` literal is a char or an actual
/// integer, for debugging code
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CharInt {
    Char(char),
    Int(i64),
}

impl_into_token!(Literal(LiteralToken));
#[derive(Debug, PartialEq, Clone)]
pub enum LiteralToken {
    /// # Note
    /// This is also what char literals compile to in fun... I know it's 
    /// horrific!
    Int(CharInt),
    Double(f64),
    String(String),
    Bool(bool),
    Void,
}

impl fmt::Display for LiteralToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(CharInt::Int(n)) => write!(f, "{}", n),
            Self::Int(CharInt::Char(c)) => write!(f, "{:?}", c),
            Self::Double(n) => write!(f, "{:?}", n),
            Self::String(s) => write!(f, "{:?}", s),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Void => write!(f, "..."),
        }
    }
}

impl_into_token!(Op(OpToken));
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpToken {
    /// `=`
    Eq,
    /// `==`
    DblEq,
    /// `!=`
    Neq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    Leq,
    /// `>=`
    Geq,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `%`
    Mod,
    /// `/`
    Div,
    /// `*`
    Mul,
}

impl OpToken {
    /// Convert this [`OpToken`] to a `&'static str` that is the exact 
    /// identifier for the corresponding builtin
    /// 
    /// # Parameters
    /// - `ret` the type that this operator should return -- this will not be 
    ///   used in some cases, but for other types, we need it for overloading
    /// 
    /// # Panics
    /// If this operator has no builtin implementation
    pub fn as_builtin(&self, ret: &FunType) -> &'static str {
        match (self, ret) {
            (OpToken::Add, FunType::PInt) => BUILTIN_IADD,
            (OpToken::Sub, FunType::PInt) => BUILTIN_ISUB,
            (OpToken::Mul, FunType::PInt) => BUILTIN_IMUL,
            (OpToken::Div, FunType::PInt) => BUILTIN_IDIV,
            (OpToken::Mod, FunType::PInt) => BUILTIN_IREM,

            (OpToken::Add, FunType::PDouble) => BUILTIN_FADD,
            (OpToken::Sub, FunType::PDouble) => BUILTIN_FSUB,
            (OpToken::Mul, FunType::PDouble) => BUILTIN_FMUL,
            (OpToken::Div, FunType::PDouble) => BUILTIN_FDIV,

            (OpToken::DblEq, FunType::PInt) => BUILTIN_ICMP_EQ,
            (OpToken::Neq, FunType::PInt) => BUILTIN_ICMP_NE,
            (OpToken::Gt, FunType::PInt) => BUILTIN_ICMP_SGT,
            (OpToken::Geq, FunType::PInt) => BUILTIN_ICMP_SGE,
            (OpToken::Lt, FunType::PInt) => BUILTIN_ICMP_SLT,
            (OpToken::Leq, FunType::PInt) => BUILTIN_ICMP_SLE,

            (OpToken::DblEq, FunType::PDouble) => BUILTIN_FCMP_EQ,
            (OpToken::Neq, FunType::PDouble) => BUILTIN_FCMP_NE,
            (OpToken::Gt, FunType::PDouble) => BUILTIN_FCMP_UGT,
            (OpToken::Geq, FunType::PDouble) => BUILTIN_FCMP_UGE,
            (OpToken::Lt, FunType::PDouble) => BUILTIN_FCMP_ULT,
            (OpToken::Leq, FunType::PDouble) => BUILTIN_FCMP_ULE,
            
            _ => panic!("no such implementation for {self:?}"),
        }
    }
}

impl PartialOrd for OpToken {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        fn as_u32(tok: OpToken) -> u32 {
            match tok {
                OpToken::Eq => 0,
                OpToken::DblEq => 1,
                OpToken::Neq => 1,
                OpToken::Lt => 1,
                OpToken::Gt => 1,
                OpToken::Leq => 1,
                OpToken::Geq => 1,
                OpToken::Add => 2,
                OpToken::Sub => 2,
                OpToken::Mod => 3,
                OpToken::Div => 3,
                OpToken::Mul => 3,
            }
        }
        as_u32(*self).partial_cmp(&as_u32(*other))
    }
}

impl fmt::Display for OpToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DblEq => write!(f, "=="),
            Self::Eq => write!(f, "="),
            Self::Neq => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::Leq => write!(f, "<="),
            Self::Geq => write!(f, ">="),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mod => write!(f, "%"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenTy {
    Char(char),
    Keyword,
    Op,
    Literal,
    Ident,
    Whitespace,
    Comment,
    Semicolon,
    Colon,
    Comma,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Ty,
}

impl fmt::Display for TokenTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Char(ch) => write!(f, "{:?}", ch),
            Self::Keyword => write!(f, "keyword"),
            Self::Op => write!(f, "operator"),
            Self::Literal => write!(f, "literal"),
            Self::Ident => write!(f, "identifier"),
            Self::Whitespace => write!(f, "whitespace"),
            Self::Comment => write!(f, "comment"),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, "comma"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Ty => write!(f, "type"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Tokv {
    Keyword(KeywordToken),
    Literal(LiteralToken),
    Op(OpToken),
    Ident(String),
    /// A typename
    Ty(String),
    /// Ignore some bytes, used both by comments and whitespace
    Ignore(usize),
    Semicolon,
    Colon,
    Comma,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Unknown(char),
}

impl fmt::Display for Tokv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Keyword(kw) => write!(f, "{}", kw),
            Self::Literal(lit) => write!(f, "{}", lit),
            Self::Op(op) => write!(f, "{}", op),
            Self::Ident(s) | Self::Ty(s) => write!(f, "{}", s),
            Self::Ignore(_) => write!(f, ""),
            Self::Semicolon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::Unknown(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub(crate) variant: Tokv,
    /// Where this token starts in the plaintext
    pub(crate) index: usize,
    /// The size of this token in bytes
    pub(crate) len: usize,
}

impl Token {
    /// Create a new `Token`, from a `Tokv` and an `index`
    pub(crate) fn new(tok: impl Into<Tokv>, index: usize, len: usize) -> Self {
        Self {
            variant: tok.into(),
            index,
            len,
        }
    }

    /// For debugging -- display this token in the context of a given plaintext
    #[allow(unused)]
    pub fn display(&self, plaintext: &str) -> String {
        display_line(self.index, plaintext, |offset| {
            format!(
                "{}",
                format!(
                    "{}{} {:?}",
                    " ".repeat(offset),
                    "^".repeat(self.len),
                    self.variant
                )
                .red()
                .bold()
            )
            .into()
        })
    }

    /// Return the exact representation of this token in the file, currently 
    /// several branches are unimplemented and are only going to be added on
    /// an as-needed basis.
    /// 
    /// # Unimplemented
    /// if this is not an `Ident` or `Type`
    pub fn as_exact_str(&self) -> &str {
        match self.variant {
            Tokv::Ident(ref s) | Tokv::Ty(ref s) => s,
            _ => unimplemented!()
        }
    }
}

impl PartialEq<Tokv> for Token {
    fn eq(&self, other: &Tokv) -> bool {
        &self.variant == other
    }
}

