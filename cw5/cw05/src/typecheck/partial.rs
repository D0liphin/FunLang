use crate::ast::CloneShallow;
use crate::{lex::*, typecheck::*};
use std::fmt;

/// All the type information `fun` currently uses for evaluating type
/// correctness. Traits do not exist in `fun`, so types are either primitive,
/// callable, or perhaps (in the future) arrays.
#[derive(Debug, Clone, PartialEq)]
pub enum FunType {
    /// UNUSED: At the moment, we can't pass Callables around: it is not in the
    /// specification... So no true higher order FP, I guess
    #[allow(unused)]
    Callable(Vec<PartialType>, Box<PartialType>),
    /// Primitve types
    PInt,
    PDouble,
    PBool,
    PStaticString,
    PVoid,
}

impl fmt::Display for FunType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Callable(params, ret) => write!(
                f,
                "Callable({}) -> {}",
                params
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(", "),
                ret
            ),
            Self::PBool => write!(f, "{}", TYPENAME_PBOOL),
            Self::PStaticString => write!(f, "{}", TYPENAME_PSTRING),
            Self::PVoid => write!(f, "{}", TYPENAME_PVOID),
            Self::PDouble => write!(f, "{}", TYPENAME_PDOUBLE),
            Self::PInt => write!(f, "{}", TYPENAME_PINT),
        }
    }
}

impl fmt::Display for PartialType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown => write!(f, "{{unknown}}"),
            Self::Partial(variant) => {
                write!(f, "{}", variant)
            }
        }
    }
}

/// Represents a type that may or maynot be fully qualified when expaneded...
/// actually, in fun all types are fully qualifed all the time, but I'm
/// still keeping it like this.
#[derive(Debug, Clone)]
pub enum PartialType {
    /// UNUSED: we need this if we ever want to do type inference
    #[allow(unused)]
    Unknown,
    Partial(FunType),
}

impl PartialType {
    #[allow(unused)]
    pub fn is_callable(&self) -> bool {
        matches!(self, PartialType::Partial(FunType::Callable(..)))
    }
}

impl PartialEq for PartialType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (_, Self::Unknown) | (Self::Unknown, _) => false,
            (Self::Partial(lhs), Self::Partial(rhs)) => lhs == rhs,
        }
    }
}

impl CloneShallow for PartialType {
    fn try_clone_shallow(&self) -> Option<Self> {
        match self {
            Self::Unknown => Some(self.clone()),
            Self::Partial(
                FunType::PInt
                | FunType::PDouble
                | FunType::PBool
                | FunType::PStaticString
                | FunType::PVoid,
            ) => Some(self.clone()),
            _ => None,
        }
    }
}

/// The primitive type `Bool`
pub const PBOOL: PartialType = PartialType::Partial(FunType::PBool);

/// The primitive type `Int`
pub const PINT: PartialType = PartialType::Partial(FunType::PInt);

/// The primitive type `String`
pub const PSTATIC_STRING: PartialType = PartialType::Partial(FunType::PStaticString);

/// The primitive type `Double`
pub const PDOUBLE: PartialType = PartialType::Partial(FunType::PDouble);

/// The primitive type `Void`
pub const PVOID: PartialType = PartialType::Partial(FunType::PVoid);

impl From<&LiteralToken> for PartialType {
    fn from(value: &LiteralToken) -> Self {
        match value {
            LiteralToken::Bool(..) => PBOOL,
            LiteralToken::Int(..) => PINT,
            LiteralToken::String(..) => PSTATIC_STRING,
            LiteralToken::Double(..) => PDOUBLE,
            LiteralToken::Void => PVOID,
        }
    }
}
