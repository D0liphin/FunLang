use crate::{ast::*, fmt::*, lex::*, typecheck::*};
use std::borrow::Cow;

use colored::Colorize;
use rustc_hash::FxHashMap;

/// # Note
/// We construct this type recursively internally and then clone it *once*
/// when we reach an unrecoverable error. This type is not currently recursive,
/// but if I ever decide to make it recursive, it would be a very good idea to
/// refactor this code considerably.
///
/// One way could be to use `MaybeBorrowed` and `MaybeDies`, where
/// `MaybeBorrowed` lets us take advantage of the possibility that our
/// [`TypeError`] might just be able to point into the map and `MaybeDies`
/// represents a lifetime that might live long enough and might not
///
/// Due to issues with Rust's borrow checker, after hours of trying to make
/// this work zerocopy, the time limit dawned on me and I just gave up trying
/// to figure out how to workaround that stupid crab polonius
#[derive(Debug, PartialEq)]
pub struct TypeError {
    pub token: Token,
    /// Obviously this is quite a big type, but it shouldn't be huge deal.
    pub expected: PartialType,
    pub description: Option<String>,
}

impl TypeError {
    pub fn display_err(&self, plaintext: &str) -> String {
        let mut err = String::with_capacity(256);

        // something like "error: lexing error" will be printed
        err += &format!("{}{} ", "error".red().bold(), ":".bold());
        let error_reason = if let PartialType::Unknown = self.expected {
            format!("while resolving types").bold()
        } else {
            format!("expected {}", self.expected).bold()
        };
        err += &format!("{error_reason}");
        err += "\n";

        err += &display_line(self.token.index, plaintext, |caret_offset| {
            let mut s = String::with_capacity(64);
            s += &" ".repeat(caret_offset);
            s += &format!(
                "{}",
                format!(
                    "{} {}",
                    "^".repeat(self.token.len),
                    self.description
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

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
/// Scope in fun is very, very simple: we just have global scope and function
/// scope.
pub enum Scope<'a> {
    /// The variable is defined at global scope
    Global,
    /// The variable is defined in the parameter list of a function, with the
    /// specified name
    Def(&'a str),
}

impl<'a> Scope<'a> {
    /// Returns the next strictest scope, or `None` if there are no further
    /// levels of scope to check
    fn upgrade(self) -> Option<Self> {
        match self {
            Self::Def(..) => Some(Self::Global),
            Self::Global => None,
        }
    }
}

/// A scoped type, this is only going to be used with a `&str`.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ScopedStr<'a> {
    pub scope: Scope<'a>,
    pub value: &'a str,
}

#[derive(Debug, Clone)]
pub enum ScopedString {
    Global(String),
    Local(String),
}

impl ScopedString {
    pub fn new(tchecker: &TypeChecker, s: ScopedStr) -> Self {
        // TODO: this is bad code because we are not accounting for possible
        //deeper scopes in the future
        match s.scope {
            Scope::Global => ScopedString::Global(s.value.to_owned()),
            Scope::Def(..) => {
                if tchecker.ident_tlookup.contains_key(&s) {
                    ScopedString::Local(s.value.to_owned())
                } else {
                    ScopedString::Global(s.value.to_owned())
                }
            }
        }
    }
}

impl<'a> ScopedStr<'a> {
    fn new(scope: Scope<'a>, value: &'a str) -> Self {
        Self { scope, value }
    }

    /// Create a [`ScopedStr`] with global scope
    fn global(value: &'a str) -> Self {
        Self {
            scope: Scope::Global,
            value,
        }
    }

    /// Create a [`ScopedStr`] wtih the specified def scope
    fn param(fn_name: &'a str, ident: &'a str) -> Self {
        Self {
            scope: Scope::Def(fn_name),
            value: ident,
        }
    }
}

pub type Tlookup<'a> = FxHashMap<ScopedStr<'a>, PartialType>;

#[derive(Debug)]
pub struct TypeChecker<'tokstream> {
    /// Allows for lookup of the type for a given identifier, this is scoped
    /// using the [`ScopedStr`] struct
    pub(crate) ident_tlookup: Tlookup<'tokstream>,
    /// Look up the actual type of a typename. This is only for primitives at
    /// the moment, since users cannot define functions
    pub(crate) typename_tlookup: Tlookup<'tokstream>,
}

impl<'tokstream> TypeChecker<'tokstream> {
    /// Include all intrinsic types, that pretty much just means primitive
    /// types: `Char`, `Void`, `Double`, `Int` and `String`
    pub fn new() -> Self {
        let mut this = Self {
            ident_tlookup: Default::default(),
            typename_tlookup: Default::default(),
        };
        // include builtin types
        this.typename_tlookup.extend([
            (ScopedStr::global(TYPENAME_PDOUBLE), PDOUBLE),
            (ScopedStr::global(TYPENAME_PINT), PINT),
            (ScopedStr::global(TYPENAME_PBOOL), PBOOL),
            (ScopedStr::global(TYPENAME_PSTRING), PSTATIC_STRING),
            (ScopedStr::global(TYPENAME_PVOID), PVOID),
        ]);
        this
    }

    /// look for `s` in upgrading scopes, until we cannot find it.
    fn get_type_of_str_from<'a>(
        tlookup: &'a Tlookup,
        mut scopedstr: ScopedStr<'a>,
    ) -> Option<&'a PartialType> {
        loop {
            match tlookup.get(&scopedstr) {
                Some(ty) => {
                    return Some(ty);
                }
                None => match scopedstr.scope.upgrade() {
                    Some(scope) => {
                        scopedstr.scope = scope;
                    }
                    None => return None,
                },
            }
        }
    }

    /// Get the type of a token that can have an associated type. At the moment
    /// this is exclusive to
    ///
    /// - `Tokv::Ident(..)`
    /// - `Tokv::Type(..)`
    ///
    /// # Errors
    /// If the type does not exist in its appropriate lookup dict
    fn get_type<'a>(
        &'a self,
        scope: Scope<'a>,
        token: &'a Token,
        expecting: &PartialType,
    ) -> Result<&PartialType, TypeError> {
        match &token.variant {
            Tokv::Ident(ident) => {
                match Self::get_type_of_str_from(&self.ident_tlookup, ScopedStr::new(scope, ident))
                {
                    Some(ty) => Ok(ty),
                    None => Err(TypeError {
                        token: token.clone(),
                        expected: expecting.clone(),
                        description: Some("undeclared identifier".into()),
                    }),
                }
            }
            Tokv::Ty(ty) => {
                match Self::get_type_of_str_from(&self.typename_tlookup, ScopedStr::new(scope, ty))
                {
                    Some(ty) => Ok(ty),
                    None => Err(TypeError {
                        token: token.clone(),
                        expected: expecting.clone(),
                        description: Some("undefined type".into()),
                    }),
                }
            }
            v => panic!("cannot look up the type of a {v:?} in the ident_lookup"),
        }
    }

    fn compute_any_explicit_call_type(
        &mut self,
        fn_ty: PartialType,
        args: &Vec<Expr>,
        repr: &Token,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        let PartialType::Partial(FunType::Callable(paramts, ret)) = fn_ty else {
            return Err(TypeError {
                token: repr.clone(),
                expected: PartialType::Unknown,
                description: Some(format!("not a Callable")),
            });
        };
        if args.len() != paramts.len() {
            return Err(TypeError {
                token: repr.clone(),
                expected: PartialType::Unknown,
                description: Some(format!(
                    "expected {} args, found {}",
                    paramts.len(),
                    args.len()
                )),
            });
        }

        let mut llvm_args = Vec::with_capacity(paramts.len());
        for (i, (arg, paramt)) in args.into_iter().zip(paramts).enumerate() {
            let (argt, texpr) = self.compute_expr_type(arg, scope)?;
            llvm_args.push(texpr);
            if argt != paramt {
                let description = Some(format!(
                    "argument at index {i} should be of type {paramt} but it is of type {argt}"
                ));
                return Err(TypeError {
                    token: repr.clone(),
                    expected: paramt,
                    description,
                });
            }
        }

        let call = TExpr::Call {
            ident: repr.as_exact_str().to_owned().into(),
            args: llvm_args,
            evalt: LlvmType::from_valid_partial(&ret),
        };
        Ok((*ret, call))
    }

    /// Compute the type for the call of a user-defined function
    fn compute_ident_explicit_call_type(
        &mut self,
        callable: &Callable,
        args: &Vec<Expr>,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        let fn_ty = self
            .get_type(scope, callable.repr, &PartialType::Unknown)?
            .clone();
        self.compute_any_explicit_call_type(fn_ty, args, callable.repr, scope)
    }

    /// Compute the true type of a `write()` call, the possible results are
    ///
    /// ```no_run
    /// Callable(Int, Bool) -> Void
    /// Callable(String) -> Void
    /// Callable(Double) -> Void
    /// ```
    pub fn compute_write_type(
        &mut self,
        write_repr: &Token,
        params: &Vec<Expr>,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        // validate that all parameters are known
        let mut paramts = Vec::with_capacity(params.len());
        let mut llvm_params = Vec::with_capacity(params.len());
        for (i, param) in params.iter().enumerate() {
            let (t, texpr) = self.compute_expr_type(param, scope)?;
            llvm_params.push(texpr);
            match t {
                PartialType::Partial(variant) => paramts.push(variant),
                // or produce an error about a type not being inferable
                PartialType::Unknown => {
                    return Err(TypeError {
                        token: write_repr.clone(),
                        expected: PartialType::Unknown,
                        description: Some(format!("cannot infer type of argument at index {}", i)),
                    });
                }
            }
        }

        // resolve overloads to their correct type
        match &paramts[..] {
            [FunType::PInt, FunType::PBool] => Ok((
                PVOID,
                TExpr::Call {
                    ident: BUILTIN_IWRITE.into(),
                    args: llvm_params,
                    evalt: LlvmType::Void,
                },
            )),
            [FunType::PDouble] => Ok((
                PVOID,
                TExpr::Call {
                    ident: BUILTIN_FWRITE.into(),
                    args: llvm_params,
                    evalt: LlvmType::Void,
                },
            )),
            [FunType::PStaticString] => Ok((
                PVOID,
                TExpr::Call {
                    ident: BUILTIN_SWRITE.into(),
                    args: llvm_params,
                    evalt: LlvmType::Void,
                },
            )),
            // or produce an error about no this overload not existing
            _ => Err(TypeError {
                token: write_repr.clone(),
                expected: PartialType::Unknown,
                description: Some(format!(
                    "no such function write({})",
                    paramts
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<String>>()
                        .join(", "),
                )),
            }),
        }
    }

    /// Compute the type of a `Expr::ExplicitCall`
    pub fn compute_explicit_call_type(
        &mut self,
        callable: &Callable,
        params: &Vec<Expr>,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        match callable.id {
            CallableId::Write => self.compute_write_type(callable.repr, params, scope),
            CallableId::Ident => self.compute_ident_explicit_call_type(callable, params, scope),
            CallableId::Op(op) => self.compute_op_explicit_call_type(callable, params, op, scope),
        }
    }

    /// Compute the type of a seq expr
    pub fn compute_seq_expr_type(
        &mut self,
        exprs: &Vec<Expr>,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        let mut final_ty = PartialType::Unknown;
        let mut texprs = Vec::with_capacity(exprs.len());
        for expr in exprs.iter() {
            let (t, texpr) = self.compute_expr_type(expr, scope)?;
            texprs.push(texpr);
            final_ty = t;
        }

        let seq = TExpr::Seq {
            exprs: texprs,
            evalt: LlvmType::from_valid_partial(&final_ty),
        };
        Ok((final_ty, seq))
    }

    /// Never fails -- literal types can always be known
    pub fn compute_literal_expr_type(
        &mut self,
        literal: &Literal,
    ) -> Result<(PartialType, TExpr), TypeError> {
        let Tokv::Literal(ref lit) = literal.repr.variant else {
            // this means we are marking something else as a void literal, even
            // when it's not... my code is going downhill!
            return Ok((
                PVOID,
                TExpr::Literal {
                    literal: TLiteral::Void,
                    evalt: LlvmType::Void,
                },
            ));
        };

        let t = PartialType::from(lit);
        let evalt = LlvmType::from_valid_partial(&t);
        Ok((
            t,
            TExpr::Literal {
                literal: TLiteral::from_literal(literal),
                evalt,
            },
        ))
    }

    pub fn compute_ident_expr_type<'a>(
        &'a mut self,
        ident: &'a Ident,
        scope: Scope<'a>,
    ) -> Result<(PartialType, TExpr), TypeError> {
        let t = self
            .get_type(scope, ident.repr, &PartialType::Unknown)
            .map(|t| t.clone())?;

        Ok(match &t {
            PartialType::Unknown => {
                return Err(TypeError {
                    token: ident.repr.clone(),
                    expected: PartialType::Unknown,
                    description: Some("type cannot be infered".to_owned()),
                })
            }
            PartialType::Partial(FunType::Callable(_, ret)) => {
                let ret = (**ret).clone();
                let call = TExpr::Call {
                    ident: ident.repr.as_exact_str().to_owned().into(),
                    args: vec![],
                    evalt: LlvmType::from_valid_partial(&ret),
                };
                (ret, call)
            }
            _ => {
                let read = TExpr::Read {
                    ident: ScopedString::new(
                        self,
                        ScopedStr::new(scope, ident.repr.as_exact_str()),
                    ),
                    // t should be a valid partial because it is not a callable
                    // or unknown
                    evalt: LlvmType::from_valid_partial(&t),
                };
                (t, read)
            }
        })
    }

    pub fn compute_if_then_else_type(
        &mut self,
        condition_repr: &Token,
        _then_repr: &Token,
        else_repr: &Token,
        condition: &Expr,
        then_expr: &Expr,
        else_expr: &Expr,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        let (conditiont, condition_texpr) = self.compute_expr_type(condition, scope)?;
        if conditiont != PBOOL {
            return Err(TypeError {
                token: condition_repr.clone(),
                expected: PBOOL,
                description: Some("the condition of this if expression".into()),
            });
        }

        let (thent, then_texpr) = self.compute_expr_type(then_expr, scope)?;
        let (elset, else_texpr) = self.compute_expr_type(else_expr, scope)?;
        if thent != elset {
            let description = Some(format!(
                "then block is of type {}, but else block is of type {}",
                thent, elset
            ));
            return Err(TypeError {
                token: else_repr.clone(),
                expected: thent,
                description,
            });
        }

        Ok((
            elset,
            TExpr::IfThenElse {
                condition: condition_texpr.boxed(),
                then_block: then_texpr.boxed(),
                else_block: else_texpr.boxed(),
                evalt: LlvmType::from_valid_partial(&thent),
            },
        ))
    }

    /// Evaluates the type of an expression, or returns a type error if it
    /// cannot be resolved
    pub fn compute_expr_type(
        &mut self,
        expr: &Expr,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        match expr {
            // we need to force a clone here, even if it's deep. It doesn't
            // really matter in the grand scheme of things... I hope
            Expr::Literal(literal) => self.compute_literal_expr_type(literal),
            Expr::ExplicitCall { callable, params } => {
                self.compute_explicit_call_type(callable, &params, scope)
            }
            Expr::Seq(exprs) => self.compute_seq_expr_type(exprs, scope),
            Expr::IfThenElse {
                condition_repr,
                then_repr,
                else_repr,
                condition,
                then_expr,
                else_expr,
            } => self.compute_if_then_else_type(
                condition_repr,
                then_repr,
                else_repr,
                condition,
                then_expr,
                else_expr,
                scope,
            ),
            Expr::Ident(ident) => self.compute_ident_expr_type(ident, scope),
        }
    }

    /// Compute the type of an operator explicit call. This is handled as a
    /// special case, even though we don't need to for better error messages
    ///
    /// Operator expressions are of the form
    ///
    /// ```plaintext
    /// {a} {op} {b}
    /// ```
    ///
    /// # Errors
    /// - `{a}` is a valid type for some specialization of `{op}`, but `{b}` is
    ///   not and vice versa
    /// - Neither `{a}` nor `{b}` are valid for any specialization of `{op}`
    /// - `{op}` is not callable at all
    /// - `{a}` or `{b}` has  an unknown type
    fn compute_op_explicit_call_type(
        &mut self,
        callable: &Callable,
        params: &Vec<Expr>,
        op: OpToken,
        scope: Scope,
    ) -> Result<(PartialType, TExpr), TypeError> {
        /// Finds the right overload
        macro_rules! numerical_ops {
            (
                $self:ident,
                $lhs:expr,
                $rhs:expr,
                {$($pattern:pat => ($ty:expr, $texpr:expr)),*$(,)?}
            ) => {{
                match ($lhs, $rhs) {
                    (
                        PartialType::Partial(lhs_ty),
                        PartialType::Partial(rhs_ty),
                    ) => match (lhs_ty, rhs_ty) {
                        $(
                        // these are the only valid calls on these operators
                        // (or any operator actually)
                        ($pattern, $pattern) => Ok(($ty, $texpr)),
                        )*
                        $(
                        // both of these are bad and should error, but we have
                        // a little type information about what we were
                        // expecting on the rhs
                        ($pattern, _) => Err(TypeError {
                            token: callable.repr.clone(),
                            expected: $ty,
                            description: Some(
                                format!(
                                    "lhs is of type {}, but rhs is of type {}",
                                    $lhs,
                                    $rhs,
                                ).into()
                            ),
                        }),
                        )*
                        _ => Err(TypeError {
                            token: callable.repr.clone(),
                            expected: PartialType::Unknown,
                            description: Some(
                                format!("cannot {} {} {}", lhs_ty, op, rhs_ty)
                                    .into(),
                            ),
                        }),
                    },
                    // this should never happen in this version of fun because
                    // we don't do any type inference, but if we allow this in
                    // the future, we have it covered!
                    _ => Err(TypeError {
                        token: callable.repr.clone(),
                        expected: PartialType::Unknown,
                        description: Some(format!("unable to infer operand types").into()),
                    }),
                }
            }}
        }

        let lhs_expr = &params[0];
        let rhs_expr = &params[1];
        let (lhs_ty, lhs_texpr) = self.compute_expr_type(lhs_expr, scope)?;
        let (rhs_ty, rhs_texpr) = self.compute_expr_type(rhs_expr, scope)?;

        match op {
            OpToken::Add | OpToken::Sub | OpToken::Mod | OpToken::Div | OpToken::Mul => {
                let result = numerical_ops!(self, &lhs_ty, &rhs_ty, {
                    FunType::PDouble => (PDOUBLE, TExpr::Call {
                        ident: Cow::Borrowed(op.as_builtin(&FunType::PDouble)),
                        args: vec![lhs_texpr, rhs_texpr],
                        // NB: this needs to be changed if PDouble becomes
                        // something else
                        evalt: LlvmType::Double,
                    }),
                    FunType::PInt => (PINT, TExpr::Call {
                        ident: Cow::Borrowed(op.as_builtin(&FunType::PInt)),
                        args: vec![lhs_texpr, rhs_texpr],
                        // NB: this needs to be changed if PInt becomes
                        // something else
                        evalt: LlvmType::Int(32),
                    }),
                });
                result
            }
            OpToken::DblEq
            | OpToken::Neq
            | OpToken::Lt
            | OpToken::Gt
            | OpToken::Leq
            | OpToken::Geq => numerical_ops!(self, &lhs_ty, &rhs_ty, {
                FunType::PInt => (PBOOL, TExpr::Call {
                    ident: Cow::Borrowed(op.as_builtin(&FunType::PInt)),
                    args: vec![lhs_texpr, rhs_texpr],
                    evalt: LLVM_BOOL,
                }),
                FunType::PDouble => (PBOOL, TExpr::Call {
                    ident: Cow::Borrowed(op.as_builtin(&FunType::PDouble)),
                    args: vec![lhs_texpr, rhs_texpr],
                    evalt: LLVM_BOOL,
                }),
            }),
            OpToken::Eq => Err(TypeError {
                expected: PartialType::Unknown,
                token: callable.repr.clone(),
                description: Some("not callable".into()),
            }),
        }
    }

    /// Type check a const expression, they look something like this
    ///
    /// ```plaintext
    /// val {ident}: {ty} = {value};
    /// ```
    ///
    /// # Errors
    /// - `{ty}` does not match `typeof({value})`
    /// - `{ty}` is undeclared
    ///
    /// # Returns
    /// A [`TastNode`]` `Const` that represents this [`AstNode`]
    pub fn type_check_const<'a>(
        &mut self,
        ty: &'a Type,
        ident: &'a Ident<'tokstream>,
        value: &'a Literal<'a>,
    ) -> Result<TastNode, TypeError> {
        let decl_ty = self
            .get_type(Scope::Global, &ty.repr, &PartialType::Unknown)?
            .clone();
        let (expr_ty, ..) = self.compute_literal_expr_type(value)?;
        let name = ident.repr.as_exact_str();

        if decl_ty == expr_ty {
            self.add_new_ident(ident.repr, ScopedStr::global(&name), decl_ty)?;
            let tnode = TastNode::Const(TConst {
                ident: name.to_owned(),
                val: TLiteral::from_literal(value),
            });
            Ok(tnode)
        } else {
            // The type exists, but it does not match the type we
            // specified
            Err(TypeError {
                token: value.repr.clone(),
                expected: decl_ty.clone(),
                description: Some(
                    format!(
                        "expected this to have type {}, found {}",
                        ty.repr.variant, expr_ty
                    )
                    .into(),
                ),
            })
        }
    }

    /// add `ident` to the lookup, higlight `tok` if it fails
    fn add_new_ident(
        &mut self,
        tok: &Token,
        ident: ScopedStr<'tokstream>,
        ty: PartialType,
    ) -> Result<(), TypeError> {
        if self.ident_tlookup.insert(ident, ty).is_some() {
            Err(TypeError {
                token: tok.clone(),
                expected: PartialType::Unknown,
                description: Some("already declared at this scope".into()),
            })
        } else {
            Ok(())
        }
    }

    /// Update the ident_tlookup with the a given function, ignoring the body
    /// of the function
    fn insert_function_signature(
        &mut self,
        signature: &FnSignature<'tokstream>,
    ) -> Result<(), TypeError> {
        let fn_name = signature.ident.repr.as_exact_str();
        let scope = Scope::Def(fn_name);

        // 1. produce a list of paramter types and llvm types
        let mut paramts = Vec::with_capacity(signature.params.len());
        for param in signature.params.iter() {
            let paramt = self
                .get_type(scope, param.ty.repr, &PartialType::Unknown)?
                .clone();
            self.add_new_ident(
                param.ident.repr,
                ScopedStr::param(fn_name, &param.ident.repr.as_exact_str()),
                paramt.clone(),
            )?;

            paramts.push(paramt);
        }

        // we need to add this now so that we can do recursion!
        let ret_ty: PartialType = self
            .get_type(scope, signature.ret.repr, &PartialType::Unknown)?
            .clone()
            .into();
        self.add_new_ident(
            signature.ident.repr,
            ScopedStr::global(fn_name),
            PartialType::Partial(FunType::Callable(paramts, Box::new(ret_ty.clone()))),
        )?;

        Ok(())
    }

    /// Type check a def's body. You first need to make sure the signature is
    /// available by calling [`TypeChecker::insert_function_signature`]
    ///
    /// # Returns
    /// A [`TastNode`] representing this [`AstNode`]
    fn type_check_def(
        &mut self,
        signature: &FnSignature<'tokstream>,
        expr: &Expr,
    ) -> Result<TastNode, TypeError> {
        let fn_name = signature.ident.repr.as_exact_str();
        let scope = Scope::Def(fn_name);

        let mut llvm_params = Vec::with_capacity(signature.params.len());
        for param in signature.params.iter() {
            let paramt = self
                .get_type(scope, param.ty.repr, &PartialType::Unknown)?
                .clone();
            let llvm_paramt = LlvmType::try_from_partial(&paramt, param.ty.repr)?;
            llvm_params.push(TParam {
                ty: llvm_paramt,
                ident: param.ident.repr.as_exact_str().to_owned(),
            });
        }

        let (body_ty, texpr) = self.compute_expr_type(expr, scope)?;
        let fn_type =
            Self::get_type_of_str_from(&mut self.ident_tlookup, ScopedStr::global(fn_name))
                .expect("added all functions on TypeChecker creation");
        let PartialType::Partial(FunType::Callable(.., ret_ty)) = fn_type else {
            unreachable!("function signatures should all be Callable");
        };

        if ret_ty.as_ref() != &body_ty {
            Err(TypeError {
                expected: ret_ty.as_ref().clone(),
                token: signature.ret.repr.clone(),
                description: Some(format!("function body is of type {body_ty}")),
            })
        } else {
            Ok(TastNode::Def(TDef {
                ident: fn_name.to_owned(),
                params: llvm_params.clone(),
                body: texpr,
                ret: LlvmType::from_valid_partial(&ret_ty),
            }))
        }
    }

    pub fn type_check_node(&mut self, node: &AstNode<'tokstream>) -> Result<TastNode, TypeError> {
        match node {
            AstNode::Def { signature, body } => self.type_check_def(signature, body),
            AstNode::Const { ty, value, ident } => self.type_check_const(ty, ident, value),
            AstNode::Expr(expr) => {
                let (.., texpr) = self.compute_expr_type(expr, Scope::Global)?;
                Ok(TastNode::Expr(texpr))
            }
        }
    }

    pub fn type_check(&mut self, ast: &Vec<AstNode<'tokstream>>) -> Result<Tast, TypeError> {
        let mut tnodes = Vec::with_capacity(ast.len());
        // first pass: add all the function signatures
        // TODO: do this for constants as well
        for node in ast.iter() {
            match node {
                AstNode::Def { signature, .. } => {
                    self.insert_function_signature(signature)?;
                }
                _ => (),
            }
        }
        for node in ast.iter() {
            let tnode = self.type_check_node(node)?;
            tnodes.push(tnode);
        }
        Ok(Tast::new(tnodes))
    }
}

#[cfg(test)]
mod tests {
    use crate::{lex::*, parse::*, ast::*, typecheck::*};

    const PLAINTEXT: &'static str = r#"
// Towers of Hanoi in Fun

def print_int(n: Int): Void = ...;
def print_char(ch: Int): Void = ...;
def skip(): Void = ...;
def print_space(): Void = ...;
def print_star(): Void = ...;
def new_line(): Void = ...;

// Mandelbrot program (without character constants)

val Ymin: Double = -1.3;
val Ymax: Double =  1.3;
val Ystep: Double = 0.05;  //0.025;

val Xmin: Double = -2.1;
val Xmax: Double =  1.1;
val Xstep: Double = 0.02;  //0.01;

val Maxiters: Int = 1000;

def m_iter(m: Int, x: Double, y: Double,
                   zr: Double, zi: Double) : Void = {
  if Maxiters <= m
  then print_star() 
  else {
    if 4.0 <= zi*zi+zr*zr then print_space() 
    else m_iter(m + 1, x, y, x+zr*zr-zi*zi, 2.0*zr*zi+y) 
  }
};

def x_iter(x: Double, y: Double) : Void = {
  if x <= Xmax
  then { m_iter(0, x, y, 0.0, 0.0) ; x_iter(x + Xstep, y) }
  else skip()
};

def y_iter(y: Double) : Void = {
  if y <= Ymax
  then { x_iter(Xmin, y) ; new_line() ; y_iter(y + Ystep) }
  else skip() 
};    


y_iter(Ymin)
"#;

    #[test]
    fn typechecks() {
        let tokstream = Lexer::new(&PLAINTEXT).into_token_stream(true).unwrap();
        let (_, ast) = Parser::parse_ast(crate::parse::TokenStream(&tokstream)).unwrap();
        println!("{}", display_ast(&ast));
        let result = TypeChecker::new().type_check(&ast);
        _ = dbg!(result);
        // println!("{}\n", err.display_err(PLAINTEXT));
    }
}
