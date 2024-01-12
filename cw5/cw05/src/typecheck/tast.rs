use std::{
    borrow::Cow,
    cell::Cell,
    fmt::{self, Write},
};

use rustc_hash::FxHashMap;

use crate::{
    ast::*,
    lex::{CharInt, LiteralToken, Token, Tokv},
    typecheck::*,
    wprintln,
};

pub trait BoxedExt {
    fn boxed(self) -> Box<Self>;
}

impl<T> BoxedExt for T {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

fn write_delimited<T>(
    out: &mut String,
    slice: &[T],
    callback: impl Fn(&mut String, &T),
    open: &str,
    delimiter: &str,
    close: &str,
    nothing: &str,
) {
    if slice.len() == 0 {
        out.push_str(nothing);
        return;
    }
    out.push_str(open);
    for item in &slice[..slice.len() - 1] {
        callback(out, item);
        out.push_str(delimiter);
    }
    callback(out, &slice[slice.len() - 1]);
    out.push_str(close)
}

pub trait DisplayLlvm {
    /// Write this as valid llvm-ir to a buffer, this might not mean the buffer
    /// contains a valid llvm file. It could be incomplete. A `CompilerContext`
    /// is passed as argument. This `CompilerContext` should be valid for the
    /// context that this function is called in
    fn write_llvm_ir(&self, out: &mut String, context: &mut CompilerContext) -> fmt::Result;
}

impl LlvmType {
    fn write_llvm_ir(&self, out: &mut String) -> fmt::Result {
        match self {
            Self::Int(bits) => out.write_fmt(format_args!("i{bits}")),
            Self::Double => out.write_str("double"),
            Self::LiteralStructure(tys) => {
                write_delimited(
                    out,
                    &tys,
                    |out, ty| _ = ty.write_llvm_ir(out),
                    "{ ",
                    ", ",
                    " }",
                    "{}",
                );
                Ok(())
            }
            Self::Void => out.write_str("void"),
            Self::Ptr => out.write_str("ptr"),
            Self::Array(n, t) => {
                write!(out, "[{n} x ")?;
                t.write_llvm_ir(out)?;
                write!(out, "]")
            }
        }
    }
}

/// LlvmType, representing only the required
#[derive(Clone, Debug, PartialEq)]
pub enum LlvmType {
    /// n-bit integer, e.g.
    ///
    /// ```no_run
    /// LlvmType::Int(32) == LlvmType::from("i32")
    /// ```
    Int(u32),
    Double,
    /// Represents a 'literal structure' in LLVM -- a structure that is not
    /// identified. Literals are structurally uniqued.
    LiteralStructure(Vec<LlvmType>),
    /// Llvm's opaque pointer type `ptr`
    Ptr,
    /// Llvm's nothing type `void`
    Void,
    Array(usize, Box<LlvmType>),
}

impl LlvmType {
    /// Get the llvm representation for this type or fail if
    /// - it is not yet implemented (will emit "invalid type" error)
    /// - it is not known
    ///
    /// # Returns
    /// An [`LlvmType`] representing this, or an appropriate [`TypeError`]
    /// highlighting `token` as being invalid.
    pub fn try_from_partial(ty: &PartialType, token: &Token) -> Result<Self, TypeError> {
        match ty {
            PartialType::Partial(..) => Ok(Self::from_valid_partial(ty)),
            _ => Err(TypeError {
                token: token.clone(),
                expected: PartialType::Unknown,
                description: Some(String::from("invalid type here")),
            }),
        }
    }

    pub fn from_partial(ty: &PartialType) -> Option<Self> {
        match ty {
            PartialType::Partial(FunType::PBool) => Some(LlvmType::Int(1)),
            PartialType::Partial(FunType::PInt) => Some(LlvmType::Int(32)),
            PartialType::Partial(FunType::PDouble) => Some(LlvmType::Double),
            PartialType::Partial(FunType::PStaticString) => Some(llvm_pstatic_string()),
            PartialType::Partial(FunType::PVoid) => Some(LlvmType::Void),
            _ => None,
        }
    }

    pub fn from_valid_partial(ty: &PartialType) -> Self {
        Self::from_partial(ty).expect("caller asserted valid")
    }
}

/// A literal value
///
/// # `DisplayLlvm` Implementation
/// Writes both the Llvm type name and the value, e.g `i32 5` or
/// `[2 x i8] c"ab"`
/// `Void` will write nothing.
#[derive(Debug, PartialEq, Clone)]
pub enum TLiteral {
    Int(i32),
    Double(f64),
    Bool(bool),
    String(String),
    Void,
}

/// Convert a byte to its string hex value, e.g. `226` -> `['E', '2']`
fn hex(b: u8) -> [char; 2] {
    let quotient = b / 16;
    let rem = b - quotient * 16;
    fn hexch(b: u8) -> char {
        let ch = match b {
            0..=9 => b'0' + b,
            10..=15 => b'A' + b - 10,
            _ => unreachable!(),
        };
        char::try_from(ch).expect("valid non-extended ascii")
    }
    [hexch(quotient), hexch(rem)]
}

impl TLiteral {
    fn write_llvm_ir(&self, out: &mut String) -> fmt::Result {
        match self {
            TLiteral::Int(val) => {
                LlvmType::Int(32).write_llvm_ir(out)?;
                write!(out, " {val}")
            }
            TLiteral::Bool(val) => {
                LlvmType::Int(1).write_llvm_ir(out)?;
                write!(out, " {val}")
            }
            TLiteral::Double(val) => {
                LlvmType::Double.write_llvm_ir(out)?;
                write!(out, " {val:?}")
            }
            TLiteral::String(string) => {
                LlvmType::Array(string.as_bytes().len(), LlvmType::Int(8).boxed())
                    .write_llvm_ir(out)?;
                write!(out, " c\"")?;
                for &b in string.as_bytes() {
                    let chs = hex(b);
                    out.push_str("\\");
                    out.extend(chs);
                }
                write!(out, "\"")
            }
            TLiteral::Void => out.write_str(""),
        }
    }
}

impl TLiteral {
    pub fn from_literal(literal: &Literal) -> Self {
        let Tokv::Literal(ref tokv) = literal.repr.variant else {
            panic!("Literal does not contain a Tokv::Literal")
        };

        match tokv {
            LiteralToken::Bool(bl) => TLiteral::Bool(*bl),
            LiteralToken::Int(CharInt::Char(ch)) => TLiteral::Int(*ch as i32),
            LiteralToken::Int(CharInt::Int(n)) => TLiteral::Int(*n as i32),
            LiteralToken::Double(db) => TLiteral::Double(*db),
            LiteralToken::String(s) => TLiteral::String(s.clone()),
            LiteralToken::Void => TLiteral::Void,
        }
    }
}

/// The whole `evalt` business was added later, and it was just so much easier
/// to add it to the variants than pull everything out of the struct and give
/// it a shared `evalt` field...
#[derive(Debug, Clone)]
pub enum TExpr {
    IfThenElse {
        condition: Box<TExpr>,
        then_block: Box<TExpr>,
        else_block: Box<TExpr>,
        evalt: LlvmType,
    },
    Literal {
        literal: TLiteral,
        evalt: LlvmType,
    },
    /// Read a value
    Read {
        ident: ScopedString,
        evalt: LlvmType,
    },
    Call {
        /// this is very likely to be a builtin, so we can save a lot of space
        /// by having this be a `Cow`
        ident: Cow<'static, str>,
        args: Vec<TExpr>,
        evalt: LlvmType,
    },
    Seq {
        exprs: Vec<TExpr>,
        evalt: LlvmType,
    },
}

impl TExpr {
    /// Patchwork garbage function
    pub fn evalt(&self) -> &LlvmType {
        match self {
            Self::IfThenElse { evalt, .. }
            | Self::Literal { evalt, .. }
            | Self::Read { evalt, .. }
            | Self::Call { evalt, .. }
            | Self::Seq { evalt, .. } => evalt,
        }
    }
}

/// Compile an llvm load instruction, loading a global into a local and
/// returning the local
fn compile_load_global(
    out: &mut String,
    globalt: &LlvmType,
    global: &str,
    context: &CompilerContext,
) -> String {
    let local = context.fresh_local();
    context.write_indent(out);
    _ = write!(out, "%{local} = load ");
    _ = globalt.write_llvm_ir(out);
    _ = write!(out, ", ptr @{global}\n");
    local
}

impl TExpr {
    /// Write the result of this expression to a temporary, returning the
    /// identifier for that local
    ///
    /// # Todo
    /// There is a very small performance improvmenet available here where we
    /// return a `Cow` instead of a `String`
    fn write_llvm_ir_to_local(&self, out: &mut String, context: &mut CompilerContext) -> String {
        match self {
            // load a global or pass through a local
            Self::Read { ident, evalt } => match ident {
                ScopedString::Global(global) => compile_load_global(out, evalt, global, context),
                ScopedString::Local(ident) => ident.clone(),
            },
            // call a function, creating temporaries for all arguments
            // if the function returns void, this will return an undef i1
            Self::Call { ident, args, evalt } => {
                // even if rust would let us no do this, we have to do this
                // anyway, so that all the locals are compiled before even
                // the first parenthesis is written
                let mut sargs = Vec::with_capacity(args.len());
                for arg in args {
                    sargs.push((arg.evalt(), arg.write_llvm_ir_to_local(out, context)));
                }
                // calls are alwaus global at the moment
                let local = context.fresh_local();
                context.write_indent(out);
                // we're doing a pretty weird hack here where we just make the
                // resulting register an undef i1 if the function returns void
                // Hopefully that works?
                if evalt == &LlvmType::Void {
                    // TODO: Void handling
                    // _ = write!(out, "%{local} = i1 undef\n");
                    // context.write_indent(out);
                    out.push_str("tail call ");
                } else {
                    _ = write!(out, "%{local} = tail call ");
                }
                _ = evalt.write_llvm_ir(out);
                _ = write!(out, " @{ident}");
                write_delimited(
                    out,
                    &sargs,
                    |out, (t, sarg)| {
                        _ = t.write_llvm_ir(out);
                        out.extend([" %", &sarg]);
                    },
                    "(",
                    ", ",
                    ")",
                    "()",
                );
                out.push_str("\n");
                local
            }
            Self::IfThenElse {
                condition,
                then_block,
                else_block,
                evalt,
            } => {
                let clocal = condition.write_llvm_ir_to_local(out, context);
                let elselabel = context.fresh_label();
                let thenlabel = context.fresh_label();
                let escapelabel = context.fresh_label();

                let rlocal = context.fresh_local();
                if evalt != &LlvmType::Void {
                    context.write_indent(out);
                    _ = write!(out, "%{rlocal} = alloca ");
                    _ = evalt.write_llvm_ir(out);
                    out.push_str("\n");
                }

                // escape with a certain value, does a store and returns code
                // execution back to other expressions
                let escape_with = |local: &str, out: &mut String, context: &mut CompilerContext| {
                    if evalt != &LlvmType::Void {
                        context.write_indent(out);
                        out.push_str("store ");
                        _ = evalt.write_llvm_ir(out);
                        out.push_str(" %");
                        out.push_str(local);
                        _ = write!(out, ", ptr %{rlocal}\n");
                    }

                    context.write_indent(out);
                    _ = write!(out, "br label %{escapelabel}\n");
                };

                context.write_indent(out);
                out.push_str("br ");
                _ = LLVM_BOOL.write_llvm_ir(out);
                _ = write!(out, " %{clocal}, label %{thenlabel}, label %{elselabel}\n");

                // then block
                _ = write!(out, "\n{thenlabel}:\n");
                let local = then_block.write_llvm_ir_to_local(out, context);
                escape_with(&local, out, context);

                // else block
                _ = write!(out, "\n{elselabel}:\n");
                let local = else_block.write_llvm_ir_to_local(out, context);
                escape_with(&local, out, context);

                // all labels return control flow here, so this is where we
                // create the output register
                _ = write!(out, "\n{escapelabel}:\n");
                if evalt != &LlvmType::Void {
                    let reg = context.fresh_local();
                    context.write_indent(out);
                    _ = write!(out, "%{reg} = load ");
                    _ = evalt.write_llvm_ir(out);
                    _ = write!(out, ", ptr %{rlocal}\n");

                    reg
                } else {
                    String::new()
                }
            }
            Self::Literal { literal, evalt } => match literal {
                TLiteral::String(string) => {
                    let fglobal = context.fresh_global();
                    let global = match context.string_pool.get(string) {
                        Some(global) => &global,
                        None => {
                            _ = context.string_pool.insert(string.clone(), fglobal.clone());
                            &fglobal
                        }
                    };

                    compile_load_global(out, evalt, global, context)
                }
                TLiteral::Void => {
                    // TODO: Void handling
                    // context.write_indent(out);
                    // _ = write!(out, "%{local} = i1 undef");
                    // out.push_str("\n");
                    String::new()
                }
                TLiteral::Int(n) => {
                    let local = context.fresh_local();
                    context.write_indent(out);
                    _ = write!(out, "%{local} = add ");
                    _ = LlvmType::Int(32).write_llvm_ir(out);
                    _ = write!(out, " 0, {n}\n");
                    local
                }
                TLiteral::Double(n) => {
                    let local = context.fresh_local();
                    context.write_indent(out);
                    _ = write!(out, "%{local} = fadd ");
                    _ = LlvmType::Double.write_llvm_ir(out);
                    _ = write!(out, " 0.0, {n:?}\n");
                    local
                }
                _ => {
                    let alloca_local = context.fresh_local();
                    let local = context.fresh_local();
                    // %alloca_local = alloca i1
                    context.write_indent(out);
                    _ = write!(out, "%{alloca_local} = alloca ");
                    _ = evalt.write_llvm_ir(out);
                    out.push_str("\n");
                    // store i1 false, ptr %alloca_local
                    context.write_indent(out);
                    out.push_str("store ");
                    _ = literal.write_llvm_ir(out);
                    _ = write!(out, ", ptr %{alloca_local}\n");
                    // %local = load i1, ptr %alloca_local
                    context.write_indent(out);
                    _ = write!(out, "%{local} = load ");
                    _ = evalt.write_llvm_ir(out);
                    _ = write!(out, ", ptr %{alloca_local}\n");
                    local
                }
            },
            Self::Seq { exprs, .. } => {
                let mut reg = String::new();
                for expr in exprs {
                    // the only time we actually use this function oof
                    reg = expr.write_llvm_ir_to_local(out, context);
                }
                reg
            }
        }
    }
}

impl DisplayLlvm for TExpr {
    fn write_llvm_ir(&self, out: &mut String, context: &mut CompilerContext) -> fmt::Result {
        let _ = self.write_llvm_ir_to_local(out, context);
        Ok(())
    }
}

/// A tuple `(LlvmType, String)` containing the type and identifier used for
/// a paramater to an LLVM function
#[derive(Debug, Clone, PartialEq)]
pub struct TParam {
    pub ty: LlvmType,
    pub ident: String,
}

impl TParam {
    fn write_llvm_ir(&self, out: &mut String) -> fmt::Result {
        self.ty.write_llvm_ir(out)?;
        write!(out, " %{}", self.ident)
    }
}

#[derive(Debug)]
pub struct TDef {
    // non @-prefixed identifier
    pub ident: String,
    pub params: Vec<TParam>,
    pub ret: LlvmType,
    pub body: TExpr,
}

impl DisplayLlvm for TDef {
    fn write_llvm_ir(&self, out: &mut String, context: &mut CompilerContext) -> fmt::Result {
        // reset locals
        context.reset_locals();
        // signature
        out.push_str("define fastcc ");
        self.ret.write_llvm_ir(out)?;
        write!(out, " @{}", self.ident)?;
        write_delimited(
            out,
            &self.params,
            |out, param| _ = param.write_llvm_ir(out),
            "(",
            ", ",
            ")",
            "()",
        );
        out.write_str(" {\n")?;
        // function body
        context.inc_indent();
        let reg = self.body.write_llvm_ir_to_local(out, context);
        if self.ret == LlvmType::Void {
            context.write_indent(out);
            out.write_str("ret void\n")?;
        } else {
            context.write_indent(out);
            out.write_str("ret ")?;
            self.ret.write_llvm_ir(out)?;
            write!(out, " %{}\n", reg)?;
        }
        // enifed
        out.write_str("}\n\n")?;
        context.reset_indent();
        Ok(())
    }
}

#[derive(Debug)]
pub struct TConst {
    /// non @-prefixed identifier
    pub ident: String,
    pub val: TLiteral,
}

impl DisplayLlvm for TConst {
    fn write_llvm_ir(&self, out: &mut String, context: &mut CompilerContext) -> fmt::Result {
        match &self.val {
            // special cased because we just insert this into the string pool
            TLiteral::String(string) => {
                context
                    .string_pool
                    .insert(string.clone(), self.ident.clone());
            }
            TLiteral::Bool(..) | TLiteral::Double(..) | TLiteral::Int(..) => {
                write!(out, "@{} = constant ", self.ident)?;
                self.val.write_llvm_ir(out)?;
            }
            TLiteral::Void => {
                wprintln!("void constants are undefined-behavior at the moment");
            }
        };
        out.write_str("\n\n")?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum TastNode {
    Def(TDef),
    Const(TConst),
    Expr(TExpr),
}

/// A typed abstract syntax tree, this tree should always be valid, so that we
/// can generate good LLVM!
///
/// This, despite the name is actually not *typed*, it is simply *type-checked*.
/// Actually, the whole structure is sufficient to perform a type-check, but
/// I am sort of patching up the unnecessary intermediary step here.
#[allow(unused)]
#[derive(Debug)]
pub struct Tast {
    nodes: Vec<TastNode>,
}

#[derive(Clone, Debug)]
pub struct CompilerContext {
    /// We could probably just as well use a global counter for everything, but
    /// this makes the generated IR much easier to read
    local_count: Cell<usize>,
    global_count: Cell<usize>,
    /// Maps actual strings -> theidentifier
    string_pool: FxHashMap<String, String>,
    indent: Cell<usize>,
}

impl CompilerContext {
    fn inc_indent(&self) {
        self.indent.set(self.indent.get() + 1);
    }

    fn fresh_label(&self) -> String {
        let label = format!("bb.{index}", index = self.local_count.get());
        self.local_count.set(self.local_count.get() + 1);
        label
    }

    fn fresh_local(&self) -> String {
        let local = format!("tmp.{index}", index = self.local_count.get());
        self.local_count.set(self.local_count.get() + 1);
        local
    }

    fn fresh_global(&self) -> String {
        let global = format!("GLOBAL.{index}", index = self.global_count.get());
        self.global_count.set(self.global_count.get() + 1);
        global
    }

    fn write_indent(&self, out: &mut String) {
        for _ in 0..self.indent.get() {
            _ = write!(out, "    "); // tabs are 4 spaces
        }
    }

    fn reset_locals(&self) {
        self.local_count.set(0);
    }

    fn reset_indent(&self) {
        self.indent.set(0);
    }
}

impl Tast {
    pub fn new(nodes: Vec<TastNode>) -> Self {
        Self { nodes }
    }

    /// Produces LLVM IR for this tree
    /// This takes the `ident_tlookup` from the type checker so that we can
    /// look up the types of all globals
    pub fn into_llvm_ir(&self) -> String {
        let mut context = CompilerContext {
            local_count: Cell::new(0),
            global_count: Cell::new(0),
            string_pool: FxHashMap::default(),
            indent: Cell::new(0),
        };

        let mut out = String::new();

        let mut composite_main_exprs = vec![];
        for node in self.nodes.iter() {
            match node {
                TastNode::Def(tdef) => _ = tdef.write_llvm_ir(&mut out, &mut context),
                TastNode::Const(tconst) => _ = tconst.write_llvm_ir(&mut out, &mut context),
                TastNode::Expr(texpr) => {
                    // TODO: we could improve performance with a swap remove
                    composite_main_exprs.push(texpr.clone());
                }
            }
        }

        // build the main method out of all the floating expressions
        composite_main_exprs.push(TExpr::Literal {
            literal: TLiteral::Void,
            evalt: LlvmType::Void,
        });
        let def_main = TDef {
            ident: "main".to_owned(),
            params: vec![],
            ret: LlvmType::Void,
            body: TExpr::Seq {
                exprs: composite_main_exprs,
                evalt: LlvmType::Void,
            },
        };
        _ = def_main.write_llvm_ir(&mut out, &mut context);

        // write strings to the end of the file
        for (string, ident) in context.string_pool {
            let len = string.len();
            // @sbuf.MY_STRING
            let sbufid = format!("sbuf.{}", ident);
            // @sbuf.MY_STRING = constant
            _ = write!(out, "\n@{sbufid} = constant ");
            // @sbuf.MY_STRING = constant [7 x i8] c"example"
            _ = TLiteral::String(string).write_llvm_ir(&mut out);
            // ...
            // @MY_STRING = constant
            _ = write!(out, "\n@{ident} = constant ", ident = ident);
            // @MY_STRING = constant { ptr, i64 }
            _ = llvm_pstatic_string().write_llvm_ir(&mut out);
            // @MY_STRING = constant { ptr, i64 } { ptr @sbuf.MY_STRING,
            _ = write!(out, " {{ ptr @{sbufid}, ");
            // ...ant { ptr, i64 } { ptr @sbuf.MY_STRING, i64
            _ = LLVM_ISIZE.write_llvm_ir(&mut out);
            // ...ant { ptr, i64 } { ptr @sbuf.MY_STRING, i64 7 }
            _ = write!(out, " {} }}\n", len);
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use super::Tast;
    use crate::{
        lex::Lexer,
        parse::{Parser, TokenStream},
        typecheck::TypeChecker,
    };

    /// might use for future tests
    #[allow(unused)]
    fn build_tast(plaintext: &'static str) -> Tast {
        let toks = Lexer::new(plaintext).into_token_stream(true).unwrap();
        let (_, ast) = Parser::parse_ast(TokenStream(&toks)).unwrap();
        TypeChecker::new().type_check(&ast).unwrap()
    }

    const TEST_CONST_LLVM_IR_PLAINTEXT: &'static str = r#"
val HELLO_WORLD: String = "Hello, world!";

def println(string: String): Void = {
    write(string);
    write('\n', true);
}

def main(): Void = {
    println("Hello, world!");
}
"#;

    #[test]
    fn test_const_llvm_ir() {
        let toks = Lexer::new(TEST_CONST_LLVM_IR_PLAINTEXT)
            .into_token_stream(true)
            .unwrap();
        let (_, ast) = Parser::parse_ast(TokenStream(&toks)).unwrap();
        let mut tchecker = TypeChecker::new();
        let tast = tchecker.type_check(&ast).unwrap();
        dbg!(&tast);
        println!("{}", tast.into_llvm_ir());
    }
}
