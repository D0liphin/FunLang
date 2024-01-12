use std::{
    ffi::OsString,
    fs,
    io::{self, Read, Write},
    path::PathBuf,
    process, time,
};

use crate::{
    ast::display_ast,
    fmt::highlight_syntax,
    parse::{Parser as FunParser, TokenStream},
    typecheck::TypeChecker,
    wprintln,
};
use crate::{ast::AstNode, lex::Lexer};

use colored::Colorize;
// use olistd::String as String;

use clap::{Parser, Subcommand};

pub enum Error {
    FileError(OsString),
    String(String),
}

impl Error {
    pub fn eprintln(&self) {
        eprint!("{}", "error: ".bold().bright_red());
        let colorstr = match self {
            Self::FileError(path) => format!("error handling file {:?}", path),
            Self::String(s) => s.into(),
        };
        eprintln!("{}", colorstr);
    }

    fn file_error(path: &PathBuf) -> Self {
        Error::FileError(path.clone().into_os_string())
    }
}

/// Load a file as a [`String`]
fn load_file(path: &PathBuf) -> Result<String, Error> {
    fn load_file_io_result(path: &PathBuf) -> io::Result<String> {
        let mut s = String::with_capacity(512);
        fs::File::open(path)?.read_to_string(&mut s)?;
        Ok(s)
    }

    load_file_io_result(path).map_err(|_| Error::file_error(path))
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Print a file with syntax highlighting
    Print {
        /// The file you wish to print
        path: PathBuf,
        /// Only print valid programs
        #[arg(long, default_value_t = false)]
        strict: bool,
        /// Print without line numbers
        #[arg(long, default_value_t = false)]
        nolnum: bool,
    },
    /// Show the AST for a file
    Ast {
        /// The file you with to show the AST of
        path: PathBuf,
    },
    /// Compile a .fun program
    Build {
        /// The file you wish to compile
        path: PathBuf,
        /// Automatically generate output files for this file stem -- e.g.
        /// 'fib.fun' will produce 'fib.ll' and 'fib'. Then run the output file.
        /// This will override other options.
        #[arg(long, default_value_t = false, verbatim_doc_comment)]
        auto: bool,
        /// Compile this program without std
        #[arg(long, default_value_t = false)]
        nostd: bool,
        /// The output file -- will completely write over any contents in this
        /// file
        #[arg(long, verbatim_doc_comment)]
        ll: Option<PathBuf>,
        /// Optionally also specify a binary file to output
        #[arg(long)]
        bin: Option<PathBuf>,
        /// Version of llvm to be used e.g. '17' for lli-17 opt-17 etc.
        #[arg(long)]
        llvm_version: Option<u8>,
    },
}

fn with_ast<F>(plaintext: &str, f: F) -> Result<(), Error>
where
    F: Fn(Vec<AstNode>) -> Result<(), Error>,
{
    let lexer = Lexer::new(&plaintext);
    let tokstream = lexer.into_token_stream(true);
    match tokstream {
        Ok(toks) => match FunParser::parse_ast(TokenStream(&toks)) {
            Ok((_, ast)) => {
                return f(ast);
            }
            Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
                eprintln!("{}", e.display_err(&plaintext));
            }
            Err(nom::Err::Incomplete(..)) => {
                // we never use this, so I don't see why it should
                // ever occur!
                println!("something truly horrible went wrong...");
            }
        },
        Err(e) => eprintln!("{}", e.display_err(&plaintext)),
    };
    Ok(())
}

impl Commands {
    pub fn exec(self) -> Result<(), Error> {
        match self {
            Self::Print {
                path,
                strict,
                nolnum,
            } => {
                let plaintext = load_file(&path)?;
                let lexer = Lexer::new(&plaintext);
                let tokstream = lexer.into_token_stream(strict);
                match tokstream {
                    Ok(toks) => println!("{}", highlight_syntax(&plaintext, &toks, !nolnum)),
                    Err(e) => eprintln!("{}", e.display_err(&plaintext)),
                }
                Ok(())
            }
            Self::Ast { path } => with_ast(&load_file(&path)?, |ast| {
                println!("{}", display_ast(&ast));
                Ok(())
            }),
            Self::Build {
                path,
                nostd,
                mut ll,
                mut bin,
                llvm_version,
                auto,
            } => {
                if auto {
                    let stem = match path.as_os_str().to_str() {
                        Some(newll) => newll.trim_end_matches(".fun").to_owned(),
                        None => {
                            return Err(Error::String(
                                "for --auto to work, filenames must be valid utf8".to_owned(),
                            ))
                        }
                    };
                    ll = Some(PathBuf::from(stem.clone() + ".ll"));
                    bin = Some(PathBuf::from(stem));
                }

                let ll = if let Some(ll) = ll {
                    ll
                } else {
                    wprintln!("did nothing because no target was specified");
                    return Ok(());
                };

                let earlier = time::Instant::now();
                if cfg!(not(target_pointer_width = "64")) {
                    wprintln!("compilation to LLVM is only available for 64-bit targets");
                    return Ok(());
                }

                let mut plaintext = String::new();
                // most janky thing I have ever done
                if !nostd {
                    plaintext.push_str("\n");
                    plaintext.push_str(include_str!("../../funstd/funstd.fun"));
                }
                plaintext.push_str(&load_file(&path)?);

                with_ast(&plaintext, |ast| {
                    let tast = match TypeChecker::new().type_check(&ast) {
                        Ok(tast) => tast,
                        Err(e) => {
                            eprintln!("{}", e.display_err(&plaintext));
                            return Ok(());
                        }
                    };

                    let mut ir = String::from(include_str!("../../funstd/funbuiltin64.ll"));
                    ir.push_str("\n");
                    ir.push_str(&tast.into_llvm_ir());

                    let mut file = fs::File::create(&ll).map_err(|_| Error::file_error(&ll))?;
                    _ = file.write_all(ir.as_bytes());

                    if let Some(obin) = &bin {
                        _ = process::Command::new(if let Some(v) = llvm_version {
                            format!("opt-{v}")
                        } else {
                            "opt".to_owned()
                        })
                        .arg(&ll)
                        .arg("-o")
                        .arg(obin)
                        .spawn()
                        .map_err(|_| Error::String("bad llvm version".to_owned()))?
                        .wait();
                    }

                    println!(
                        "{finished} fun llvm target(s) in {:?}",
                        time::Instant::now().duration_since(earlier),
                        finished = "Finished".bold().green()
                    );

                    if let Some(obin) = &bin {
                        if auto {
                            if cfg!(any(target_os = "linux", target_os = "macos")) {
                                _ = process::Command::new("chmod")
                                    .arg("777")
                                    .arg(obin)
                                    .spawn()
                                    .map_err(|_| {
                                        Error::String(
                                            "couldn't run chmod... maybe you need to use sudo?"
                                                .to_owned(),
                                        )
                                    })?
                                    .wait();
                                _ = process::Command::new(obin)
                                    .spawn()
                                    .map_err(|_| {
                                        Error::String(format!("failed executing {obin:?}"))
                                    })?
                                    .wait();
                            } else {
                                wprintln!(
                                    "cannot run the program -- you are not on a unix system!"
                                );
                            }
                        }
                    }

                    Ok(())
                })
            }
        }
    }
}
