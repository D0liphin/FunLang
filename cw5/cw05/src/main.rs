mod ast;
mod commands;
mod fmt;
mod lex;
mod parse;
mod util;
mod typecheck;

mod olistd {
    // drop in SsoString when stabilised
}

use clap::Parser;
use commands::Cli;

fn main() {
    let cli = Cli::parse();
    if let Some(command) = cli.command {
        if let Err(e) = command.exec() {
            e.eprintln();
        }
    }
}
