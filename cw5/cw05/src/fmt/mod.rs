use crate::lex::*;
use colored::Colorize;
use std::string::String;

/// display a line, highlighting some information about it.
///
/// - `f` is a callback that takes an offset and returns a string to append
///   to the bottom. The offset is how many spaces you have to shift to
///   be directly below `index`.
pub(crate) fn display_line(index: usize, plaintext: &str, f: impl Fn(usize) -> String) -> String {
    let mut s = String::with_capacity(256);
    let mut line_number = 0;
    for (i, &b) in plaintext.as_bytes().iter().enumerate() {
        if i == index {
            break;
        }
        if b == b'\n' {
            line_number += 1;
        }
    }

    // the entire line at index and the offset from the start of that line
    let (line, caret_offset) = {
        let mut line_start = 0;
        let mut line_end = plaintext.len();
        for (i, &b) in plaintext[index..].as_bytes().iter().enumerate() {
            if b == b'\n' {
                line_end = index + i;
                break;
            }
        }
        for (i, &b) in plaintext[..index].as_bytes().iter().rev().enumerate() {
            if b == b'\n' {
                line_start = index - i;
                break;
            }
        }
        (&plaintext[line_start..line_end], index - line_start)
    };

    let line = highlight_syntax(
        line,
        &Lexer::new(line)
            .into_token_stream(false)
            .expect("all lines are lexable in fun"),
        false,
    );

    let bold_pipe = format!("{}", "|".blue().bold());
    let line_number = format!(" {} ", line_number);
    // fancy padding
    s += &" ".repeat(line_number.len());
    s += &bold_pipe;
    s += "\n";
    // line number
    s += &format!("{}", line_number.blue().bold());
    s += &bold_pipe;
    s += " ";
    // the actual line
    s += &line;
    s += "\n";
    // positioned caret and expected message
    s += &" ".repeat(line_number.len());
    s += &bold_pipe;
    s += &f(caret_offset + 1);

    s
}

fn push_line_start(line_number: usize, dst: &mut String) {
    if line_number > 0 {
        dst.push_str("\n");
    }
    dst.push_str(&format!(
        "{}",
        format!(" {: >3} | ", line_number).dimmed().blue()
    ));
}

/// push a highlighted char and also add some fancy line number stuff if it is
/// a new line..
fn push_highlighted_char(
    ch: char,
    dst: &mut String,
    line_number: &mut usize,
    display_line_numbers: bool,
) {
    if ch == '\n' {
        *line_number += 1;
        if display_line_numbers {
            push_line_start(*line_number, dst);
            return;
        }
    }
    dst.push(ch);
}

pub fn highlight_syntax(plaintext: &str, toks: &[Token], display_line_numbers: bool) -> String {
    // conservative capacity estimate, to be honest
    let mut s = String::with_capacity(plaintext.len() + toks.len() * 4);

    let mut buf = String::with_capacity(64);
    let mut i = 0;
    let mut line_number = 0;
    if display_line_numbers {
        push_line_start(line_number, &mut buf);
    }
    for tok in toks {
        loop {
            let b = plaintext.as_bytes()[i];

            if i == tok.index {
                s += &format!("{}", buf.dimmed().green());
                buf.clear();

                let tokstr = format!(
                    "{}",
                    match tok.variant {
                        Tokv::Literal(..) => format!("{}", tok.variant).green(),
                        Tokv::Keyword(..) => format!("{}", tok.variant).red(),
                        Tokv::Op(..) => format!("{}", tok.variant).cyan(),
                        Tokv::Colon
                        | Tokv::LBrace
                        | Tokv::RBrace
                        | Tokv::LParen
                        | Tokv::RParen
                        | Tokv::Semicolon => format!("{}", tok.variant).cyan().dimmed(),
                        Tokv::Ty(..) => format!("{}", tok.variant).yellow(),
                        Tokv::Unknown(..) => format!("{}", tok.variant)
                            .underline()
                            .on_bright_red()
                            .bold(),
                        _ => format!("{}", tok.variant).normal(),
                    }
                );
                s += &tokstr;
                i += tok.len;
                break;
            }

            push_highlighted_char(b as char, &mut buf, &mut line_number, display_line_numbers);
            i += 1;
        }
    }

    let mut buf = String::new();
    for &b in &plaintext.as_bytes()[i..] {
        push_highlighted_char(b as char, &mut buf, &mut line_number, display_line_numbers);
    }
    s += &format!("{}", buf.dimmed().green());

    s
}
