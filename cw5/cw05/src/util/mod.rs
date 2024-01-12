#[allow(unused)]
pub const PLAINTEXT: &'static str = include_str!("../../examples/intsqrt.fun");

/// Python-style `input()` function with formated strings
#[macro_export]
macro_rules! input {
    ($($tok:tt)*) => {{
        // use std::io::Write;
        // print!($($tok)*);
        // let _ = std::io::stdout().flush();
        // let mut line = String::new();
        // let _ = std::io::stdin().read_line(&mut line);
        // line
    }};
}

#[macro_export]
macro_rules! display_option_tok {
    ($option_tok:expr) => {{
        // if let Some(tok) = $option_tok {
        //     println!("{}", tok.display(crate::util::PLAINTEXT));
        // }
    }};
}

/// Print a warning to the user (this is used mainly during llvm compilation)
#[macro_export]
macro_rules! wprintln {
    ($($ftt:tt)*) => {{
        use colored::Colorize;
        eprintln!("{}{} {}", "warning".bold().yellow(), ":".bold(), format_args!($($ftt)*));
    }};
}
