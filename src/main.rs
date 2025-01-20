use crate::lexer::Lexer;
use std::io;
use std::io::{BufRead, Write};

mod lexer;
mod parser;

fn main() {
    start_repl(&mut io::stdin().lock(), &mut io::stdout());
}

pub fn start_repl(input: &mut impl BufRead, output: &mut impl Write) {
    loop {
        write!(output, ">>").unwrap();
        output.flush().unwrap();

        let mut cmd = String::new();
        if let Err(e) = input.read_line(&mut cmd) {
            writeln!(output, "Error: {e}").unwrap();
        }
        for token in Lexer::new(cmd.chars().collect()) {
            writeln!(output, "{token:?}").unwrap();
        }
    }
}
