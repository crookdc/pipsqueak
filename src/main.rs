use crate::lexer::Lexer;
use crate::parser::{Node, Parser};
use std::io;
use std::io::{BufRead, Write};

mod lexer;
mod parser;
mod eval;

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
        let mut parser = Parser::new(Lexer::new(cmd.chars().collect()));
        let mut program = parser.parse().unwrap();
        while let Some(stmt) = program.pop() {
            writeln!(output, "{}", stmt.literal()).unwrap();
        }
    }
}
