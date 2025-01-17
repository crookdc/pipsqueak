use crate::lexer::Lexer;
use std::io::{BufRead, Write};

pub fn start(input: &mut impl BufRead, output: &mut impl Write) {
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
