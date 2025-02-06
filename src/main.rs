use crate::eval::Evaluator;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{BufRead, Write};
use std::{env, fs, io};

mod builtin;
mod eval;
mod lexer;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        start_repl(&mut io::stdin().lock(), &mut io::stdout());
    } else {
        execute_script(args[1].as_str());
    }
}

fn execute_script(path: &str) {
    let src = fs::read_to_string(path).unwrap();
    let mut parser = Parser::new(Lexer::new(src.chars().collect()));
    let mut program = parser.parse().unwrap();
    let binding = env::current_dir().unwrap();
    let mut eval = Evaluator::new(Box::new(binding));
    while let Some(stmt) = program.pop() {
        eval.eval_stmt(stmt).unwrap();
    }
}

fn start_repl(input: &mut impl BufRead, output: &mut impl Write) {
    let binding = env::current_dir().unwrap();
    let mut evaluator = Evaluator::new(Box::new(binding));
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
            evaluator.eval_stmt(stmt).unwrap();
        }
    }
}
