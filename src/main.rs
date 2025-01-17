use std::io;

mod lexer;
mod repl;

fn main() {
    repl::start(&mut io::stdin().lock(), &mut io::stdout());
}
