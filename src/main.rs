mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

fn main() -> Result<(), String> {
    println!("Hello samidarko! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::repl()
}
