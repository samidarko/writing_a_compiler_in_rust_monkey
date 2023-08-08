mod lexer;
mod repl;
mod token;

fn main() {
    println!("Hello samidarko! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::repl();
}
