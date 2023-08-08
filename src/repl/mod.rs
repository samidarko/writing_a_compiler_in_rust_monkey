use crate::lexer::Lexer;

use crate::token::Token::EoF;
use std::io::{self, Write};

pub fn repl() {
    let mut input = String::new();

    let mut stdout = io::stdout().lock();

    loop {
        write!(stdout, ">> ").expect("TODO: panic message");
        stdout.flush().expect("TODO: panic message");

        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                let mut lexer = Lexer::new(input.chars().collect());
                let mut tok = lexer.next_token().expect("ouch"); // TODO repl to return Result?

                while tok != EoF {
                    println!("{:?}", tok); // TODO maybe use write!
                    tok = lexer.next_token().expect("aye"); // TODO repl to return Result?
                }

                input.clear();
            }
            Err(error) => println!("error: {error}"),
        }
    }
}
