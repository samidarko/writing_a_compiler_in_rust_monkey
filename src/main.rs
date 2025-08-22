use monkey_interpreter_rs::repl;

fn main() -> Result<(), String> {
    println!("Hello samidarko! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    let exit_code = repl::repl()?;
    if exit_code != 0 {
        std::process::exit(exit_code);
    }
    Ok(())
}
