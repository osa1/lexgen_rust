use lexgen_rust::{Lexer, Token};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file = &args[1];
    let contents = std::fs::read_to_string(file).unwrap();
    let lexer = Lexer::new(&contents);
    let tokens: Vec<Token> = lexer.map(|t| t.unwrap().1).collect();
    println!("{:?}", tokens);
}
