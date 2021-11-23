use lexgen_rust::{Lexer, Token};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file = &args[1];
    let contents = std::fs::read_to_string(file).unwrap();
    let mut contents = contents.as_str();

    // Skip byte order mark
    if contents.len() > 3 && contents.as_bytes()[..3] == [0xEF, 0xBB, 0xBF] {
        contents = &contents[3..];
    }

    let lexer = Lexer::new(contents);
    let tokens: Vec<Token> = lexer.map(|t| t.unwrap().1).collect();
    println!("{:?}", tokens);
}
