use lexgen_rust::Token;
use rustc_ap_rustc_lexer as rustc_lexer;

fn main() {
    let mut files: Vec<String> = std::env::args().collect();
    files.remove(0);
}

fn lexgen_token_to_rustc_token(token: &Token) -> rustc_lexer::TokenKind {
    match token {
        Token::Kw(_) => todo!(),
        Token::WeakKw(_) => todo!(),
        Token::ReservedKw(_) => todo!(),
        Token::Id(_) => todo!(),
        Token::LifetimeOrLabel(_) => todo!(),
        Token::Punc(_) => todo!(),
        Token::Delim(_) => todo!(),
        Token::Comment(_) => todo!(),
        Token::Lit(_) => todo!(),
    }
}
