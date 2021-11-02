use lexgen_rust::{Delim, Punc, Token};
use rustc_ap_rustc_lexer as rustc_lexer;

fn main() {
    let mut files: Vec<String> = std::env::args().collect();
    files.remove(0);
}

fn _lexgen_token_to_rustc_token(lexgen_tokens: &[Token]) -> Vec<rustc_lexer::TokenKind> {
    let mut tokens: Vec<rustc_lexer::TokenKind> = Vec::with_capacity(lexgen_tokens.len() * 2);

    for lexgen_token in lexgen_tokens {
        match lexgen_token {
            Token::Kw(_) | Token::WeakKw(_) | Token::ReservedKw(_) | Token::Id(_) => {
                tokens.push(rustc_lexer::TokenKind::Ident);
            }
            Token::LifetimeOrLabel(str) => {
                let starts_with_number = str.chars().next().unwrap().is_numeric();
                tokens.push(rustc_lexer::TokenKind::Lifetime { starts_with_number })
            }
            Token::Punc(punc) => match punc {
                Punc::Plus => tokens.push(rustc_lexer::TokenKind::Plus),
                Punc::Minus => tokens.push(rustc_lexer::TokenKind::Minus),
                Punc::Star => tokens.push(rustc_lexer::TokenKind::Star),
                Punc::Slash => tokens.push(rustc_lexer::TokenKind::Slash),
                Punc::Percent => tokens.push(rustc_lexer::TokenKind::Percent),
                Punc::Caret => tokens.push(rustc_lexer::TokenKind::Caret),
                Punc::Not => tokens.push(rustc_lexer::TokenKind::Bang),
                Punc::And => tokens.push(rustc_lexer::TokenKind::And),
                Punc::Or => tokens.push(rustc_lexer::TokenKind::Or),
                Punc::AndAnd => {
                    tokens.push(rustc_lexer::TokenKind::And);
                    tokens.push(rustc_lexer::TokenKind::And);
                }
                Punc::OrOr => {
                    tokens.push(rustc_lexer::TokenKind::Or);
                    tokens.push(rustc_lexer::TokenKind::Or);
                }
                Punc::Shl => {
                    tokens.push(rustc_lexer::TokenKind::Lt);
                    tokens.push(rustc_lexer::TokenKind::Lt);
                }
                Punc::Shr => {
                    tokens.push(rustc_lexer::TokenKind::Gt);
                    tokens.push(rustc_lexer::TokenKind::Gt);
                }
                Punc::PlusEq => {
                    tokens.push(rustc_lexer::TokenKind::Plus);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::MinusEq => {
                    tokens.push(rustc_lexer::TokenKind::Minus);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::StarEq => {
                    tokens.push(rustc_lexer::TokenKind::Star);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::SlashEq => {
                    tokens.push(rustc_lexer::TokenKind::Slash);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::PercentEq => {
                    tokens.push(rustc_lexer::TokenKind::Percent);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::CaretEq => {
                    tokens.push(rustc_lexer::TokenKind::Caret);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::AndEq => {
                    tokens.push(rustc_lexer::TokenKind::And);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::OrEq => {
                    tokens.push(rustc_lexer::TokenKind::Or);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::ShlEq => {
                    tokens.push(rustc_lexer::TokenKind::Lt);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::ShrEq => {
                    tokens.push(rustc_lexer::TokenKind::Gt);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::Eq => {
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::EqEq => {
                    tokens.push(rustc_lexer::TokenKind::Eq);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::Ne => {
                    tokens.push(rustc_lexer::TokenKind::Bang);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::Gt => {
                    tokens.push(rustc_lexer::TokenKind::Gt);
                }
                Punc::Lt => {
                    tokens.push(rustc_lexer::TokenKind::Lt);
                }
                Punc::Ge => {
                    tokens.push(rustc_lexer::TokenKind::Gt);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::Le => {
                    tokens.push(rustc_lexer::TokenKind::Lt);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::At => {
                    tokens.push(rustc_lexer::TokenKind::At);
                }
                Punc::Underscore => {
                    tokens.push(rustc_lexer::TokenKind::Ident);
                }
                Punc::Dot => {
                    tokens.push(rustc_lexer::TokenKind::Dot);
                }
                Punc::DotDot => {
                    tokens.push(rustc_lexer::TokenKind::Dot);
                    tokens.push(rustc_lexer::TokenKind::Dot);
                }
                Punc::DotDotDot => {
                    tokens.push(rustc_lexer::TokenKind::Dot);
                    tokens.push(rustc_lexer::TokenKind::Dot);
                    tokens.push(rustc_lexer::TokenKind::Dot);
                }
                Punc::DotDotEq => {
                    tokens.push(rustc_lexer::TokenKind::Dot);
                    tokens.push(rustc_lexer::TokenKind::Dot);
                    tokens.push(rustc_lexer::TokenKind::Eq);
                }
                Punc::Comma => {
                    tokens.push(rustc_lexer::TokenKind::Comma);
                }
                Punc::Semi => {
                    tokens.push(rustc_lexer::TokenKind::Semi);
                }
                Punc::Colon => {
                    tokens.push(rustc_lexer::TokenKind::Colon);
                }
                Punc::PathSep => {
                    tokens.push(rustc_lexer::TokenKind::Colon);
                    tokens.push(rustc_lexer::TokenKind::Colon);
                }
                Punc::RArrow => {
                    tokens.push(rustc_lexer::TokenKind::Minus);
                    tokens.push(rustc_lexer::TokenKind::Gt);
                }
                Punc::FatArrow => {
                    tokens.push(rustc_lexer::TokenKind::Eq);
                    tokens.push(rustc_lexer::TokenKind::Gt);
                }
                Punc::Pound => {
                    tokens.push(rustc_lexer::TokenKind::Pound);
                }
                Punc::Dollar => {
                    tokens.push(rustc_lexer::TokenKind::Dollar);
                }
                Punc::Question => {
                    tokens.push(rustc_lexer::TokenKind::Question);
                }
            },
            Token::Delim(delim) => match delim {
                Delim::LBrace => {
                    tokens.push(rustc_lexer::TokenKind::OpenBrace);
                }
                Delim::RBrace => {
                    tokens.push(rustc_lexer::TokenKind::CloseBrace);
                }
                Delim::LBracket => {
                    tokens.push(rustc_lexer::TokenKind::OpenBracket);
                }
                Delim::RBracket => {
                    tokens.push(rustc_lexer::TokenKind::CloseBracket);
                }
                Delim::LParen => {
                    tokens.push(rustc_lexer::TokenKind::OpenParen);
                }
                Delim::RParen => {
                    tokens.push(rustc_lexer::TokenKind::CloseParen);
                }
            },
            Token::Comment(_) => {
                // TODO: doc style
                // TODO: confuses multi-line and single-line comments
                tokens.push(rustc_lexer::TokenKind::LineComment { doc_style: None });
            }
            Token::Lit(lit) => {
                let kind = match lit {
                    lexgen_rust::Lit::Char(_) => {
                        rustc_lexer::LiteralKind::Char { terminated: true }
                    }
                    lexgen_rust::Lit::String(_) => {
                        rustc_lexer::LiteralKind::Str { terminated: true }
                    }
                    lexgen_rust::Lit::Int(_) => rustc_lexer::LiteralKind::Int {
                        // TODO: base
                        base: rustc_lexer::Base::Decimal,
                        // TODO: empty_int
                        empty_int: false,
                    },
                    lexgen_rust::Lit::RawString(_) => rustc_lexer::LiteralKind::RawStr {
                        // TODO: n_hashes
                        n_hashes: 0,
                        err: None,
                    },
                    lexgen_rust::Lit::Byte(_) => {
                        rustc_lexer::LiteralKind::Byte { terminated: true }
                    }
                    lexgen_rust::Lit::RawByteString(_) => rustc_lexer::LiteralKind::RawByteStr {
                        // TODO: n_hashes
                        n_hashes: 0,
                        err: None,
                    },
                };

                // TODO: suffix_start
                tokens.push(rustc_lexer::TokenKind::Literal {
                    kind,
                    suffix_start: 0,
                });
            }
        }
    }

    tokens
}
