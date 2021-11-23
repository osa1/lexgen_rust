use lexgen_rust::{self, Delim, Punc, Token};
use rustc_ap_rustc_lexer as rustc_lexer;

fn main() {
    let mut files: Vec<String> = std::env::args().collect();
    files.remove(0);

    for file in files {
        let file_contents = std::fs::read_to_string(file).unwrap();

        let rustc_tokens: Vec<rustc_lexer::Token> = rustc_lexer::tokenize(&file_contents).collect();
        let mut rustc_tokens_iter = rustc_tokens.iter();

        for tok in lexgen_rust::Lexer::new(&file_contents) {
            let (start, tok, end) = tok.unwrap();
            let tok_rustc_toks = lexgen_token_to_rustc_token(std::slice::from_ref(&tok));
            for tok_rustc_tok in tok_rustc_toks {
                let rustc_token = rustc_tokens_iter.next().unwrap().kind;
                assert!(
                    token_eq(rustc_token, tok_rustc_tok),
                    "lexgen={:?}, rustc={:?}, {:?} - {:?}",
                    tok_rustc_tok,
                    rustc_token,
                    start,
                    end,
                );
            }
        }
    }
}

fn lexgen_token_to_rustc_token(lexgen_tokens: &[Token]) -> Vec<rustc_lexer::TokenKind> {
    let mut tokens: Vec<rustc_lexer::TokenKind> = Vec::with_capacity(lexgen_tokens.len() * 2);

    for lexgen_token in lexgen_tokens {
        match lexgen_token {
            Token::Whitespace => tokens.push(rustc_lexer::TokenKind::Whitespace),
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
                Punc::Tilde => {
                    tokens.push(rustc_lexer::TokenKind::Tilde);
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
                    lexgen_rust::Lit::Float(_) => rustc_lexer::LiteralKind::Float {
                        base: rustc_lexer::Base::Decimal, // TODO
                        empty_exponent: true,             // TODO
                    },
                    lexgen_rust::Lit::ByteString(_) => todo!(),
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

fn token_eq(tok1: rustc_lexer::TokenKind, tok2: rustc_lexer::TokenKind) -> bool {
    match (tok1, tok2) {
        (
            rustc_lexer::TokenKind::LineComment { .. },
            rustc_lexer::TokenKind::LineComment { .. },
        ) => true,
        (
            rustc_lexer::TokenKind::BlockComment {
                doc_style: _,
                terminated: _,
            },
            rustc_lexer::TokenKind::BlockComment {
                doc_style: _,
                terminated: _,
            },
        ) => true,
        (rustc_lexer::TokenKind::Whitespace, rustc_lexer::TokenKind::Whitespace) => true,
        (rustc_lexer::TokenKind::Ident, rustc_lexer::TokenKind::Ident) => true,
        (rustc_lexer::TokenKind::RawIdent, rustc_lexer::TokenKind::RawIdent) => true,
        (rustc_lexer::TokenKind::UnknownPrefix, rustc_lexer::TokenKind::UnknownPrefix) => true,
        (
            rustc_lexer::TokenKind::Literal {
                kind: kind1,
                suffix_start: _,
            },
            rustc_lexer::TokenKind::Literal {
                kind: kind2,
                suffix_start: _,
            },
        ) => match (kind1, kind2) {
            (rustc_lexer::LiteralKind::Int { .. }, rustc_lexer::LiteralKind::Int { .. }) => true,
            (rustc_lexer::LiteralKind::Float { .. }, rustc_lexer::LiteralKind::Float { .. }) => {
                true
            }
            (rustc_lexer::LiteralKind::Char { .. }, rustc_lexer::LiteralKind::Char { .. }) => true,
            (rustc_lexer::LiteralKind::Byte { .. }, rustc_lexer::LiteralKind::Byte { .. }) => true,
            (rustc_lexer::LiteralKind::Str { .. }, rustc_lexer::LiteralKind::Str { .. }) => true,
            (
                rustc_lexer::LiteralKind::ByteStr { .. },
                rustc_lexer::LiteralKind::ByteStr { .. },
            ) => true,
            (rustc_lexer::LiteralKind::RawStr { .. }, rustc_lexer::LiteralKind::RawStr { .. }) => {
                true
            }
            (
                rustc_lexer::LiteralKind::RawByteStr { .. },
                rustc_lexer::LiteralKind::RawByteStr { .. },
            ) => true,
            (_, _) => false,
        },
        (
            rustc_lexer::TokenKind::Lifetime {
                starts_with_number: _,
            },
            rustc_lexer::TokenKind::Lifetime {
                starts_with_number: _,
            },
        ) => true,
        (rustc_lexer::TokenKind::Semi, rustc_lexer::TokenKind::Semi) => true,
        (rustc_lexer::TokenKind::Comma, rustc_lexer::TokenKind::Comma) => true,
        (rustc_lexer::TokenKind::Dot, rustc_lexer::TokenKind::Dot) => true,
        (rustc_lexer::TokenKind::OpenParen, rustc_lexer::TokenKind::OpenParen) => true,
        (rustc_lexer::TokenKind::CloseParen, rustc_lexer::TokenKind::CloseParen) => true,
        (rustc_lexer::TokenKind::OpenBrace, rustc_lexer::TokenKind::OpenBrace) => true,
        (rustc_lexer::TokenKind::CloseBrace, rustc_lexer::TokenKind::CloseBrace) => true,
        (rustc_lexer::TokenKind::OpenBracket, rustc_lexer::TokenKind::OpenBracket) => true,
        (rustc_lexer::TokenKind::CloseBracket, rustc_lexer::TokenKind::CloseBracket) => true,
        (rustc_lexer::TokenKind::At, rustc_lexer::TokenKind::At) => true,
        (rustc_lexer::TokenKind::Pound, rustc_lexer::TokenKind::Pound) => true,
        (rustc_lexer::TokenKind::Tilde, rustc_lexer::TokenKind::Tilde) => true,
        (rustc_lexer::TokenKind::Question, rustc_lexer::TokenKind::Question) => true,
        (rustc_lexer::TokenKind::Colon, rustc_lexer::TokenKind::Colon) => true,
        (rustc_lexer::TokenKind::Dollar, rustc_lexer::TokenKind::Dollar) => true,
        (rustc_lexer::TokenKind::Eq, rustc_lexer::TokenKind::Eq) => true,
        (rustc_lexer::TokenKind::Bang, rustc_lexer::TokenKind::Bang) => true,
        (rustc_lexer::TokenKind::Lt, rustc_lexer::TokenKind::Lt) => true,
        (rustc_lexer::TokenKind::Gt, rustc_lexer::TokenKind::Gt) => true,
        (rustc_lexer::TokenKind::Minus, rustc_lexer::TokenKind::Minus) => true,
        (rustc_lexer::TokenKind::And, rustc_lexer::TokenKind::And) => true,
        (rustc_lexer::TokenKind::Or, rustc_lexer::TokenKind::Or) => true,
        (rustc_lexer::TokenKind::Plus, rustc_lexer::TokenKind::Plus) => true,
        (rustc_lexer::TokenKind::Star, rustc_lexer::TokenKind::Star) => true,
        (rustc_lexer::TokenKind::Slash, rustc_lexer::TokenKind::Slash) => true,
        (rustc_lexer::TokenKind::Caret, rustc_lexer::TokenKind::Caret) => true,
        (rustc_lexer::TokenKind::Percent, rustc_lexer::TokenKind::Percent) => true,
        (rustc_lexer::TokenKind::Unknown, rustc_lexer::TokenKind::Unknown) => true,
        (_, _) => false,
    }
}
