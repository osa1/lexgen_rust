use crate::{CustomError, Delim, Lexer, Lit, Punc, Token};

use lexgen_util::{LexerError, LexerErrorKind};

fn ignore_pos<A, E, L>(ret: Option<Result<(L, A, L), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

// Skips whitespace
fn next<I: Iterator<Item = char> + Clone>(
    lexer: &mut Lexer<I>,
) -> Option<Result<Token, LexerError<CustomError>>> {
    match ignore_pos(lexer.next()) {
        None => None,
        Some(Err(err)) => Some(Err(err)),
        Some(Ok(Token::Whitespace)) => next(lexer),
        Some(Ok(other)) => Some(Ok(other)),
    }
}

#[test]
fn comment() {
    let input = "/*\n\n*/";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);

    // Terminated at the end of input (no newline)
    let input = "//  /  ";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);

    // Terminated with newlines
    let input = "//  /  \n";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);

    // Empty comment, terminated with eof
    let input = "//";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);

    // Empty comment, terminated with eol
    let input = "//\n";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn documentation() {
    let input = "/// asdf";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Documentation)));
    assert_eq!(next(&mut lexer), None);

    let input = "//! asdf";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Documentation)));
    assert_eq!(next(&mut lexer), None);

    let input = "/** asdf */";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Documentation)));
    assert_eq!(next(&mut lexer), None);

    let input = "/*! asdf */";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Documentation)));
    assert_eq!(next(&mut lexer), None);

    // Empty comment, terminated with eof
    let input = "//";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);

    // Empty comment, terminated with eol
    let input = "//\n";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Comment)));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn char_lit() {
    let input = "'a' '\\n' '\\r' '\\t' '\\\\' '\\0' '\\\'' '\\\"' '\\x11' '\\u{7FFF}'";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Char))));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn string_lit() {
    let input = "\"a\" \"a\nb\" \"a\r\nb\"";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::String))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::String))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::String))));
    assert_eq!(next(&mut lexer), None);

    let input = "\"a\r\"";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::InvalidToken,
            ..
        }))
    ));

    // '"' '\\' '\\' '"'
    let input = "\"\\\\\"";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::String))));
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn int_lit_valid() {
    let input = "[1] 123 123i32 123u32 123_u32 0xff 0xff_u8 \
                 0o70 0o70i16 0b1111_1111_1001_0000 0b1111_1111_1001_0000i64 0b________1 0usize";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Delim(Delim::LBracket))),);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Delim(Delim::RBracket))),);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))));
    assert_eq!(next(&mut lexer), None);
}

/*
#[test]
fn int_lit_invalid() {
    let input = "
        // invalid suffixes

        0invalidSuffix;

        // uses numbers of the wrong base

        123AFB43;
        0b0102;
        0o0581;

        // integers too big for their type (they overflow)

        128_i8;
        256_u8;

        // bin, hex, and octal literals must have at least one digit

        0b_;
        0b____;
    ";
    let mut lexer = Lexer::new_from_iter(input);

    next(&mut lexer); // skip comment

    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(CustomError::InvalidIntSuffix),
            ..
        })),
    ));

    next(&mut lexer); // skip ';'
    next(&mut lexer); // skip comment

    // 123AFB43
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(CustomError::InvalidDigitForBase),
            ..
        })),
    ));

    lexer.next(); // skip ';'

    // 0b0102
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(CustomError::InvalidDigitForBase),
            ..
        })),
    ));

    lexer.next(); // skip ';'

    // 0o0581
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(CustomError::InvalidDigitForBase),
            ..
        })),
    ));

    next(&mut lexer); // skip ';'
    next(&mut lexer); // skip comment

    // 128_i8
    // TODO: This overflows, but is that a lexing error?
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int("128_i8")))),);

    lexer.next(); // skip ';'

    // 256_u8
    // TODO: Overflow here
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int("256_u8")))),);

    next(&mut lexer); // skip ';'
    next(&mut lexer); // skip comment

    // 0b_
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(CustomError::IntWithoutDigit),
            ..
        })),
    ));

    next(&mut lexer); // skip ';'

    // 0b____
    assert!(matches!(
        next(&mut lexer),
        Some(Err(LexerError {
            kind: LexerErrorKind::Custom(CustomError::IntWithoutDigit),
            ..
        })),
    ));

    next(&mut lexer); // skip ';'
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn raw_string_lit() {
    let input = r#####"
        r"foo" r#""foo""# r##"foo #"# bar"## r"\x52"
    "#####;

    let mut lexer = Lexer::new_from_iter(input);

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawString(r#####"r"foo""#####))))
    );

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawString(r#####"r#""foo""#"#####))))
    );

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawString(
            r#####"r##"foo #"# bar"##"#####
        ))))
    );

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawString(r#####"r"\x52""#####))))
    );

    assert_eq!(next(&mut lexer), None,);
}

// TODO: Enable this test
// #[test]
// fn char_fail() {
//     let input = "'\n'";
//     let mut lexer = Lexer::new_from_iter(input);
//     assert_eq!(
//         next(&mut lexer),
//         Some(Err(LexerError::InvalidToken {
//             location: Loc {
//                 line: 0,
//                 col: 0,
//                 byte_idx: 0
//             }
//         }))
//     );
// }

#[test]
fn byte_lit() {
    let input = r#"b'a' b'\t' b'\xAA'"#;
    let mut lexer = Lexer::new_from_iter(input);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Byte("b'a'")))),);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Byte("b'\\t'")))),);
    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::Byte("b'\\xAA'")))),
    );
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn raw_byte_string_lit() {
    let input = r#####" br"foo" br##"foo #"# bar"##   br"\x52" "#####;
    let mut lexer = Lexer::new_from_iter(input);

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawByteString(r#"br"foo""#)))),
    );

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawByteString(
            r###"br##"foo #"# bar"##"###
        )))),
    );

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::RawByteString(r###"br"\x52""###)))),
    );
}

#[test]
fn float() {
    let input = "1.17549435e-38 2.0 0f64";
    let mut lexer = Lexer::new_from_iter(input);

    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::Float("1.17549435e-38")))),
    );
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Float("2.0")))),);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Float("0f64")))),);
    assert_eq!(next(&mut lexer), None);
}

#[test]
fn byte_string() {
    let input = r#####"b"abc""#####;
    let mut lexer = Lexer::new_from_iter(input);
    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::ByteString(input)))),
    );
    assert_eq!(next(&mut lexer), None);

    let input = r#####"b"\x01""#####;
    let mut lexer = Lexer::new_from_iter(input);
    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::ByteString(input)))),
    );
    assert_eq!(next(&mut lexer), None);

    let input = r#####"b"\n""#####;
    let mut lexer = Lexer::new_from_iter(input);
    assert_eq!(
        next(&mut lexer),
        Some(Ok(Token::Lit(Lit::ByteString(input)))),
    );
    assert_eq!(next(&mut lexer), None);
}
*/

#[test]
fn int_float_range_confusion() {
    let input = "1. 1..";
    let mut lexer = Lexer::new_from_iter(input.chars());
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Float))),);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Lit(Lit::Int))),);
    assert_eq!(next(&mut lexer), Some(Ok(Token::Punc(Punc::DotDot))),);
}
