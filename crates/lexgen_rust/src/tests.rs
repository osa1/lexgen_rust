use crate::{CustomError, Delim, Lexer, Lit, Token};

use lexgen_util::LexerError;

fn ignore_pos<A, E, L>(ret: Option<Result<(L, A, L), E>>) -> Option<Result<A, E>> {
    ret.map(|res| res.map(|(_, a, _)| a))
}

#[test]
fn comment() {
    let input = "/*\n\n*/";
    let mut lexer = Lexer::new(input);
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Comment(input))));
    assert_eq!(ignore_pos(lexer.next()), None);

    // Terminated at the end of input (no newline)
    let input = "//  /  ";
    let mut lexer = Lexer::new(input);
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Comment(input))));
    assert_eq!(ignore_pos(lexer.next()), None);

    // Terminated with newlines
    let input = "//  /  \n";
    let mut lexer = Lexer::new(input);
    assert_eq!(ignore_pos(lexer.next()), Some(Ok(Token::Comment(input))));
    assert_eq!(ignore_pos(lexer.next()), None);
}

#[test]
fn char_lit() {
    let input = "'a' '\\n' '\\r' '\\t' '\\\\' '\\0' '\\\'' '\\\"' '\\x11' '\\u{7FFF}'";
    let mut lexer = Lexer::new(input);
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'a'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\n'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\r'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\t'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\\\'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\0'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\\''"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\\"'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\x11'"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Char("'\\u{7FFF}'"))))
    );
    assert_eq!(ignore_pos(lexer.next()), None);
}

#[test]
fn string_lit() {
    let input = "\"a\" \"a\nb\" \"a\r\nb\"";
    let mut lexer = Lexer::new(input);
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::String("\"a\""))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::String("\"a\nb\""))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::String("\"a\r\nb\""))))
    );
    assert_eq!(ignore_pos(lexer.next()), None);

    let input = "\"a\r\"";
    let mut lexer = Lexer::new(input);
    assert!(matches!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::InvalidToken { .. }))
    ));

    // '"' '\\' '\\' '"'
    let input = "\"\\\\\"";
    let mut lexer = Lexer::new(input);
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::String("\"\\\\\""))))
    );
    assert_eq!(ignore_pos(lexer.next()), None);
}

#[test]
fn int_lit_valid() {
    let input = "[1] 123 123i32 123u32 123_u32 0xff 0xff_u8 \
                 0o70 0o70i16 0b1111_1111_1001_0000 0b1111_1111_1001_0000i64 0b________1 0usize";
    let mut lexer = Lexer::new(input);
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Delim(Delim::LBracket))),
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("1"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Delim(Delim::RBracket))),
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("123"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("123i32"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("123u32"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("123_u32"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0xff"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0xff_u8"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0o70"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0o70i16"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0b1111_1111_1001_0000"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0b1111_1111_1001_0000i64"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0b________1"))))
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("0usize"))))
    );
    assert_eq!(ignore_pos(lexer.next()), None);
}

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
    let mut lexer = Lexer::new(input);

    lexer.next(); // skip comment

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::Custom(CustomError::InvalidIntSuffix))),
    );

    lexer.next(); // skip ';'
    lexer.next(); // skip comment

    // 123AFB43
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::Custom(CustomError::InvalidDigitForBase))),
    );

    lexer.next(); // skip ';'

    // 0b0102
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::Custom(CustomError::InvalidDigitForBase))),
    );

    lexer.next(); // skip ';'

    // 0o0581
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::Custom(CustomError::InvalidDigitForBase))),
    );

    lexer.next(); // skip ';'
    lexer.next(); // skip comment

    // 128_i8
    // TODO: This overflows, but is that a lexing error?
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("128_i8")))),
    );

    lexer.next(); // skip ';'

    // 256_u8
    // TODO: Overflow here
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Int("256_u8")))),
    );

    lexer.next(); // skip ';'
    lexer.next(); // skip comment

    // 0b_
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::Custom(CustomError::IntWithoutDigit))),
    );

    lexer.next(); // skip ';'

    // 0b____
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Err(LexerError::Custom(CustomError::IntWithoutDigit))),
    );

    lexer.next(); // skip ';'
    assert_eq!(ignore_pos(lexer.next()), None);
}

#[test]
fn raw_string_lit() {
    let input = r#####"
        r"foo" r#""foo""# r##"foo #"# bar"## r"\x52"
    "#####;

    let mut lexer = Lexer::new(input);

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawString(r#####"r"foo""#####))))
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawString(r#####"r#""foo""#"#####))))
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawString(
            r#####"r##"foo #"# bar"##"#####
        ))))
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawString(r#####"r"\x52""#####))))
    );

    assert_eq!(ignore_pos(lexer.next()), None,);
}

// TODO: Enable this test
// #[test]
// fn char_fail() {
//     let input = "'\n'";
//     let mut lexer = Lexer::new(input);
//     assert_eq!(
//         ignore_pos(lexer.next()),
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
    let mut lexer = Lexer::new(input);
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Byte("b'a'")))),
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Byte("b'\\t'")))),
    );
    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::Byte("b'\\xAA'")))),
    );
    assert_eq!(ignore_pos(lexer.next()), None,);
}

#[test]
fn raw_byte_string_lit() {
    let input = r#####" br"foo" br##"foo #"# bar"##   br"\x52" "#####;
    let mut lexer = Lexer::new(input);

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawByteString(r#"br"foo""#)))),
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawByteString(
            r###"br##"foo #"# bar"##"###
        )))),
    );

    assert_eq!(
        ignore_pos(lexer.next()),
        Some(Ok(Token::Lit(Lit::RawByteString(r###"br"\x52""###)))),
    );
}
