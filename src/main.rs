fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file = &args[1];
    let contents = std::fs::read_to_string(file).unwrap();
    let lexer = Lexer::new(&contents);
    let tokens: Vec<Token> = lexer.map(|t| t.unwrap().1).collect();
    println!("{:?}", tokens);
}

use lexgen::lexer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'input> {
    /// A keyword
    Kw(Kw),

    /// A weak keyword:
    /// https://doc.rust-lang.org/reference/keywords.html#weak-keywords
    WeakKw(WeakKw),

    /// A reserved keyword:
    /// https://doc.rust-lang.org/reference/keywords.html#reserved-keywords
    ReservedKw(ReservedKw),

    /// An identifier:
    /// https://doc.rust-lang.org/reference/identifiers.html
    Id(&'input str),

    /// A lifetime or label:
    /// https://doc.rust-lang.org/reference/tokens.html#lifetimes-and-loop-labels
    LifetimeOrLabel(&'input str),

    /// A punctuation:
    /// https://doc.rust-lang.org/reference/tokens.html#punctuation
    Punc(Punc),

    /// A delimiter:
    /// https://doc.rust-lang.org/reference/tokens.html#delimiters
    Delim(Delim),

    /// A single-line or multi-line, doc or non-doc comment
    Comment(&'input str),

    /// A literal
    Lit(Lit<'input>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kw {
    As,
    Async,
    Await,
    Break,
    Continue,
    Crate,
    Dyn,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Pub,
    Ref,
    Return,
    SelfType,
    SelfValue,
    Static,
    Struct,
    Super,
    Trait,
    True,
    Type,
    Unsafe,
    Use,
    Where,
    While,
}

/// https://doc.rust-lang.org/reference/keywords.html#weak-keywords
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WeakKw {
    MacroRules,
    Union,
    StaticLifetime,
}

/// https://doc.rust-lang.org/reference/keywords.html#reserved-keywords
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReservedKw {
    Abstract,
    Become,
    Box,
    Do,
    Final,
    Macro,
    Override,
    Priv,
    Try,
    Typeof,
    Unsized,
    Virtual,
    Yield,
}

/// https://doc.rust-lang.org/reference/tokens.html#punctuation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punc {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Not,
    And,
    Or,
    AndAnd,
    OrOr,
    Shl,
    Shr,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    CaretEq,
    AndEq,
    OrEq,
    ShlEq,
    ShrEq,
    Eq,
    EqEq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    At,
    Underscore,
    Dot,
    DotDot,
    DotDotDot,
    DotDotEq,
    Comma,
    Semi,
    Colon,
    PathSep,
    RArrow,
    FatArrow,
    Pound,
    Dollar,
    Question,
}

/// https://doc.rust-lang.org/reference/tokens.html#delimiters
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lit<'input> {
    Char(&'input str),
    String(&'input str),
    Int(&'input str),
    RawString(&'input str),
}

#[derive(Debug, Default)]
struct LexerState {
    /// Number of `#` read for a raw literal
    raw_delimiter_size: usize,

    /// Number of closing `#`s seen so far. Used when terminating raw strings.
    closing_delimiters_seen: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CustomError {
    InvalidIntSuffix,
    IntWithoutDigit,
    InvalidDigitForBase,
}

// - Check that digits match the prefix (e.g. if the match starts with "0b", digits must be 1 or 0)
// - Check that that is at least one digit
// - Check that the suffix is valid ("i32", "u64", etc.)
fn check_int<'input>(match_: &'input str) -> Result<Token, CustomError> {
    if match_.starts_with("0b") {
        check_int_base(match_, &match_[2..], |char| char.is_digit(2))
    } else if match_.starts_with("0o") {
        check_int_base(match_, &match_[2..], |char| char.is_digit(8))
    } else if match_.starts_with("0x") {
        check_int_base(match_, &match_[2..], |char| char.is_digit(16))
    } else {
        check_int_base(match_, match_, |char| char.is_digit(10))
    }
}

fn check_int_base<'input, F: Fn(char) -> bool>(
    match_full: &'input str,
    match_no_prefix: &'input str,
    is_digit: F,
) -> Result<Token<'input>, CustomError> {
    let mut chars = match_no_prefix.char_indices();
    let mut digit_seen = false;

    let mut suffix = "";

    while let Some((char_idx, char)) = chars.next() {
        if (char >= '0' && char <= '9')
            || (char >= 'a' && char <= 'f')
            || (char >= 'A' && char <= 'F')
        {
            if is_digit(char) {
                digit_seen = true;
            } else {
                return Err(CustomError::InvalidDigitForBase);
            }
        } else if char == '_' {
            continue;
        } else {
            // Suffix starts
            suffix = &match_no_prefix[char_idx..];
            break;
        }
    }

    if !digit_seen {
        return Err(CustomError::IntWithoutDigit);
    }

    match suffix {
        "" | "u8" | "i8" | "u16" | "i16" | "u32" | "i32" | "u64" | "i64" | "u128" | "i128"
        | "usize" | "isize" => Ok(Token::Lit(Lit::Int(match_full))),
        _ => return Err(CustomError::InvalidIntSuffix),
    }
}

lexer! {
    Lexer(LexerState) -> Token<'input>;

    type Error = CustomError;

    // https://doc.rust-lang.org/reference/whitespace.html
    let whitespace =
        ['\t' '\n' '\u{B}' '\u{C}' '\r' ' ' '\u{85}' '\u{200E}' '\u{200F}' '\u{2028}' '\u{2029}'];

    let oct_digit = ['0'-'7'];
    let dec_digit = ['0'-'9'];
    let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F'];
    let bin_digit = '0' | '1';
    let digit = $oct_digit | $dec_digit | $hex_digit | $bin_digit;
    let int_suffix = ('u' | 'i') '8' | ('u' | 'i') "16" | ('u' | 'i') "32" |
            ('u' | 'i') "64" | ('u' | 'i') "128" | ('u' | 'i') "size";

    let id = $$XID_Start $$XID_Continue*;

    rule Init {
        "as" = Token::Kw(Kw::As),
        "async" = Token::Kw(Kw::Async),
        "await" = Token::Kw(Kw::Await),
        "break" = Token::Kw(Kw::Break),
        "continue" = Token::Kw(Kw::Continue),
        "crate" = Token::Kw(Kw::Crate),
        "dyn" = Token::Kw(Kw::Dyn),
        "else" = Token::Kw(Kw::Else),
        "enum" = Token::Kw(Kw::Enum),
        "extern" = Token::Kw(Kw::Extern),
        "false" = Token::Kw(Kw::False),
        "fn" = Token::Kw(Kw::Fn),
        "for" = Token::Kw(Kw::For),
        "impl" = Token::Kw(Kw::Impl),
        "in" = Token::Kw(Kw::In),
        "let" = Token::Kw(Kw::Let),
        "loop" = Token::Kw(Kw::Loop),
        "match" = Token::Kw(Kw::Match),
        "mod" = Token::Kw(Kw::Mod),
        "pub" = Token::Kw(Kw::Pub),
        "ref" = Token::Kw(Kw::Ref),
        "return" = Token::Kw(Kw::Return),
        "Self" = Token::Kw(Kw::SelfType),
        "self" = Token::Kw(Kw::SelfValue),
        "static" = Token::Kw(Kw::Static),
        "struct" = Token::Kw(Kw::Struct),
        "super" = Token::Kw(Kw::Super),
        "trait" = Token::Kw(Kw::Trait),
        "true" = Token::Kw(Kw::True),
        "type" = Token::Kw(Kw::Type),
        "unsafe" = Token::Kw(Kw::Unsafe),
        "use" = Token::Kw(Kw::Use),
        "where" = Token::Kw(Kw::Where),
        "while" = Token::Kw(Kw::While),

        "macro_rules" = Token::WeakKw(WeakKw::MacroRules),
        "union" = Token::WeakKw(WeakKw::Union),
        "'static" = Token::WeakKw(WeakKw::StaticLifetime),

        "abstract" = Token::ReservedKw(ReservedKw::Abstract),
        "become" = Token::ReservedKw(ReservedKw::Become),
        "box" = Token::ReservedKw(ReservedKw::Box),
        "do" = Token::ReservedKw(ReservedKw::Do),
        "final" = Token::ReservedKw(ReservedKw::Final),
        "macro" = Token::ReservedKw(ReservedKw::Macro),
        "override" = Token::ReservedKw(ReservedKw::Override),
        "priv" = Token::ReservedKw(ReservedKw::Priv),
        "try" = Token::ReservedKw(ReservedKw::Try),
        "typeof" = Token::ReservedKw(ReservedKw::Typeof),
        "unsized" = Token::ReservedKw(ReservedKw::Unsized),
        "virtual" = Token::ReservedKw(ReservedKw::Virtual),
        "yield" = Token::ReservedKw(ReservedKw::Yield),

        $id => |lexer| {
            let id = lexer.match_();
            lexer.return_(Token::Id(id))
        },

        '_' $$XID_Continue+ => |lexer| {
            let id = lexer.match_();
            lexer.return_(Token::Id(id))
        },

        "'_" | "'" $id => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::LifetimeOrLabel(match_))
        },

        $whitespace,

        "+" = Token::Punc(Punc::Plus),
        "-" = Token::Punc(Punc::Minus),
        "*" = Token::Punc(Punc::Star),
        "/" = Token::Punc(Punc::Slash),
        "%" = Token::Punc(Punc::Percent),
        "^" = Token::Punc(Punc::Caret),
        "!" = Token::Punc(Punc::Not),
        "&" = Token::Punc(Punc::And),
        "|" = Token::Punc(Punc::Or),
        "&&" = Token::Punc(Punc::AndAnd),
        "||" = Token::Punc(Punc::OrOr),
        "<<" = Token::Punc(Punc::Shl),
        ">>" = Token::Punc(Punc::Shr),
        "+=" = Token::Punc(Punc::PlusEq),
        "-=" = Token::Punc(Punc::MinusEq),
        "*=" = Token::Punc(Punc::StarEq),
        "/=" = Token::Punc(Punc::SlashEq),
        "%=" = Token::Punc(Punc::PercentEq),
        "^=" = Token::Punc(Punc::CaretEq),
        "&=" = Token::Punc(Punc::AndEq),
        "|=" = Token::Punc(Punc::OrEq),
        "<<=" = Token::Punc(Punc::ShlEq),
        ">>=" = Token::Punc(Punc::ShrEq),
        "=" = Token::Punc(Punc::Eq),
        "==" = Token::Punc(Punc::EqEq),
        "!=" = Token::Punc(Punc::Ne),
        ">" = Token::Punc(Punc::Gt),
        "<" = Token::Punc(Punc::Lt),
        ">=" = Token::Punc(Punc::Ge),
        "<=" = Token::Punc(Punc::Le),
        "@" = Token::Punc(Punc::At),
        "_" = Token::Punc(Punc::Underscore),
        "." = Token::Punc(Punc::Dot),
        ".." = Token::Punc(Punc::DotDot),
        "..." = Token::Punc(Punc::DotDotDot),
        "..=" = Token::Punc(Punc::DotDotEq),
        "," = Token::Punc(Punc::Comma),
        ";" = Token::Punc(Punc::Semi),
        ":" = Token::Punc(Punc::Colon),
        "::" = Token::Punc(Punc::PathSep),
        "->" = Token::Punc(Punc::RArrow),
        "=>" = Token::Punc(Punc::FatArrow),
        "#" = Token::Punc(Punc::Pound),
        "$" = Token::Punc(Punc::Dollar),
        "?" = Token::Punc(Punc::Question),

        "(" = Token::Delim(Delim::LParen),
        ")" = Token::Delim(Delim::RParen),
        "[" = Token::Delim(Delim::LBracket),
        "]" = Token::Delim(Delim::RBracket),
        "{" = Token::Delim(Delim::LBrace),
        "}" = Token::Delim(Delim::RBrace),

        "/*" => |lexer| lexer.switch(LexerRule::MultilineComment),
        "//" => |lexer| lexer.switch(LexerRule::SinglelineComment),

        //
        // Character literals
        //

        "'" _ "'" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Char(match_)))
        },

        // NB: Escaped double quote is valid!
        "'\\" ('n' | 'r' | 't' | '\\' | '0' | '\'' | '"') "'" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Char(match_)))
        },

        "'\\x" $oct_digit $hex_digit "'" => |lexer| {
            // TODO: Check that the number is in range
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Char(match_)))
        },

        "'\\u{" $hex_digit+ "}'" => |lexer| {
            // TODO: Check that there's at most 6 digits
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Char(match_)))
        },

        //
        // End of character literals
        //

        '"' => |lexer| lexer.switch(LexerRule::String),

        "r#" => |lexer| {
            lexer.state().raw_delimiter_size = 1;
            lexer.state().closing_delimiters_seen = 0;
            lexer.switch(LexerRule::RawLitStart)
        },

        "r\"" => |lexer| {
            lexer.state().raw_delimiter_size = 0;
            lexer.state().closing_delimiters_seen = 0;
            lexer.switch(LexerRule::RawString)
        },

        //
        // Integer literals
        //

        ("0b" | "0o" | "0x")? ($digit | '_')* $id? =? |lexer| {
            let match_ = lexer.match_();
            lexer.return_(check_int(match_))
        },

        //
        // End of integer literals
        //
    }

    rule SinglelineComment {
        $ | '\n' => |lexer| {
            let comment = lexer.match_();
            lexer.switch_and_return(LexerRule::Init, Token::Comment(comment))
        },

        _,
    }

    rule MultilineComment {
        "*/" => |lexer| {
            let comment = lexer.match_();
            lexer.switch_and_return(LexerRule::Init, Token::Comment(comment))
        },

        _,
    }

    rule String {
        '"' => |lexer| {
            let match_ = lexer.match_();
            lexer.switch_and_return(LexerRule::Init, Token::Lit(Lit::String(match_)))
        },

        '\\' => |lexer| lexer.switch(LexerRule::StringEscape),

        '\r' => |lexer| lexer.switch(LexerRule::StringCR),

        _,
    }

    rule StringEscape {
        '\\' => |lexer| lexer.switch(LexerRule::String),

        // Quote escape
        '\'' => |lexer| lexer.switch(LexerRule::String),
        '\"' => |lexer| lexer.switch(LexerRule::String),

        // ASCII escape
        'x' $oct_digit $hex_digit => |lexer| lexer.switch(LexerRule::String),
        'n' => |lexer| lexer.switch(LexerRule::String),
        'r' => |lexer| lexer.switch(LexerRule::String),
        't' => |lexer| lexer.switch(LexerRule::String),
        '\\' => |lexer| lexer.switch(LexerRule::String),
        '0' => |lexer| lexer.switch(LexerRule::String),

        // Unicode escape
        "u{" $hex_digit+ '}' => |lexer| lexer.switch(LexerRule::String),

        // String continue
        '\n' => |lexer| lexer.switch(LexerRule::String),
    }

    rule StringCR {
        '\n' => |lexer| lexer.switch(LexerRule::String),
    }

    // NB. `r#` already consumed
    rule RawLitStart {
        '#' => |lexer| {
            lexer.state().raw_delimiter_size += 1;
            lexer.continue_()
        },

        '"' => |lexer| lexer.switch(LexerRule::RawString),

        // TODO: Check that we saw only one `#`
        $$XID_Start $$XID_Continue* => |lexer| {
            let match_ = lexer.match_();
            lexer.switch_and_return(LexerRule::Init, Token::Id(match_))
        },
    }

    rule RawString {
        '"' => |lexer| {
            if lexer.state().raw_delimiter_size == 0 {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, Token::Lit(Lit::RawString(match_)))
            } else {
                lexer.state().closing_delimiters_seen = 0;
                lexer.switch(LexerRule::RawStringMatchClosingHashes)
            }
        },

        _,
    }

    rule RawStringMatchClosingHashes {
        '#' => |lexer| {
            let mut state = lexer.state();
            state.closing_delimiters_seen += 1;
            if state.closing_delimiters_seen == state.raw_delimiter_size {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, Token::Lit(Lit::RawString(match_)))
            } else {
                lexer.continue_()
            }
        },

        '"' => |lexer| {
            let mut state = lexer.state();
            state.closing_delimiters_seen = 0;
            lexer.continue_()
        },

        _ => |lexer| {
            lexer.state().closing_delimiters_seen = 0;
            lexer.switch(LexerRule::RawString)
        },
    }

    rule RawId {
        // TODO
    }
}

#[cfg(test)]
use lexgen_util::{LexerError, Loc};

#[cfg(test)]
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
