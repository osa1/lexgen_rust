use lexgen::lexer;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'input> {
    Whitespace,

    /// A keyword
    Kw(Kw),

    /// A weak keyword:
    /// <https://doc.rust-lang.org/reference/keywords.html#weak-keywords>
    WeakKw(WeakKw),

    /// A reserved keyword:
    /// <https://doc.rust-lang.org/reference/keywords.html#reserved-keywords>
    ReservedKw(ReservedKw),

    /// An identifier:
    /// <https://doc.rust-lang.org/reference/identifiers.html>
    Id(&'input str),

    /// A lifetime or label:
    /// <https://doc.rust-lang.org/reference/tokens.html#lifetimes-and-loop-labels>
    LifetimeOrLabel(&'input str),

    /// A punctuation:
    /// <https://doc.rust-lang.org/reference/tokens.html#punctuation>
    Punc(Punc),

    /// A delimiter:
    /// <https://doc.rust-lang.org/reference/tokens.html#delimiters>
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

/// <https://doc.rust-lang.org/reference/keywords.html#weak-keywords>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WeakKw {
    MacroRules,
    Union,
    StaticLifetime,
}

/// <https://doc.rust-lang.org/reference/keywords.html#reserved-keywords>
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

/// <https://doc.rust-lang.org/reference/tokens.html#punctuation>
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

    // Not in the spec yet, but used in const_deref feature:
    // https://github.com/rust-lang/rust/issues/88955
    Tilde,
}

/// <https://doc.rust-lang.org/reference/tokens.html#delimiters>
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
    Byte(&'input str),
    RawByteString(&'input str),
    Float(&'input str),
    ByteString(&'input str),
}

#[derive(Debug, Default)]
pub struct LexerState {
    /// Number of `#` read for a raw literal
    raw_delimiter_size: usize,

    /// Number of closing `#`s seen so far. Used when terminating raw strings.
    closing_delimiters_seen: usize,

    /// Level (nesting) of the current block comment. `0` when not lexing a block comment,
    /// `1` or more when lexing a block comment.
    block_comment_level: usize,
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
fn check_int(match_: &str) -> Result<Token, CustomError> {
    if let Some(rest) = match_.strip_prefix("0b") {
        check_int_base(match_, rest, |char| char.is_digit(2))
    } else if let Some(rest) = match_.strip_prefix("0o") {
        check_int_base(match_, rest, |char| char.is_digit(8))
    } else if let Some(rest) = match_.strip_prefix("0x") {
        check_int_base(match_, rest, |char| char.is_ascii_hexdigit())
    } else {
        check_int_base(match_, match_, |char| char.is_ascii_digit())
    }
}

fn check_int_base<'input, F: Fn(char) -> bool>(
    match_full: &'input str,
    match_no_prefix: &'input str,
    is_digit: F,
) -> Result<Token<'input>, CustomError> {
    let chars = match_no_prefix.char_indices();
    let mut digit_seen = false;

    let mut suffix = "";

    for (char_idx, char) in chars {
        #[allow(clippy::manual_range_contains)]
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
        _ => Err(CustomError::InvalidIntSuffix),
    }
}

lexer! {
    pub Lexer(LexerState) -> Token<'input>;

    type Error = CustomError;

    // https://doc.rust-lang.org/reference/whitespace.html
    let whitespace =
        ['\t' '\n' '\u{B}' '\u{C}' '\r' ' ' '\u{85}' '\u{200E}' '\u{200F}' '\u{2028}' '\u{2029}'];

    let oct_digit = ['0'-'7'];
    let dec_digit = ['0'-'9'];
    let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F'];
    let bin_digit = '0' | '1';
    let int_suffix = ('u' | 'i') '8' | ('u' | 'i') "16" | ('u' | 'i') "32" |
            ('u' | 'i') "64" | ('u' | 'i') "128" | ('u' | 'i') "size";

    let id = $$XID_Start $$XID_Continue*;

    let ascii_for_string = ($$ascii # ['"' '\\' '\r']);
    let byte_escape = ("\\x" $hex_digit $hex_digit) | "\\n" | "\\r" | "\\t" | "\\\\" | "\\0" | "\\\"" | "\\'";
    let string_continue = "\\\n";

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

        $whitespace+ = Token::Whitespace,

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
        "~" = Token::Punc(Punc::Tilde),

        "(" = Token::Delim(Delim::LParen),
        ")" = Token::Delim(Delim::RParen),
        "[" = Token::Delim(Delim::LBracket),
        "]" = Token::Delim(Delim::RBracket),
        "{" = Token::Delim(Delim::LBrace),
        "}" = Token::Delim(Delim::RBrace),

        "/*" => |lexer| {
            lexer.state().block_comment_level = 1;
            lexer.switch(LexerRule::MultilineComment)
        },
        "//" => |lexer| lexer.switch(LexerRule::SinglelineComment),

        //
        // Character literals
        //

        // TODO: This allows newline, tab etc. in single quotes
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

        //
        // Byte literals
        //

        // TODO: Exclude newline, tab, ... without '\\'
        "b'" ($$ascii | $byte_escape) "'" => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Byte(match_)))
        },

        "b\"" ($ascii_for_string | $byte_escape | $string_continue | "\r\n")* '"' => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::ByteString(match_)))
        },

        //
        // End of byte literals
        //

        //
        // String literals
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

        "br#" => |lexer| {
            lexer.state().raw_delimiter_size = 1;
            lexer.state().closing_delimiters_seen = 0;
            lexer.switch(LexerRule::RawByteStringLitStart)
        },

        "br\"" => |lexer| {
            lexer.state().raw_delimiter_size = 0;
            lexer.state().closing_delimiters_seen = 0;
            lexer.switch(LexerRule::RawByteStringLit)
        },

        //
        // End of string literals
        //

        //
        // Float literals
        //
        // Float literals need to be defined before int literals to lex a literal like `0f64` as
        // float rather than integer with a suffix.
        //

        let dec_literal = $dec_digit ($dec_digit | '_')*;
        let float_exponent = ('e' | 'E') ('+' | '-')? ($dec_digit | '_')* $dec_digit ($dec_digit | '_')*;
        let float_suffix = "f32" | "f64";

        $dec_literal '.' > ((_ # '.') | $) => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Float(match_)))
        },

        $dec_literal $float_exponent => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Float(match_)))
        },

        $dec_literal '.' $dec_literal $float_exponent? => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Float(match_)))
        },

        $dec_literal ('.' $dec_literal)? $float_exponent? $float_suffix => |lexer| {
            let match_ = lexer.match_();
            lexer.return_(Token::Lit(Lit::Float(match_)))
        },

        //
        // End of float literals
        //

        //
        // Integer literals
        //

        let digit = $oct_digit | $dec_digit | $hex_digit | $bin_digit;

        ("0b" | "0o" | "0x")? ($digit | '_')* $id? =? |lexer| {
            let match_ = lexer.match_();
            lexer.return_(check_int(match_))
        },

        //
        // End of integer literals
        //
    }

    // https://github.com/osa1/lexgen/issues/29#issuecomment-977550895
    rule SinglelineComment {
        (_ # '\n')* > ('\n' | $) => |lexer| {
            let comment = lexer.match_();
            lexer.switch_and_return(LexerRule::Init, Token::Comment(comment))
        },
    }

    rule MultilineComment {
        "/*" => |lexer| {
            lexer.state().block_comment_level += 1;
            lexer.continue_()
        },

        "*/" => |lexer| {
            lexer.state().block_comment_level -= 1;
            if lexer.state().block_comment_level == 0 {
                let comment = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, Token::Comment(comment))
            } else {
                lexer.continue_()
            }
        },

        _ => |lexer| lexer.continue_(),
    }

    rule String {
        '"' => |lexer| {
            let match_ = lexer.match_();
            lexer.switch_and_return(LexerRule::Init, Token::Lit(Lit::String(match_)))
        },

        '\\' => |lexer| lexer.switch(LexerRule::StringEscape),

        '\r' => |lexer| lexer.switch(LexerRule::StringCR),

        _ => |lexer| lexer.continue_(),
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

        _ => |lexer| lexer.continue_(),
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

    rule RawByteStringLitStart {
        '#' => |lexer| {
            lexer.state().raw_delimiter_size += 1;
            lexer.continue_()
        },

        '"' => |lexer| lexer.switch(LexerRule::RawByteStringLit),
    },

    rule RawByteStringLit {
        '"' => |lexer| {
            if lexer.state().raw_delimiter_size == 0 {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, Token::Lit(Lit::RawByteString(match_)))
            } else {
                lexer.state().closing_delimiters_seen = 0;
                lexer.switch(LexerRule::RawByteStringMatchClosingHashes)
            }
        },

        $$ascii => |lexer| lexer.continue_(),
    },

    rule RawByteStringMatchClosingHashes {
        '#' => |lexer| {
            let mut state = lexer.state();
            state.closing_delimiters_seen += 1;
            if state.closing_delimiters_seen == state.raw_delimiter_size {
                let match_ = lexer.match_();
                lexer.switch_and_return(LexerRule::Init, Token::Lit(Lit::RawByteString(match_)))
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
            lexer.switch(LexerRule::RawByteStringLit)
        },
    },
}
