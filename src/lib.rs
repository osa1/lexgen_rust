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

    String(&'input str),

    /// A punctuation:
    /// https://doc.rust-lang.org/reference/tokens.html#punctuation
    Punc(Punc),

    /// A delimiter:
    /// https://doc.rust-lang.org/reference/tokens.html#delimiters
    Delim(Delim),

    /// A single-line or multi-line, doc or non-doc comment
    Comment(&'input str),
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

lexer! {
    Lexer -> Token<'input>;

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

        $$XID_Start $$XID_Continue* => |lexer| {
            let id = lexer.match_();
            lexer.return_(Token::Id(id))
        },

        '_' $$XID_Continue+ => |lexer| {
            let id = lexer.match_();
            lexer.return_(Token::Id(id))
        },

        // https://doc.rust-lang.org/reference/whitespace.html
        ['\t' '\n' '\u{B}' '\u{C}' '\r' ' ' '\u{85}'
            '\u{200E}' '\u{200F}' '\u{2028}' '\u{2029}'],

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
}

#[cfg(test)]
fn ignore_pos<A, E>(ret: Option<Result<(usize, A, usize), E>>) -> Option<Result<A, E>> {
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
